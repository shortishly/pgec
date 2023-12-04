%% Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(pgec_storage_common).


-export([bucket/1]).
-export([callback_mode/0]).
-export([handle_event/4]).
-export([key/2]).
-export([pt/1]).
-export([row/3]).
-export([value/2]).
-import(pgec_statem, [nei/1]).
-include("pgec_storage.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("leveled/include/leveled.hrl").


callback_mode() ->
    handle_event_function.


handle_event(internal,
             {put = Action,
              #{bucket := Bucket,
                key := Key,
                value := Value} = Detail},
             _,
             _) ->
    {keep_state_and_data,
     [nei({storage,
           #{request => {put,
                         Bucket,
                         term_to_binary(Key),
                         Value,
                         [],
                         ?STD_TAG,
                         infinity,
                         false},
             label => storage_label(Action, Detail)}}),
      nei({telemetry,
           Action,
           #{count => 1},
           #{bucket => Bucket}})]};

handle_event(internal,
             {delete = Action,
              #{bucket := Bucket,
                key := Key} = Detail},
             _,
             _) ->
    {keep_state_and_data,
     [nei({cache_delete, Detail}),
      nei({storage,
           #{request => {put,
                         Bucket,
                         term_to_binary(Key),
                         delete,
                         [],
                         ?STD_TAG,
                         infinity,
                         false},
             label => storage_label(Action, Detail)}}),
      nei({telemetry,
           Action,
           #{count => 1},
           #{bucket => Bucket}})]};

handle_event(internal,
             {keys = Action,
              #{bucket := Bucket,
                folder := Folder,
                accumulator := Accumulator} = Detail},
             _,
             _) ->
    {keep_state_and_data,
     [nei({storage,
           #{request => {return_runner,
                         {keylist,
                          ?STD_TAG,
                          Bucket,
                          {fun
                               (FoldBucket, Key, A) ->
                                   Folder(
                                     FoldBucket,
                                     binary_to_term(Key),
                                     A)
                           end,
                           Accumulator}}},
             label => storage_label(Action, Detail)}}),
      nei({telemetry,
           Action,
           #{count => 1},
           #{bucket => Bucket}})]};

handle_event(internal,
             {get = Action,
              #{bucket := Bucket,
                key := Key} = Detail},
             _,
             _) ->
    {keep_state_and_data,
     [nei({storage,
           #{request => {get, Bucket, term_to_binary(Key), ?STD_TAG},
             label => storage_label(Action, Detail)}}),
      nei({telemetry,
           Action,
           #{count => 1},
           #{bucket => Bucket}})]};

handle_event(internal,
             {storage, #{request := Request, label := Label}},
             _,
             #{requests := Requests, storage := Storage} = Data) ->
    {keep_state,
     Data#{requests := gen_server:send_request(
                         Storage,
                         Request,
                         Label,
                         Requests)}};

handle_event(info, {'EXIT', _, _}, _, _) ->
    stop;

handle_event(info, Msg, _, #{requests := Existing} = Data) ->
    case gen_server:check_response(Msg, Existing, true) of
        {{reply, Reply}, Label, Updated} ->
            {keep_state,
             Data#{requests := Updated},
             [nei({telemetry,
                   reqids_size,
                   #{value => gen_server:reqids_size(Updated)}}),
                  nei({response, #{label => Label, reply => Reply}})]};

        {{error, {Reason, _}}, _, UpdatedRequests} ->
            {stop, Reason, Data#{requests := UpdatedRequests}}
    end;

handle_event(internal,
             {cache_read,
              #{bucket := Bucket,
                from := From,
                key := Key}},
             _,
             #{cache := Cache}) ->
    case ets:lookup(Cache, {Bucket, Key}) of
        [] ->
            {keep_state_and_data,
             [nei({telemetry,
                   cache,
                   #{count => 1},
                   #{action => miss, bucket => Bucket}}),
              nei({get, #{from => From, bucket => Bucket, key => Key}})]};

        [#entry{value = Value}] ->
            {keep_state_and_data,
             [{reply, From, {ok, Value}},
              nei({telemetry,
                   cache,
                   #{count => 1},
                   #{action => hit, bucket => Bucket}}),
              {{timeout, {Bucket, Key}},
               pgec_config:timeout(expiry),
               expired}]}
    end;

handle_event(internal,
             {cache_write,
              #{bucket := Bucket,
                key := Key,
                value := Value}},
              _,
              #{cache := Cache}) ->
    case ets:update_element(
           Cache,
           {Bucket, Key},
           {#entry.value,  Value}) of

        true ->
            {keep_state_and_data,
             [nei({telemetry,
                   cache,
                   #{count => 1},
                   #{action => update,
                     bucket => Bucket}}),
              {{timeout, {Bucket, Key}},
               pgec_config:timeout(expiry),
               expired}]};

        false ->
            true = ets:insert_new(
                     Cache,
                     #entry{key = {Bucket, Key}, value = Value}),

            {keep_state_and_data,
             [nei({telemetry,
                   cache,
                   #{count => 1},
                   #{action => insert,
                     bucket => Bucket}}),
              {{timeout, {Bucket, Key}},
               pgec_config:timeout(expiry),
               expired}]}
    end;

handle_event(internal,
             {cache_delete,
              #{bucket := Bucket,
                key := Key}},
             _,
             #{cache := Cache}) ->
    ets:delete(Cache, {Bucket, Key}),
    {keep_state_and_data,
     [nei({telemetry,
           cache,
           #{count => 1},
           #{action => delete,
             bucket => Bucket}}),
      {{timeout, {Bucket, Key}}, infinity, cancelled}]};

handle_event({timeout, {Bucket, _} = Key}, expired, _, #{cache := Cache}) ->
    ets:delete(Cache, Key),
    {keep_state_and_data,
     nei({telemetry,
          cache,
          #{count => 1},
          #{action => expired,
            bucket => Bucket}})};

handle_event(
  internal,
  {response,
   #{reply := {ok, Value} = Reply,
     label := #{action := get = Action,
                bucket := Bucket,
                key := Key,
                from := From}}},
  _,
  _) ->
    {keep_state_and_data,
     [{reply, From, Reply},
      nei({telemetry,
           Action,
           #{count => 1},
           #{bucket => Bucket}}),
      nei({cache_write,
           #{bucket => Bucket,
             key => Key,
             value => Value}})]};

handle_event(internal,
             {response,
              #{reply := {async, F},
                label := #{from := From}} = Response},
             State,
             Data) ->
    ?LOG_DEBUG(#{response => Response,
                 state => State,
                 data => Data}),
    {keep_state_and_data, {reply, From, F()}};

handle_event(internal,
             {response,
              #{reply := Reply,
                label := #{from := From}} = Response},
             State,
             Data) ->
    ?LOG_DEBUG(#{response => Response,
                 state => State,
                 data => Data}),
    {keep_state_and_data, {reply, From, Reply}};

handle_event(internal,
             {response, Response},
             State,
             Data) ->
    ?LOG_DEBUG(#{response => Response,
                 state => State,
                 data => Data}),
    keep_state_and_data;

handle_event(internal,
             {telemetry, EventName, Measurements},
             _,
             _) ->
    {keep_state_and_data,
     nei({telemetry, EventName, Measurements, #{}})};

handle_event(internal,
             {telemetry, EventName, Measurements, Metadata},
             _,
             _) when is_atom(EventName) ->
    {keep_state_and_data,
     nei({telemetry, [EventName], Measurements, Metadata})};

handle_event(internal,
             {telemetry, EventName, Measurements, Metadata},
             _,
             Data) ->
    ok = telemetry:execute([pgec, storage] ++ EventName,
                           Measurements,
                           maps:merge(
                             maps:with([operator, client_flags], Data),
                             Metadata)),
    keep_state_and_data.


storage_label(Action, Detail) ->
    Detail#{action => Action}.


pt(#{publication := Publication, table := Table}) ->
    {Publication, Table}.


bucket(#{publication := Publication, table := Table}) ->
    <<"pgec/", Publication/bytes, "/", Table/bytes>>.


value(Tuple, #{keys := Keys}) ->
    pick(Tuple, lists:seq(1, tuple_size(Tuple)) -- Keys).

key(Tuple, #{keys := Keys}) ->
    pick(Tuple, Keys).


pick(Tuple, [Position]) ->
    element(Position, Tuple);

pick(Tuple, Positions) ->
    list_to_tuple(positions(Tuple, Positions)).


row(K, V, #{keys := [1]} = Metadata)
  when is_tuple(V) ->
    ?LOG_DEBUG(#{k => K, v => V, metadata => Metadata}),
    list_to_tuple([K | tuple_to_list(V)]);

row(K, V, #{keys := [1]} = Metadata) ->
    ?LOG_DEBUG(#{k => K, v => V, metadata => Metadata}),
    list_to_tuple([K, V]);

row(K, V, #{keys := [L]} = Metadata)
  when is_tuple(V),
       tuple_size(V) == L - 1 ->
    ?LOG_DEBUG(#{k => K, v => V, metadata => Metadata}),
    list_to_tuple(tuple_to_list(V) ++ [K]);

row(K, V, #{keys := [N]} = Metadata)
  when is_tuple(V),
       N =< tuple_size(V) ->
    ?LOG_DEBUG(#{k => K, v => V, metadata => Metadata}),
    list_to_tuple(
      positions(V, lists:seq(1, N - 1)) ++
          [K | positions(V, lists:seq(N, tuple_size(V)))]);

row(K, V, #{keys := [2]} = Metadata) ->
    ?LOG_DEBUG(#{k => K, v => V, metadata => Metadata}),
    list_to_tuple([V, K]);

row(K, V, #{keys := Keys} = Metadata)
  when is_tuple(K),
       is_tuple(V) ->
    ?LOG_DEBUG(#{k => K, v => V, metadata => Metadata}),
    list_to_tuple(
      integrate(
        tuple_to_list(K),
        tuple_to_list(V),
        Keys,
        1,
        []));

row(K, V, #{keys := Keys} = Metadata)
  when is_tuple(K) ->
    ?LOG_DEBUG(#{k => K, v => V, metadata => Metadata}),
    list_to_tuple(
      integrate(
        tuple_to_list(K),
        [V],
        Keys,
        1,
        [])).


integrate([], [], [], _, A) ->
    lists:reverse(A);

integrate([], Columns, _, _, A) ->
    lists:reverse(A) ++ Columns;

integrate(Keys,
          [Column | Columns],
          [Position | _] = Positions,
          N,
          A)
  when N < Position ->
    ?FUNCTION_NAME(Keys,
                   Columns,
                   Positions,
                   N + 1,
                   [Column | A]);

integrate([Key | Keys],
          Columns,
          [_ | Positions],
          N,
          A) ->
    ?FUNCTION_NAME(
       Keys,
       Columns,
       Positions,
       N + 1,
       [Key | A]).


positions(Tuple, Positions) ->
    [element(Position, Tuple) || Position <- Positions].
