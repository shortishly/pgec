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


-module(pgec_storage).


-export([callback_mode/0]).
-export([delete/1]).
-export([handle_event/4]).
-export([init/1]).
-export([keys/1]).
-export([metadata/1]).
-export([position/1]).
-export([position_update/1]).
-export([read/1]).
-export([start_link/0]).
-export([table_map/1]).
-export([truncate/1]).
-export([write/1]).
-import(pgec_statem, [nei/1]).
-import(pgec_storage_common, [bucket/1]).
-import(pgec_storage_common, [key/2]).
-import(pgec_storage_common, [pt/1]).
-import(pgec_storage_common, [value/2]).
-import(pgmp_statem, [send_request/1]).
-include("pgec_storage.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("leveled/include/leveled.hrl").

start_link() ->
    gen_statem:start({local, ?MODULE}, ?MODULE, [], []).


keys(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


position(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


position_update(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


read(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


delete(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


write(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


table_map(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


truncate(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


metadata(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


send_request(Arg, Action) ->
    ?FUNCTION_NAME(Arg, Action, config(Action)).


send_request(Arg, Action, Config) ->
    send_request(
      maps:without(
        args(Config),
        maybe_label(
          Arg#{request => {request, args(Action, Arg, Config)}}))).

config(keys) ->
    [publication,
     table,
     {folder, fun
                  (_Bucket, Key, A) ->
                      [Key | A]
              end},
     {accumulator, []}];

config(position_update) ->
    [publication, position];

config(Action) when Action == write;
                    Action == delete ->
    [publication, table, row];

config(read) ->
    [publication, table, key];

config(Action) when Action == metadata;
                    Action == truncate ->
    [publication, table];

config(table_map) ->
    [publication,
     table,
     schema,
     keys,
     columns,
     oids];

config(position) ->
    [publication].


args(Config) ->
    lists:map(
      fun
          ({Key, _}) ->
              Key;

          (Key) ->
              Key
      end,
      Config).


args(Action, Arg, Config) ->
    lists:foldl(
      fun
          ({Parameter, Default}, A) ->
              A#{Parameter => maps:get(Parameter, Arg, Default)};

          (Parameter, A) ->
              case maps:find(Parameter, Arg) of
                  {ok, Value} ->
                      A#{Parameter => Value};

                  error ->
                      error(arg_missing, [Parameter])
              end
      end,
      #{action => Action},
      Config).


maybe_label(#{requests := _, label := _} = Arg) ->
    Arg;

maybe_label(#{requests := _} = Arg) ->
    Arg#{label => ?MODULE};

maybe_label(Arg) ->
    Arg.

callback_mode() ->
    handle_event_function.


init([]) ->
    process_flag(trap_exit, true),
    {ok,
     ready,
     #{requests => gen_server:reqids_new(),
       cache => ets:new(cache, [{keypos, 2}]),
       mappings => ets:new(?MODULE, [protected])},
     nei(leveled)}.


handle_event(internal, leveled, _, Data) ->
    case pgec_sup:get_child(hd(get('$ancestors')), leveled) of
        {_, PID, worker, _} when is_pid(PID) ->
            {keep_state, Data#{storage => PID}};

        {_, _, _, _} = Reason ->
            {stop, Reason};

        false ->
            {stop, no_storage}
    end;

handle_event({call, From},
             {request,
              #{action := position_update,
                position := Position}},
             _,
             _) ->
    {keep_state_and_data,
     nei({put,
          #{from => From,
            bucket => <<"pgec">>,
            key => position,
            value => Position}})};

handle_event({call, From},
             {request, #{action := position}},
             _,
             _) ->
    {keep_state_and_data,
     nei({get,
          #{from => From,
            bucket => <<"pgec">>,
            key => position}})};

handle_event({call, From},
             {request,
              #{action := read,
                key := Key} = Detail},
             _,
             _) ->
    {keep_state_and_data,
     nei({cache_read,
          #{bucket => bucket(Detail),
            from => From,
            key => Key}})};

handle_event({call, From},
             {request,
              #{action := table_map} = Mapping},
             _,
             #{mappings := Mappings}) ->
    NonValue = [action, publication, table],
    Key = pt(Mapping),
    Value = maps:without(NonValue, Mapping),
    ets:insert(Mappings, {Key, Value}),
    {keep_state_and_data,
     nei({put,
          #{from => From,
            bucket => <<"pgec/mapping">>,
            key => Key,
            value => Value}})};

handle_event({call, From},
             {request,
              #{action := metadata} = Metadata},
             _,
             _) ->
    {keep_state_and_data,
     nei({cache_read,
          #{from => From,
            bucket => <<"pgec/mapping">>,
            key => pt(Metadata)}})};

handle_event({call, From},
             {request,
              #{action := truncate = Action} = Detail},
             _,
             Data) ->
    {next_state,
     {Action, make_ref()},
     Data,
     [{push_callback_module, pgec_storage_truncate},
      nei({truncate, #{bucket => bucket(Detail)}}),
      {reply, From, ok}]};

handle_event({call, From},
             {request,
              #{action := write,
                row := Row} = Detail},
             _,
             #{mappings := Mappings} = Data) ->
    case ets:lookup(Mappings, pt(Detail)) of
        [] ->
            {next_state,
             unready,
             Data,
             [{push_callback_module, pgec_storage_backfill},
              nei({missing, Detail}),
              postpone]};

        [{_, Mapping}] ->
            BKV = #{bucket => bucket(Detail),
                    key => key(Row, Mapping),
                    value => value(Row, Mapping)},

            {keep_state_and_data,
             [nei({cache_write, BKV}),
              nei({put, BKV#{from => From}})]}
    end;

handle_event({call, From},
             {request,
              #{action := delete,
                row := Row} = Detail},
             _,
             #{mappings := Mappings} = Data) ->
    case ets:lookup(Mappings, pt(Detail)) of
        [] ->
            {next_state,
             unready,
             Data,
             [{push_callback_module, pgec_storage_backfill},
              nei({missing, Detail}),
              postpone]};

        [{_, Mapping}] ->
            BK = #{bucket => bucket(Detail),
                   key => key(Row, Mapping)},
            {keep_state_and_data,
             [nei({cache_delete, BK}),
              nei({delete, BK#{from => From}})]}
    end;

handle_event({call, From},
             {request, #{action := keys} = Detail},
             _,
             _) ->
    {keep_state_and_data,
     nei({keys,
          maps:merge(
            #{from => From,
              bucket => bucket(Detail)},
            maps:with([folder, accumulator], Detail))})};

handle_event(EventType, EventContent, State, Data) ->
    pgec_storage_common:handle_event(EventType,
                                     EventContent,
                                     State,
                                     Data).
