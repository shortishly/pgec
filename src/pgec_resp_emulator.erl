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


-module(pgec_resp_emulator).


-export([info/1]).
-export([init/1]).
-export([recv/1]).
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


init([]) ->
    {ok,
     #{protocol => #{version => 2},
       requests => gen_statem:reqids_new()}}.


recv(#{command := #{name := info},
       message := {array, [_]}}) ->
    {continue,
     {encode,
      {bulk,
       ["# Server\r\nredis_version:", pgec:version(), "\r\n"]}}};

recv(#{command := #{name := ping},
       message := {array, [_]}}) ->
    {continue, {encode, {string, "pong"}}};

recv(#{command := #{name := ping},
       message := {array, [_, {bulk, _} = Greeting]}}) ->
    {continue, {encode, Greeting}};

recv(#{command := #{name := command},
       message := {array, _}}) ->
    {continue, {encode, {array, []}}};

recv(#{command := #{name := hello},
       message := {array, [_, {bulk, <<"3">>}]},
       data := #{protocol := Protocol} = Data}) ->
    {continue,
     Data#{protocol := Protocol#{version => 3}},
     {encode,
      {map,
       [{{bulk, <<"server">>}, {bulk, <<"pgec">>}},
        {{bulk, <<"version">>}, {bulk, pgec:version()}},
        {{bulk,<<"proto">>}, {integer, 3}},
        {{bulk, <<"id">>}, {integer, erlang:phash2(self())}},
        {{bulk, <<"mode">>}, {bulk, <<"standalone">>}},
        {{bulk, <<"role">>}, {bulk, <<"master">>}},
        {{bulk, <<"modules">>}, {array, []}}]}}};

recv(#{command := #{name := hello},
       message := {array, [_, {bulk, <<"2">>}]},
       data := #{protocol := Protocol} = Data}) ->
    {continue,
     Data#{protocol := Protocol#{version => 2}},
     {encode,
      {array,
       [{bulk, <<"server">>}, {bulk, <<"pgec">>},
        {bulk, <<"version">>}, {bulk, pgec:version()},
        {bulk, <<"proto">>}, {integer, 2},
        {bulk, <<"id">>}, {integer, erlang:phash2(self())},
        {bulk, <<"mode">>}, {bulk, <<"standalone">>},
        {bulk, <<"role">>}, {bulk, <<"master">>},
        {bulk, <<"modules">>}, {array, []}]}}};

recv(#{command := #{name := hello},
       message := {array, [_]},
       data := #{protocol := Protocol} = Data}) ->
    {continue,
     Data#{protocol := Protocol#{version => 2}},
     {encode,
      {array,
       [{bulk, <<"server">>}, {bulk, <<"pgec">>},
        {bulk, <<"version">>}, {bulk, pgec:version()},
        {bulk, <<"proto">>}, {integer, 2},
        {bulk, <<"id">>}, {integer, erlang:phash2(self())},
        {bulk, <<"mode">>}, {bulk, <<"standalone">>},
        {bulk, <<"role">>}, {bulk, <<"master">>},
        {bulk, <<"modules">>}, {array, []}]}}};

recv(#{command := #{name := exists},
       message := {array, [_ | Keys]}}) ->
    {continue,
     {encode,
      {integer,
       lists:foldl(
         fun
             ({bulk, Key}, A) ->
                          try lookup(ptk(Key)) of
                              {ok, _} ->
                                  A + 1;

                              not_found ->
                                  A

                          catch
                              error:badarg ->
                                  A
                          end
                  end,
         0,
         Keys)}}};

recv(#{command := #{name := hexists},
       message := {array, [_, {bulk, Key}, {bulk, Field}]} = Message}) ->
    {continue,
     {encode,
      {integer,
       try lookup(ptk(Key)) of
           {ok, #{metadata := Metadata, row := Row}} ->
               ?LOG_DEBUG(#{metadata => Metadata,
                            message => Message,
                            row => Row}),
               case row(Metadata, Row) of
                   #{Field := Value} when Value /= null ->
                       1;

                   #{} ->
                       0
               end;

           not_found ->
               0

       catch
           error:badarg ->
               0
       end}}};

recv(#{command := #{name := hset},
       message := {array, [_, {bulk, Key} | NameValues]}}) ->
    #{key := Encoded} = PTK = ptk(Key),
    try lookup(PTK) of
        {ok, #{metadata := _Metadata}} ->
            [{_, Metadata}] = metadata(PTK),
            case pbe(update(Metadata, names(Metadata, NameValues)),
                     pgec_kv:keys(
                       Metadata,
                       string:split(
                         Encoded,
                         "/",
                         all)) ++ values(Metadata, NameValues)) of
                {ok, Changed} ->
                    {continue,
                     {encode,
                      {integer, Changed}}};

                {error, _} = Error ->
                    {continue, {encode, Error}}
            end;

        not_found ->
            [{_, Metadata}] = metadata(PTK),

            case pbe(insert(Metadata, names(Metadata, NameValues)),
                     pgec_kv:keys(
                                   Metadata,
                                   string:split(
                                     Encoded,
                                     "/",
                                     all)) ++ values(Metadata, NameValues)) of
                {ok, Changed} ->
                    {continue,
                     {encode,
                      {integer, Changed}}};

                {error, _} = Error ->
                    {continue, {encode, Error}}
            end
    catch
        error:badarg ->
            {continue, {encode, {integer, 0}}}
    end;

recv(#{command := #{name := del},
       message := {array, [_ | Keys]}}) ->
    {continue,
     {encode,
      {integer,
       try
           [{command_complete, 'begin'}] = pgmp_connection_sync:query(
                                             #{sql => "begin"}),

           N = lists:foldl(
                 fun
                     ({bulk, Key}, A) ->
                         #{key := Encoded} = PTK = ptk(Key),

                         [{_, Metadata}] = metadata(PTK),

                         [{parse_complete, []}] = pgmp_connection_sync:parse(
                                                    #{sql => delete(Metadata)}),

                         [{bind_complete, []}] = pgmp_connection_sync:bind(
                                                   #{args => pgec_kv:keys(
                                                               Metadata,
                                                               string:split(
                                                                 Encoded,
                                                                 "/",
                                                                 all))}),

                         [{command_complete,
                           {delete, 1}}] =  pgmp_connection_sync:execute(#{}),
                         A + 1
                 end,
                 0,
                 Keys),

           [{command_complete, commit}] = pgmp_connection_sync:query(
                                            #{sql => "commit"}),

           N
       catch
           error:badarg ->
               [{command_complete, rollback}] = pgmp_connection_sync:query(#{sql => "rollback"}),
               0
       end}}};

recv(#{command := #{name := hget},
       message := {array, [_, {bulk, Key}, {bulk, Field}]}}) ->
    {continue,
     {encode,
      {bulk,
       try lookup(ptk(Key)) of
           {ok, #{metadata := Metadata, row := Row}} ->
               case row(Metadata, Row) of
                   #{Field := Value} when is_integer(Value) ->
                       integer_to_list(Value);

                   #{Field := Value} when is_float(Value) ->
                       float_to_list(Value, [short]);

                   #{Field := Value} when is_map(Value) ->
                       apply(
                         pgmp_config:codec(json),
                         encode,
                         [Value]);

                   #{Field := Value} when Value /= null ->
                       Value;

                   #{} ->
                       null
               end;

           not_found ->
               null

       catch
           error:badarg ->
               null
       end}}};

recv(#{command := #{name := hgetall},
       message := {array, [_, {bulk, Key}]}}) ->
    {continue,
     {encode,
      {array,
       try lookup(ptk(Key)) of
           {ok, #{metadata := Metadata, row := Row}} ->
               maps:fold(
                 fun
                     (_, null, A) ->
                         A;

                     (K, V, A) when is_integer(V) ->
                         [{bulk, K}, {bulk, integer_to_list(V)} | A];

                     (K, V, A) when is_float(V) ->
                         [{bulk, K}, {bulk, float_to_list(V, [short])} | A];

                     (K, V, A) when is_map(V) ->
                         [{bulk, K},
                          {bulk,
                           apply(
                             pgmp_config:codec(json),
                             encode,
                             [V])} | A];

                     (K, V, A) ->
                         [{bulk, K}, {bulk, V} | A]
                 end,
                 [],
                 row(Metadata, Row));

           not_found ->
               []

       catch
           error:badarg ->
               []
       end}}};

recv(#{command := #{name := hlen},
       message := {array, [_, {bulk, Key}]}}) ->
    {continue,
     {encode,
      {integer,
       try lookup(ptk(Key)) of
           {ok, #{metadata := Metadata, row := Row}} ->
               maps:fold(
                 fun
                     (_, null, A) ->
                         A;

                     (_, _, A) ->
                         A + 1
                 end,
                 0,
                 row(Metadata, Row));

        not_found ->
               0

    catch
        error:badarg ->
            0
    end}}};

recv(#{command := #{name := hkeys},
       message := {array, [_, {bulk, Key}]}}) ->
    {continue,
     {encode,
      {array,
       try lookup(ptk(Key)) of
           {ok, #{metadata := Metadata, row := Row}} ->
               maps:fold(
                 fun
                     (_, null, A) ->
                         A;

                     (K, _, A) ->
                         [{bulk, K} | A]
                 end,
                 [],
                 row(Metadata, Row));

        not_found ->
               []

    catch
        error:badarg ->
            []
    end}}};

recv(#{command := #{name := psubscribe},
       message := {array, [_, {bulk, Pattern}]}}) ->
    [_, PTK] = string:split(Pattern, ":"),
    case ptk(PTK) of
        #{publication := Publication, table := Table, key := <<"*">>} ->
            pgmp_pg:join(#{m => pgmp_rep_log_ets,
                           publication => Publication,
                           name => Table}),
            {continue,
             {encode,
              {array,
               [{bulk, "subscribe"},
                {bulk, Pattern},
                {integer, 1}]}}}
    end;

recv(#{message := {array, [{bulk, Command} | _]}} = Unknown) ->
    ?LOG_DEBUG(#{unknown => Unknown}),
    {continue, {encode, {error, ["unknown command '", Command, "'"]}}}.


info({notify,
      #{action := Action,
        keys := Keys,
        name := Name,
        publication := Publication}}) ->
    {continue,
     lists:map(
       fun
           (Key) ->
               {encode,
                {array,
                 [{bulk, "message"},
                  {bulk, topic("__keyspace@0__", Publication, Name, Key)},
                  {bulk, action(Action)}]}}
       end,
       Keys)};

info(Message) ->
    ?LOG_DEBUG(#{info => Message}),
    continue.


lookup(PTK) ->
    ?LOG_DEBUG(#{ptk => PTK}),

    case metadata(PTK) of
        [{_, Metadata}] ->
            ?LOG_DEBUG(#{metadata => Metadata}),

            case lookup(Metadata, PTK) of
                [Row] ->
                    ?LOG_DEBUG(#{row => Row}),
                    {ok, #{metadata => Metadata, row => Row, ptk => PTK}};

                [] ->
                    not_found
            end;

        [] ->
            not_found
    end.


lookup(Metadata, PTK) ->
    ?LOG_DEBUG(#{metadata => Metadata, ptk => PTK}),
     ets:lookup(table(Metadata, PTK), key(Metadata, PTK)).


table(#{table := Table} = Metadata, PTK) ->
    ?LOG_DEBUG(#{metadata => Metadata, ptk => PTK}),
    Table.


key(Metadata, #{key := Encoded} = PTK) ->
    ?LOG_DEBUG(#{metadata => Metadata, ptk => PTK}),
    pgec_kv:key(Metadata, string:split(Encoded, "/", all)).


metadata(#{publication := _, table := _} = Arg) ->
    ?LOG_DEBUG(#{arg => Arg}),
    pgec_metadata:lookup(Arg).


ptk(PTK) ->
    ?LOG_DEBUG(#{ptk => PTK}),
    case string:split(PTK, ".", all) of
        [Publication, Table, Key] ->
            #{publication => Publication, table => Table, key => Key};

        _Otherwise ->
            error(badarg, [PTK])
    end.


row(Metadata, Row) ->
    ?LOG_DEBUG(#{metadata => Metadata, row => Row}),
    pgec_kv:row(Metadata, <<"application/json">>, Row).


topic(Prefix, Publication, Name, Key) when is_integer(Key) ->
    ?FUNCTION_NAME(Prefix, Publication, Name, integer_to_list(Key));

topic(Prefix, Publication, Name, Key) ->
    [Prefix, ":", Publication, ".", Name, ".", Key].


action(delete) ->
    "del";

action(insert_new) ->
    "set";

action(update) ->
    "set".


update(#{keys := Positions,
         columns := Columns,
         namespace := Namespace,
         table := Table},
       Names) ->
    [io_lib:format(
       "update ~s.~s set ",
       [Namespace, Table]),

     lists:join(
       ", ",
       lists:map(
         fun
             ({Name, Position}) ->
                 io_lib:format(
                   "~s = $~b",
                   [Name, length(Positions) + Position])
         end,
         lists:zip(Names, lists:seq(1, length(Names))))),

     " where ",

     lists:join(
       " and ",
       lists:map(
         fun
             ({Column, Position}) ->
                 io_lib:format(
                   "~s = $~b",
                   [Column, Position])
         end,
         [{lists:nth(
             Position,
             Columns), N} || {Position, N} <- lists:zip(
                                                Positions,
                                                lists:seq(
                                                  1, length(Positions)))]))].


insert(#{keys := Positions,
         columns := Columns,
         namespace := Namespace,
         table := Table},
       Names) ->
    [io_lib:format(
       "insert into ~s.~s (",
       [Namespace, Table]),

     lists:join(
       ", ",
       [lists:nth(Position, Columns) || Position <- Positions] ++ Names),

     ") values (",

     lists:join(
       ", ",
       lists:map(
         fun
             (Parameter) ->
                 [$$, integer_to_list(Parameter)]
         end,
         lists:seq(1, length(Positions) + length(Names)))),

     ")"].


names(Metadata, [{bulk, Name}, {bulk, _} | T]) ->
    [Name | ?FUNCTION_NAME(Metadata, T)];

names(_, []) ->
    [].


values(Metadata, NameValues) ->
    ?FUNCTION_NAME(Metadata, NameValues, []).


values(#{coids := COIDs} = Metadata,
       [{bulk, Name}, {bulk, Encoded} | T] = L,
       A) ->
    case maps:find(Name, COIDs) of
        {ok, OID} ->
            ?FUNCTION_NAME(
               Metadata,
               T,
               [{#{format => text, type_oid => OID}, Encoded} | A]);

        error ->
            error(badarg, [Metadata, L, A])
    end;

values(_, [], A) ->
    pgmp_data_row:decode(#{<<"client_encoding">> => <<"UTF8">>}, lists:reverse(A)).


delete(#{keys := Positions,
         columns := Columns,
         namespace := Namespace,
         table := Table}) ->
    [io_lib:format(
       "delete from ~s.~s where ",
       [Namespace, Table]),
     lists:join(
       " and ",
       lists:map(
         fun
             ({Column, Position}) ->
                 io_lib:format(
                   "~s = $~b",
                   [Column, Position])
         end,
         [{lists:nth(
             Position,
             Columns), N} || {Position, N} <- lists:zip(
                                                Positions,
                                                lists:seq(
                                                  1, length(Positions)))]))].


pbe(SQL, Bindings) ->
    [{command_complete, 'begin'}] = pgmp_connection_sync:query(
                                      #{sql => "begin"}),

    case pgmp_connection_sync:parse(#{sql => SQL}) of
        [{parse_complete, []}] ->
            case pgmp_connection_sync:bind(#{args => Bindings}) of
                [{bind_complete, []}] ->
                    case pgmp_connection_sync:execute(#{}) of
                        [{command_complete,
                          {Reason, Changed}}] when Reason == update;
                                                   Reason == insert;
                                                   Reason == delete ->
                            [{command_complete,
                              commit}] = pgmp_connection_sync:query(
                                           #{sql => "commit"}),
                            {ok, Changed}

                    end;

                [{error, Reason}] ->
                    [{command_complete, rollback}] = pgmp_connection_sync:query(
                                                       #{sql => "rollback"}),
                    {error, ["ERROR", ": ", atom_to_list(Reason)]}
            end;

        [{error, Reason}] ->
            [{command_complete, rollback}] = pgmp_connection_sync:query(
                                               #{sql => "rollback"}),
            {error, ["ERROR", ": ", atom_to_list(Reason)]};

        [{error_response,
          #{code := Code,
            message := Message,
            severity_localized := SeverityLocalized}}] ->

            [{command_complete, rollback}] = pgmp_connection_sync:query(
                                               #{sql => "rollback"}),
            {error, [SeverityLocalized, " ", Code, ": ", Message]}
    end.
