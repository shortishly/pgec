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
-import(pgmp_connection_sync, [bind/1]).
-import(pgmp_connection_sync, [execute/1]).
-import(pgmp_connection_sync, [parse/1]).
-import(pgmp_connection_sync, [query/1]).
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
       message := {array, [_, {bulk, Key}, {bulk, Field}]}}) ->
    {continue,
     {encode,
      {integer,
       try lookup(ptk(Key)) of
           {ok, #{metadata := Metadata, row := Row}} ->
               try row(Metadata, Row) of
                   #{Field := Value} when Value /= null ->
                       1;

                   #{} ->
                       0

               catch
                   Class:Exception:Stacktrace ->
                       ?LOG_DEBUG(#{class => Class,
                                    exception => Exception,
                                    stacktrace => Stacktrace}),
                       0
               end;

           not_found ->
               0
       catch
           error:badarg ->
               0
       end}}};

recv(#{command := #{name := hset},
       message := {array, [_, {bulk, Key} | NameValues]}} = Recv) ->
    ?LOG_DEBUG(#{recv => Recv}),
    #{key := Encoded} = PTK = ptk(Key),
    try lookup(PTK) of
        {ok, #{metadata := _Metadata}} ->
            [{_, Metadata}] = metadata(PTK),
            try pbe(update(Metadata, names(Metadata, NameValues)),
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

                {error, Reason} ->
                    {continue, {encode, {error, Reason}}}

            catch
                error:badarg:Stacktrace ->
                    ?LOG_DEBUG(#{stacktrace => Stacktrace}),
                    {continue, {encode, {integer, 0}}};

                Class:Exception:Stacktrace ->
                    ?LOG_DEBUG(#{class => Class,
                                   exception => Exception,
                                   stacktrace => Stacktrace}),
                    {continue, {encode, encode({error, Exception})}}
            end;

        not_found ->
            [{_, Metadata}] = metadata(PTK),

            try pbe(insert(Metadata, names(Metadata, NameValues)),
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

                {error, Reason} ->
                    {continue, {encode, {error, Reason}}}

            catch
                error:badarg:Stacktrace ->
                    ?LOG_DEBUG(#{stacktrace => Stacktrace}),
                    {continue, {encode, {integer, 0}}};

                Class:Exception:Stacktrace ->
                    ?LOG_DEBUG(#{class => Class,
                                 exception => Exception,
                                 stacktrace => Stacktrace}),
                    {continue, {encode, encode({error, Exception})}}
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
           start(),

           N = lists:foldl(
                 fun
                     ({bulk, Key}, A) ->
                         #{key := Encoded} = PTK = ptk(Key),

                         [{_, Metadata}] = metadata(PTK),

                         [{parse_complete, []}] = parse(
                                                    #{sql => delete(Metadata)}),

                         [{bind_complete, []}] = bind(
                                                   #{args => pgec_kv:keys(
                                                               Metadata,
                                                               string:split(
                                                                 Encoded,
                                                                 "/",
                                                                 all))}),

                         [{command_complete,
                           {delete, 1}}] = execute(#{}),
                         A + 1
                 end,
                 0,
                 Keys),

           commit(),
           N

       catch
           Class:Exception:Stacktrace ->
               ?LOG_DEBUG(#{class => Class,
                            exception => Exception,
                            stacktrace => Stacktrace}),
               rollback(),
               0
       end}}};

recv(#{command := #{name := hget},
       message := {array, [_, {bulk, Key}, {bulk, Field}]}} = Recv) ->
    ?LOG_DEBUG(#{recv => Recv}),
    {continue,
     {encode,
      {bulk,
       try lookup(ptk(Key)) of
           {ok, #{metadata := Metadata, row := Row}} ->
               try row(Metadata, Row) of
                   #{Field := Value} when Value /= null ->
                       ?LOG_DEBUG(#{field => Field, value => Value}),
                       Value;

                   Otherwise ->
                       ?LOG_DEBUG(#{otherwise => Otherwise}),
                       null
               catch
                   Class:Exception:Stacktrace ->
                       ?LOG_DEBUG(#{class => Class,
                                    exception => Exception,
                                    stacktrace => Stacktrace}),
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
               try maps:fold(
                     fun
                         (_, null, A) ->
                             A;

                         (K, V, A) ->
                             [{bulk, K}, {bulk, V} | A]
                     end,
                     [],
                     row(Metadata, Row))
               catch
                   Class:Exception:Stacktrace ->
                       ?LOG_DEBUG(#{class => Class,
                                    exception => Exception,
                                    stacktrace => Stacktrace}),
                       []
               end;

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
               try maps:fold(
                     fun
                         (_, null, A) ->
                             A;

                         (_, _, A) ->
                             A + 1
                     end,
                     0,
                     row(Metadata, Row))

               catch
                   Class:Exception:Stacktrace ->
                       ?LOG_DEBUG(#{class => Class,
                                    exception => Exception,
                                    stacktrace => Stacktrace}),
                       0
               end;

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
               try maps:fold(
                     fun
                         (_, null, A) ->
                             A;

                         (K, _, A) ->
                             [{bulk, K} | A]
                     end,
                     [],
                     row(Metadata, Row))
               catch
                   Class:Exception:Stacktrace ->
                       ?LOG_DEBUG(#{class => Class,
                                    exception => Exception,
                                    stacktrace => Stacktrace}),
                       []
               end;

           not_found ->
               []
       catch
           error:badarg ->
               []
       end}}};

recv(#{command := #{name := psubscribe},
       message := {array, [_, {bulk, Pattern}]}}) ->
    [_, PTK] = string:split(Pattern, ":"),
    try ptk(PTK) of
        #{publication := Publication, table := Table} ->
            pgmp_pg:join(#{m => pgmp_rep_log_ets,
                           publication => Publication,
                           name => Table}),
            {continue,
             {encode,
              {array,
               [{bulk, "subscribe"},
                {bulk, Pattern},
                {integer, 1}]}}}

    catch
        error:badarg ->
            []
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
    Result = pgec_kv:row(Metadata, <<"text/plain">>, Row),
    ?LOG_DEBUG(#{metadata => Metadata,
                 row => Row,
                result => Result}),
    Result.


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
        {ok, Type} ->
            ?FUNCTION_NAME(
               Metadata,
               T,
               [decode(Type, Encoded) | A]);

        error ->
            error(badarg, [Metadata, L, A])
    end;

values(_, [], A) ->
    lists:reverse(A).


decode(#{<<"typname">> := <<"bool">>}, Value)
  when Value == <<"true">>; Value == <<"false">> ->
    binary_to_atom(Value);

decode(#{<<"typname">> := Name}, Value)
  when Name == <<"int2">>;
       Name == <<"int4">>;
       Name == <<"int8">> ->
    binary_to_integer(Value);

decode(#{<<"typname">> := <<"float", _/bytes>>}, Value) ->
    try
        binary_to_float(Value)
    catch
        error:badarg ->
            binary_to_integer(Value)
    end;

decode(#{<<"typname">> := Name}, Value)
  when Name == <<"timestamp">>;
       Name == <<"timestampz">> ->
    calendar:rfc3339_to_system_time(
      binary_to_list(Value),
      [{unit, microsecond}]);

decode(#{<<"typname">> := <<"date">>},
       <<Ye:4/bytes, "-", Mo:2/bytes, "-", Da:2/bytes>>) ->
    triple(Ye, Mo, Da);

decode(#{<<"typname">> := <<"time">>},
       <<Ho:2/bytes, ":", Mi:2/bytes, ":", Se:2/bytes>>) ->
    triple(Ho, Mi, Se);

decode(Type, Value) ->
    ?LOG_DEBUG(#{type => Type, value => Value}),
    Value.


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
    ?LOG_DEBUG(#{sql => SQL, bindings => Bindings}),
    start(),
    case parse(#{sql => SQL}) of
        [{parse_complete, []}] ->
            case bind(#{args => Bindings}) of
                [{bind_complete, []}] ->
                    case execute(#{}) of
                        [{command_complete,
                          {Reason, Changed}}] when Reason == update;
                                                   Reason == insert;
                                                   Reason == delete ->
                            commit(),
                            {ok, Changed}
                    end;

                [{error, _} = Error] ->
                    rollback(),
                    encode(Error)
            end;

        [{error, _} = Error] ->
            rollback(),
            encode(Error);

        [{error_response,
          #{code := Code,
            message := Message,
            severity_localized := SeverityLocalized}}] ->

            rollback(),
            {error, [SeverityLocalized, " ", Code, ": ", Message]}
    end.


start() ->
    [{command_complete, 'begin'}] = query(#{sql => "begin"}),
    ok.

commit() ->
    [{command_complete, commit}] = query(#{sql => "commit"}),
    ok.

rollback() ->
    [{command_complete, rollback}] = query(#{sql => "rollback"}),
    ok.


encode({error, Reason}) when is_atom(Reason) ->
    {error, ["ERROR", ": ", atom_to_list(Reason)]}.

triple(X, Y, Z) ->
    list_to_tuple([binary_to_integer(I) || I <- [X, Y, Z]]).
