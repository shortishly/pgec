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


-export([init/1]).
-export([recv/1]).
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


init([]) ->
    {ok, #{}}.


recv(#{message := {array, [{bulk, <<"INFO">>}]}}) ->
    {continue, {encode, {bulk, "# Server\r\nredis_version:1.2.3\r\n"}}};

recv(#{message := {array, [{bulk, <<"HGETALL">>}, {bulk, Key}]}}) ->
    try lookup(ptk(Key)) of
        {ok, #{metadata := Metadata, row := Row}} ->

            {continue,
             {encode,
              {array,
               maps:fold(
                 fun
                     (K, V, A) when is_integer(V) ->
                         [{bulk, K}, {bulk, integer_to_list(V)} | A];

                     (K, V, A) when is_float(V) ->
                         [{bulk, K}, {bulk, float_to_list(V, [short])} | A];

                     (K, V, A) ->
                         [{bulk, K}, {bulk, V} | A]
                 end,
                 [],
                 row(Metadata, Row))}}};

        not_found ->
            {continue, {encode, {array, []}}}

    catch
        error:badarg ->
            {continue, {encode, {array, []}}}
    end;

recv(#{message := {array, [{bulk, Command} | _]}}) ->
    {continue, {encode, {error, ["unknown command '", Command, "'"]}}}.


lookup(PTK) ->
    ?LOG_DEBUG(#{ptk => PTK}),

    case metadata(PTK) of
        [{_, Metadata}] ->
            case lookup(Metadata, PTK) of
                [Row] ->
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


table(Metadata, #{table := Table} = PTK) ->
    ?LOG_DEBUG(#{metadata => Metadata, ptk => PTK}),
    binary_to_existing_atom(Table).


key(Metadata, #{key := Encoded} = PTK) ->
    ?LOG_DEBUG(#{metadata => Metadata, ptk => PTK}),
    pgec_kv:key(Metadata, string:split(Encoded, "/", all)).


metadata(#{publication := Publication, table := Table} = Arg) ->
    ?LOG_DEBUG(#{arg => Arg}),
    ets:lookup(pgec_metadata, {Publication, Table}).


ptk(PTK) ->
    ?LOG_DEBUG(#{ptk => PTK}),
    case string:split(PTK, ".", all) of
        [Publication, Table, Key] ->
            #{publication => Publication, table => Table, key => Key};

        _Otherwise ->
            error(badarg, [PTK])
    end.


row(Metadata, Row) ->
    pgec_kv:row(Metadata, <<"application/json">>, Row).
