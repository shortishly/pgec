%% Copyright (c) 2022 Peter Morgan <peter.james.morgan@gmail.com>
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


-module(pgec_mcd_emulator_text).


-export([recv/1]).
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


recv(#{message := #{command := quit}}) ->
    stop;

recv(#{message := #{command := get, keys := Keys}} = Arg) ->
    ?LOG_DEBUG(#{arg => Arg}),
    {continue,
     lists:foldl(
       fun
           (Key, A) ->
               try lookup(ptk(Key)) of
                   {ok, #{metadata := Metadata, row := Row}} ->
                       [{encode,
                         #{command => value,
                           key => Key,
                           flags => 0,
                           expiry => 0,
                           data => jsx:encode(row(Metadata, Row))}} | A];

                   not_found ->
                       A
               catch
                   error:badarg ->
                       A
               end
       end,
       [{encode, #{command => 'end'}}],
       Keys)};

recv(#{message := #{command := Command}}) ->
    {continue,
     {encode,
      #{command => client_error,
        reason => io_lib:format("~p: not implemented", [Command])}}}.


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
    Key = key(Metadata, PTK),
    ?LOG_DEBUG(#{metadata => Metadata, ptk => PTK, key => Key}),
    case pgec_storage_sync:read(PTK#{key := Key}) of
        {ok, Value} ->
            [pgec_storage_common:row(Key, Value, Metadata)];

        not_found ->
            []
    end.


key(Metadata, #{key := Encoded} = PTK) ->
    ?LOG_DEBUG(#{metadata => Metadata, ptk => PTK}),
    pgec_kv:key(Metadata, string:split(Encoded, "/", all)).


metadata(#{publication := Publication, table := Table} = Arg) ->
    ?LOG_DEBUG(#{arg => Arg}),
    case pgec_storage_sync:metadata(Arg) of
        {ok, Metdata} ->
            [{{Publication, Table}, Metdata}];

        not_found ->
            []
    end.


ptk(PTK) ->
    ?LOG_DEBUG(#{ptk => PTK}),
    case string:split(PTK, ".", all) of
        [Publication, Table, Key] ->
            #{publication => Publication, table => Table, key => Key};

        _Otherwise ->
            error(badarg, [PTK])
    end.


row(Metadata, Row) ->
    Result = pgec_kv:row(Metadata, <<"application/json">>, Row),
    ?LOG_DEBUG(#{metadata => Metadata, row => Row, result => Result}),
    Result.
