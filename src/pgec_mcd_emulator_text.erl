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


recv(#{message := #{command := get, keys := Keys}} = Arg) ->
    ?LOG_DEBUG(#{arg => Arg}),
    {continue,
     lists:foldl(
       fun
           (Key, A) ->
               case lookup(ptk(Key)) of
                   {ok, #{metadata := Metadata, row := Row}} ->
                       [{encode,
                         #{command => value,
                           key => Key,
                           flags => 0,
                           expiry => 0,
                           data => jsx:encode(row(Metadata, Row))}} | A];

                   not_found ->
                       A
               end
       end,
       [{encode, #{command => 'end'}}],
       Keys)}.


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


key(#{keys := Positions, oids := Types} = Metadata, #{key := Encoded} = PTK) ->
    ?LOG_DEBUG(#{metadata => Metadata, ptk => PTK}),

    case lists:map(
           fun
               (Position) ->
                   lists:nth(Position, Types)
           end,
           Positions) of

        [KeyOID] ->
            ?LOG_DEBUG(#{key_oid => keyOID}),
            [Decoded] = pgmp_data_row:decode(#{}, [{#{format => text, type_oid => KeyOID}, Encoded}]),
            Decoded
    end.


metadata(#{publication := Publication, table := Table} = Arg) ->
    ?LOG_DEBUG(#{arg => Arg}),
    ets:lookup(pgec_metadata, {Publication, Table}).


ptk(PTK) ->
    ?LOG_DEBUG(#{ptk => PTK}),
    [Publication, Table, Key] = string:split(PTK, ".", all),
    #{publication => Publication, table => Table, key => Key}.


row(#{columns := Columns, oids := OIDS}, Row) when is_tuple(element(1, Row)) ->
    [Composite | Values] = tuple_to_list(Row),
    ?FUNCTION_NAME(Columns, OIDS, tuple_to_list(Composite) ++ Values, #{});

row(#{columns := Columns, oids := OIDS}, Row) ->
    ?FUNCTION_NAME(Columns, OIDS, tuple_to_list(Row), #{}).


row([], [], [] , A) ->
    A;

row([Column | Columns], [OID | OIDs], [Value | Values] , A) ->
    #{OID := ColumnType} = pgmp_types:cache(),
    ?FUNCTION_NAME(Columns, OIDs, Values, A#{Column => value(ColumnType, Value)}).


value(#{<<"typname">> := <<"date">>}, {Ye, Mo, Da}) ->
    iolist_to_binary(
      io_lib:format(
        "~4..0b-~2..0b-~2..0b",
        [Ye, Mo, Da]));

value(#{<<"typname">> := <<"time">>}, {Ho, Mi, Se}) ->
    iolist_to_binary(
      io_lib:format(
        "~2..0b:~2..0b:~2..0b",
        [Ho, Mi, Se]));

value(_ColumnType, Value) ->
    Value.
