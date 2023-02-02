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


-module(pgec_kv).


-export([key/2]).
-export([row/2]).
-export([row/3]).
-include_lib("kernel/include/logger.hrl").


key(#{keys := Positions, oids := Types} = Metadata, Keys)
  when length(Positions) == length(Keys) ->
    ?LOG_DEBUG(#{keys => Keys, metadata => Metadata}),

    case pgmp_data_row:decode(
           #{<<"client_encoding">> => <<"UTF8">>},
           lists:map(
           fun
               ({Encoded, KeyOID}) ->
                   {#{format => text, type_oid => KeyOID}, Encoded}
           end,
           lists:zip(
             Keys,

             lists:map(
               fun
                   (Position) ->
                       lists:nth(Position, Types)
               end,
               Positions)))) of

        [Primary] ->
            Primary;

        Composite ->
            list_to_tuple(Composite)
    end;

key(Metadata, Keys) ->
    error(badarg, [Metadata, Keys]).


row(Metadata, ContentType) ->
    ?LOG_DEBUG(
       #{content_type => ContentType,
         metadata => Metadata}),

    fun
        (Values, A) ->
            [?FUNCTION_NAME(Metadata, ContentType, Values) | A]
    end.


row(#{columns := Columns, oids := OIDS} = Metadata, ContentType, Values) ->
    maps:from_list(
      lists:zipwith3(
        combine(ContentType),
        Columns,
        OIDS,
        values(Metadata, Values))).


values(#{keys := [_]}, Values) ->
    tuple_to_list(Values);

values(#{keys := KeyPositions}, CompositeWithValues) when is_tuple(CompositeWithValues) ->
    [Composite | Values] = tuple_to_list(CompositeWithValues),
    values(1, KeyPositions, tuple_to_list(Composite), Values).


values(_, [], [], Values) ->
    Values;
values(_, _, Keys, []) ->
    Keys;
values(Pos, [Pos | KeyPositions], [Key | Keys], Values) ->
    [Key | ?FUNCTION_NAME(Pos + 1, KeyPositions, Keys, Values)];
values(Pos, KeyPositions, Keys, [Value | Values]) ->
    [Value | ?FUNCTION_NAME(Pos + 1, KeyPositions, Keys, Values)].


combine(ContentType) ->
    fun
        (Column, OID, Value) ->
            #{OID := ColumnType} = pgmp_types:cache(),
            {Column, value(ContentType, ColumnType, Value)}
    end.


value(_ContentType, #{<<"typname">> := <<"date">>}, {Ye, Mo, Da}) ->
    iolist_to_binary(
      io_lib:format(
        "~4..0b-~2..0b-~2..0b",
        [Ye, Mo, Da]));

value(_ContentType, #{<<"typname">> := <<"time">>}, {Ho, Mi, Se}) ->
    iolist_to_binary(
      io_lib:format(
        "~2..0b:~2..0b:~2..0b",
        [Ho, Mi, Se]));

value(_ContentType, _ColumnType, Value) ->
    Value.
