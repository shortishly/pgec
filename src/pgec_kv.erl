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
-export([keys/2]).
-export([row/2]).
-export([row/3]).
-include_lib("kernel/include/logger.hrl").


keys(#{keys := Positions, oids := Types}, Keys) ->
    pgmp_data_row:decode(
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
            Positions))),
      pgmp_types:cache(pgec_util:db())).


key(#{keys := Positions} = Metadata, Keys)
  when length(Positions) == length(Keys) ->
    ?LOG_DEBUG(#{keys => Keys, metadata => Metadata}),

    case keys(Metadata, Keys) of
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
            ?LOG_DEBUG(#{values => Values, a => A}),
            [?FUNCTION_NAME(Metadata, ContentType, Values) | A]
    end.


row(#{columns := Columns, oids := OIDS} = Metadata, ContentType, Values) ->
    ?LOG_DEBUG(#{metadata => Metadata,
                 content_type => ContentType,
                 values => Values}),
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
            #{OID := ColumnType} = pgmp_types:cache(pgec_util:db()),
            {Column, value(ContentType, ColumnType, Value)}
    end.


value(<<"text/plain">> = ContentType,
      ColumnType,
      null = Value) ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),
    Value;

value(<<"text/plain">> = ContentType,
      #{<<"typname">> := Name} = ColumnType,
      Value) when Name == <<"int2">>;
                  Name == <<"int4">>;
                  Name == <<"int8">>;
                  Name == <<"money">> ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),
    integer_to_list(Value);

value(<<"text/plain">> = ContentType,
      #{<<"typname">> := Name} = ColumnType,
      Value) when Name == <<"numeric">>,
                  is_integer(Value) ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),
    integer_to_list(Value);

value(<<"text/plain">> = ContentType,
      #{<<"typname">> := Name} = ColumnType,
      Value) when Name == <<"numeric">>,
                  is_float(Value) ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),
    float_to_list(Value, [short]);

value(ContentType,
      #{<<"typname">> := <<"float", _/bytes>>} = ColumnType,
      Value) when Value == 'NaN';
                  Value == 'Infinity';
                  Value == '-Infinity' ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),
    atom_to_binary(Value);

value(<<"text/plain">> = ContentType,
      #{<<"typname">> := <<"float", _/bytes>>} = ColumnType,
      Value) when is_float(Value) ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),
    float_to_list(Value, [short]);

value(<<"text/plain">> = ContentType,
      #{<<"typname">> := <<"float", _/bytes>>} = ColumnType,
      Value) when is_integer(Value) ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),
    integer_to_list(Value);

value(<<"text/plain">> = ContentType,
      #{<<"typname">> := <<"bool">>} = ColumnType,
      Value) ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),
    atom_to_list(Value);

value(<<"text/plain">> = ContentType,
      #{<<"typname">> := Name} = ColumnType,
      Value) when Name == <<"jsonb">>;
                  Name == <<"json">>,
                  is_map(Value) ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),
    jsx:encode(Value);

value(ContentType,
      #{<<"typname">> := Type} = ColumnType,
      {{Ye, Mo, Da}, {Ho, Mi, Se}} = Value)
  when Type == <<"timestamp">>; Type == <<"timestampz">>->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),

    iolist_to_binary(
      io_lib:format(
        "~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0bZ",
        [Ye, Mo, Da, Ho, Mi, Se]));

value(ContentType,
      #{<<"typname">> := <<"date">>} = ColumnType,
      {Ye, Mo, Da} = Value) ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),

    iolist_to_binary(
      io_lib:format(
        "~4..0b-~2..0b-~2..0b",
        [Ye, Mo, Da]));

value(ContentType,
      #{<<"typname">> := <<"time">>} = ColumnType,
      {Ho, Mi, Se} = Value) ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),
    iolist_to_binary(
      io_lib:format(
        "~2..0b:~2..0b:~2..0b",
        [Ho, Mi, Se]));

value(<<"text/plain">> = ContentType,
      #{<<"typname">> := Type} = ColumnType,
      {From, To} = Value) when Type == <<"lseg">>;
                               Type == <<"box">> ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),
    jsx:encode(#{from => From, to => To});

value(<<"text/plain">> = ContentType,
      #{<<"typname">> := Type} = ColumnType,
      Value) when Type == <<"point">>;
                  Type == <<"line">>;
                  Type == <<"path">>;
                  Type == <<"polygon">>;
                  Type == <<"circle">> ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),
    jsx:encode(Value);


value(ContentType,
      #{<<"typname">> := Type} = ColumnType,
      {From, To} = Value) when Type == <<"lseg">>;
                               Type == <<"box">> ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),
    #{from => From, to => To};

value(ContentType,
      #{<<"typname">> := Name} = ColumnType,
      Value) when (Name == <<"timestamp">> orelse
                   Name == <<"timestampz">>) andalso
                  is_integer(Value) ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),
    iolist_to_binary(
      calendar:system_time_to_rfc3339(
        Value,
        [{unit, microsecond}, {offset, "Z"}]));

value(ContentType, ColumnType, Value) ->
    ?LOG_DEBUG(#{content_type => ContentType,
                 column_type => ColumnType,
                 value => Value}),
    Value.
