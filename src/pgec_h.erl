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


-module(pgec_h).


-define(JSON, <<"application/json">>).
-export([init/2]).
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


init(#{bindings := #{publication := Publication,
                     table := Table,
                     key := _}} = Req,
     Opts) ->

    ?LOG_DEBUG(#{req => Req, opts => Opts}),

    case ets:lookup(
           pgec_metadata,
           {Publication, Table}) of

        [{_, Metadata}] ->
            ?LOG_DEBUG(#{metadata => Metadata}),

            case lookup(Metadata, Req) of
                [Row] ->
                    ContentType = negotiate_content_type(Req),

                    {ok,
                     cowboy_req:reply(
                       200,
                       headers(ContentType),
                       [encode(
                          ContentType,
                          row(ContentType, Metadata, Row)),
                        "\n"],
                       Req),
                     Opts};

                [] ->
                    {ok,
                     not_found(Req,
                               #{publication => Publication,
                                 key => key(Metadata, Req),
                                 table => Table}),
                     Opts}
            end;

        [] ->
            ?LOG_DEBUG(#{metadata => not_found}),
            {ok, not_found(Req, #{publication => Publication, table => Table}), Opts}
    end;

init(#{bindings := #{publication := Publication,
                     table := Table}} = Req,
     Opts) ->

    ?LOG_DEBUG(#{req => Req, opts => Opts}),

    ContentType = negotiate_content_type(Req),

    case ets:lookup(
           pgec_metadata,
           {Publication, Table}) of

        [{_, Metadata}] ->
            {ok,
             cowboy_req:reply(
               200,
               headers(ContentType),
               [encode(
                  ContentType,
                  #{rows => ets:foldl(
                              row(ContentType, Metadata),
                              [],
                              table(Metadata, Req))}),
                "\n"],
               Req),
             Opts};

        [] ->
            {ok, not_found(Req), Opts}
    end;

init(#{bindings := #{publication := Publication}} = Req, Opts) ->
    ?LOG_DEBUG(#{req => Req, opts => Opts}),

    ContentType = negotiate_content_type(Req),

    {ok,
     cowboy_req:reply(
       200,
       headers(ContentType),
       [encode(
          ContentType,
          #{tables => ets:select(
                        pgec_metadata,
                        ets:fun2ms(
                          fun
                              ({{Pub, Table}, _}) when Publication == Pub ->
                                  Table
                          end))}),
        "\n"],
       Req),
     Opts};

init(Req, Opts) ->
    ?LOG_DEBUG(#{req => Req, opts => Opts}),

    {ok,
     cowboy_req:reply(
       200,
       headers(),
       [jsx:encode(#{publications => pgmp_config:replication(
                                       logical,
                                       publication_names)}),
        "\n"],
       Req),
     Opts}.


%% Not much negotiation here, we only support JSON right now.
%%
negotiate_content_type(#{headers := #{}}) ->
    ?JSON.


headers() ->
    ?FUNCTION_NAME(?JSON).


headers(ContentType) ->
    #{<<"content-type">> => ContentType}.


not_found(Req, Body) ->
    cowboy_req:reply(404, headers(), jsx:encode(Body), Req).


not_found(Req) ->
    cowboy_req:reply(404, #{}, <<>>, Req).


lookup(Metadata, Req) ->
    ?LOG_DEBUG(#{metadata => Metadata, req => Req}),
    ets:lookup(table(Metadata, Req), key(Metadata, Req)).


table(Metadata, #{bindings := #{table := Table}}) ->
    ?LOG_DEBUG(#{metadata => Metadata, table => Table}),
    binary_to_existing_atom(Table).


key(#{keys := Positions, oids := Types}, #{bindings := #{key := Encoded}}) ->
    ?LOG_DEBUG(#{keys => Positions, oids => Types, key => Encoded}),

    case lists:map(
           fun
               (Position) ->
                   lists:nth(Position, Types)
           end,
           Positions) of

        [KeyOID] ->
            ?LOG_DEBUG(#{key_oid => keyOID}),
            [Decoded] = pgmp_data_row:decode(
                   #{<<"client_encoding">> => <<"UTF8">>},
                   [{#{format => text, type_oid => KeyOID}, Encoded}]),
            Decoded
    end.


encode(?JSON, Content) ->
    jsx:encode(Content).


row(ContentType, Metadata) ->
    fun
        (Values, A) ->
            [?FUNCTION_NAME(ContentType, Metadata, Values) | A]
    end.


row(ContentType, #{columns := Columns, oids := OIDS}, Row) when is_tuple(element(1, Row)) ->
    [Composite | Values] = tuple_to_list(Row),
    ?FUNCTION_NAME(ContentType, Columns, OIDS, tuple_to_list(Composite) ++ Values, #{});

row(ContentType, #{columns := Columns, oids := OIDS}, Row) ->
    ?FUNCTION_NAME(ContentType, Columns, OIDS, tuple_to_list(Row), #{}).


row(_ContentType, [], [], [] , A) ->
    A;

row(ContentType, [Column | Columns], [OID | OIDs], [Value | Values] , A) ->
    #{OID := ColumnType} = pgmp_types:cache(),
    ?FUNCTION_NAME(ContentType, Columns, OIDs, Values, A#{Column => value(ContentType, ColumnType, Value)}).


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
