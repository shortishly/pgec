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


init(Req, Opts) ->
    ?FUNCTION_NAME(Req, Opts, cowboy_req:path_info(Req)).


init(#{bindings := #{publication := Publication,
                     table := Table}} = Req,
     Opts,
     [] = Keys) ->

    ?LOG_DEBUG(#{req => Req,
                 opts => Opts,
                 keys => Keys}),

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


init(#{bindings := #{publication := Publication,
                     table := Table}} = Req,
     Opts,
     Keys) ->

    ?LOG_DEBUG(#{req => Req, opts => Opts, keys => Keys}),

    case ets:lookup(
           pgec_metadata,
           {Publication, Table}) of

        [{_, Metadata}] ->
            ?LOG_DEBUG(#{metadata => Metadata}),

            case lookup(Metadata, Req) of
                [_] = Row ->
                    ContentType = negotiate_content_type(Req),

                    {ok,
                     cowboy_req:reply(
                       200,
                       headers(ContentType),
                       [encode(
                          ContentType,
                          hd(lists:foldl(
                               row(ContentType, Metadata),
                               [],
                               Row))),
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

init(#{bindings := #{publication := Publication}} = Req,
     Opts,
     _) ->

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

init(Req, Opts, _) ->
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


key(#{keys := Positions, oids := Types} = Metadata, Req) ->
    ?LOG_DEBUG(#{metadata => Metadata, req => Req}),

    case pgmp_data_row:decode(
           #{<<"client_encoding">> => <<"UTF8">>},
           lists:map(
           fun
               ({Encoded, KeyOID}) ->
                   {#{format => text, type_oid => KeyOID}, Encoded}
           end,
           lists:zip(
             cowboy_req:path_info(Req),

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
    end.


encode(?JSON, Content) ->
    jsx:encode(Content).


row(ContentType,
    #{columns := Columns,
      oids := OIDS} = Metadata) ->
    ?LOG_DEBUG(
       #{content_type => ContentType,
         metadata => Metadata}),

    fun
        (Values, A) ->
            [maps:from_list(
               lists:zipwith3(
                 combine(ContentType),
                 Columns,
                 OIDS,
                 values(Metadata, Values))) | A]
    end.


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
