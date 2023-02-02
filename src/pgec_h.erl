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
                              pgec_kv:row(Metadata, ContentType),
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

            try lookup(Metadata, Req) of
                [_] = Row ->
                    ContentType = negotiate_content_type(Req),

                    {ok,
                     cowboy_req:reply(
                       200,
                       headers(ContentType),
                       [encode(
                          ContentType,
                          hd(lists:foldl(
                               pgec_kv:row(Metadata, ContentType),
                               [],
                               Row))),
                        "\n"],
                       Req),
                     Opts};

                [] ->
                    {ok,
                     not_found(Req,
                               #{publication => Publication,
                                 keys => Keys,
                                 table => Table}),
                     Opts}

            catch error:badarg ->
                    {ok,
                     not_found(Req,
                               #{publication => Publication,
                                 keys => Keys,
                                 table => Table}),
                     Opts}
            end;

        [] ->
            ?LOG_DEBUG(#{metadata => not_found}),
            {ok,
             not_found(Req,
                       #{publication => Publication,
                         keys => Keys,
                         table => Table}),
             Opts}
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
    cowboy_req:reply(404, headers(), [jsx:encode(Body), "\n"], Req).


not_found(Req) ->
    cowboy_req:reply(404, #{}, <<>>, Req).


lookup(Metadata, Req) ->
    ?LOG_DEBUG(#{metadata => Metadata, req => Req}),
    ets:lookup(table(Metadata, Req), key(Metadata, Req)).


table(Metadata, #{bindings := #{table := Table}}) ->
    ?LOG_DEBUG(#{metadata => Metadata, table => Table}),
    binary_to_existing_atom(Table).


key(Metadata, Req) ->
    ?LOG_DEBUG(#{metadata => Metadata, req => Req}),
    pgec_kv:key(Metadata, cowboy_req:path_info(Req)).


encode(?JSON, Content) ->
    jsx:encode(Content).
