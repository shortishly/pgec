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


-export([init/2]).
-include_lib("kernel/include/logger.hrl").


init(#{bindings := #{publication := Publication,
                     table := Table,
                     key := Key}} = Req,
     Opts) ->
    ?LOG_DEBUG(#{req => Req,
                 table => Table,
                 key => Key,
                 opts => Opts}),

    case lists:member(
           Publication,
           pgmp_config:replication(logical, publication_names)) of

        true ->
            case lookup(Req) of
                [] ->
                    {ok, not_found(Req), Opts};

                [{_, Value}] ->
                    {ok,
                     cowboy_req:reply(200,
                                      #{},
                                      io_lib:format("~p~n", [Value]),
                                      Req),
                     Opts}
            end;

        false ->
            not_found(Req)
    end;

init(#{bindings := #{publication := Publication,
                     table := _}} = Req,
     Opts) ->
    case pgmp_pg:get_members([pgmp_rep_log_ets, Publication]) of
        [Manager] when is_pid(Manager) ->
            {reply, Metadata} = gen_statem:receive_response(
                                  pgmp_rep_log_ets:metadata(
                                    #{server_ref => Manager})),

            Columns = maps:get(table(Req), Metadata),

            {ok,
             cowboy_req:reply(
               200,
               headers(),
               jsx:encode(
                 ets:foldl(
                   fun
                       (Row, A) ->
                           [maps:from_list(
                              lists:zip(Columns, tuple_to_list(Row))) | A]
                   end,
                   [],
                   table(Req))),
              Req),
             Opts}
    end;

init(#{bindings := #{publication := _Publication}} = Req, Opts) ->
    {ok, not_found(Req), Opts};

init(Req, Opts) ->
    {ok,
     cowboy_req:reply(
       200,
       headers(),
       jsx:encode(#{publications => pgmp_config:replication(
                                      logical,
                                      publication_names)}),
       Req),
     Opts}.


headers() ->
    #{<<"content-type">> => <<"application/json">>}.


not_found(Req) ->
    cowboy_req:reply(404, #{}, <<>>, Req).


lookup(Req) ->
    ?FUNCTION_NAME(table(Req), key(Req)).

lookup(Table, Key) ->
    ?LOG_DEBUG(#{table => Table, key => Key}),
    ets:lookup(Table, Key).

table(#{bindings := #{table := Table}}) ->
    binary_to_existing_atom(Table).


key(#{bindings := #{key := Expr}}) ->
    phrase_exprs:eval(#{expressions => phrase_exprs:parse_scan(binary_to_list(Expr))}).
