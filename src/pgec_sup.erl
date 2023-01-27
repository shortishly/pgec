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


-module(pgec_sup).


-behaviour(supervisor).
-export([get_child/2]).
-export([init/1]).
-export([start_link/0]).
-export([supervisor/1]).
-export([worker/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, configuration()}.

configuration() ->
    {pgec_config:sup_flags(?MODULE), children()}.


children() ->
    [worker(pgec_telemetry),
     worker(pgec_metadata) |
     lists:map(
       fun
           (Publication) ->
               worker(#{id => Publication,
                        m => pgec_table_metadata,
                        args => [#{publication => Publication}]})
       end,
       pgmp_config:replication(logical, publication_names))].


worker(Arg) ->
    child(Arg).


supervisor(Arg) ->
    maps:merge(child(Arg), #{type => supervisor}).


child(#{m := M} = Arg) ->
    maps:merge(
      #{id => pgec_util:tl_snake_case(M), start => mfargs(Arg)},
      maps:with(keys(), Arg));

child(Arg) when is_atom(Arg) ->
    ?FUNCTION_NAME(#{m => Arg}).


mfargs(#{m := M} = Arg) ->
    {M, maps:get(f, Arg, start_link), maps:get(args, Arg, [])}.


keys() ->
    [id, start, restart, significant, shutdown, type, modules].


get_child(SupRef, Id) ->
    lists:keyfind(Id, 1, supervisor:which_children(SupRef)).
