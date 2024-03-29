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


-module(common).


-feature(maybe_expr, enable).


-export([all/1]).
-export([pbe/1]).
-export([stop_applications/0]).
-include_lib("common_test/include/ct.hrl").


is_a_test(is_a_test) ->
    false;
is_a_test(Function) ->
    hd(lists:reverse(string:tokens(atom_to_list(Function), "_"))) =:= "test".


all(Module) ->
    [Function || {Function, Arity} <- Module:module_info(exports),
                 Arity =:= 1,
                 is_a_test(Function)].


stop_applications() ->
    ?FUNCTION_NAME([pgec, pgmp, mcd, resp]).


stop_applications([Application | Applications]) ->
    application:stop(Application),
    ?FUNCTION_NAME(Applications);

stop_applications([]) ->
    ok.


pbe(#{sql := SQL, args := Parameters}) ->
    maybe
        [{parse_complete,[]}] ?= pgmp_connection_sync:parse(#{sql => SQL}),
        [{bind_complete, []}] ?= pgmp_connection_sync:bind(#{args => Parameters}),
        pgmp_connection_sync:execute(#{})
    end.
