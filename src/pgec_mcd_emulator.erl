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


-module(pgec_mcd_emulator).


-export([expire/1]).
-export([flush_all/1]).
-export([init/1]).
-export([recv/1]).
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


init([]) ->
    {ok, #{}}.

recv(#{message := #{header := _}}) ->
    stop;

recv(#{message := #{meta := _}}) ->
    stop;

recv(#{message := #{command := _}} = Arg) ->
    ?LOG_DEBUG(#{arg => Arg}),
    pgec_mcd_emulator_text:recv(Arg).


expire(_) ->
    ok.


flush_all(_) ->
    ok.
