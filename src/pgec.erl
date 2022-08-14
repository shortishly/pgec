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


-module(pgec).


-export([get_env/1]).
-export([priv_consult/1]).
-export([priv_dir/0]).
-export([start/0]).


start() ->
    application:ensure_all_started(?MODULE).


priv_dir() ->
    code:priv_dir(?MODULE).


priv_consult(Filename) ->
    phrase_file:consult(filename:join(priv_dir(), Filename)).


get_env(Par) ->
    application:get_env(?MODULE, Par).
