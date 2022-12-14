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


-module(pgec_statem).


-export([cancel_generic_timeout/1]).
-export([generic_timeout/1]).
-export([generic_timeout/2]).
-export([nei/1]).


nei(Event) ->
    {next_event, internal, Event}.


generic_timeout(Name) ->
    ?FUNCTION_NAME(Name, pgec_config:timeout(Name)).


cancel_generic_timeout(Name) ->
    generic_timeout(Name, infinity).


generic_timeout(Name, Timeout) ->
    {{timeout, Name}, Timeout, Name}.
