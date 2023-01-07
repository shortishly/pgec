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


-module(pgec_config).


-export([http/1]).
-export([timeout/1]).
-export([telemetry/1]).
-import(envy, [envy/1]).


timeout(no_members = Name) ->
    envy(#{caller => ?MODULE,
           type => integer_or_atom,
           names => [Name, timeout],
           default => timer:seconds(1)});

timeout(Name) ->
    envy(#{caller => ?MODULE,
           type => integer_or_atom,
           names => [Name, timeout],
           default => infinity}).


http(port = Name) ->
    envy(#{caller => ?MODULE,
           names => [?FUNCTION_NAME, Name],
           default => 80}).


telemetry(Name) when Name == module; Name == function ->
    envy(#{caller => ?MODULE,
           type => atom,
           names =>[?FUNCTION_NAME, Name]});

telemetry(event_names = Name) ->
    envy(#{caller => ?MODULE,
           names => [?FUNCTION_NAME, Name],
           default => filename:join(pgec:priv_dir(), "telemetry.terms")});

telemetry(config = Name) ->
    envy:get_env(pgec,
                 pgmp_util:snake_case([?FUNCTION_NAME, Name]),
                 [app_env, {default, []}]).
