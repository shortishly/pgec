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
-export([options/1]).
-export([timeout/1]).


timeout(no_members = Name) ->
    envy(to_integer_or_atom, [Name, timeout], timer:seconds(1));

timeout(Name) ->
    envy(to_integer_or_atom, [Name, timeout], infinity).


http(port = Name) ->
    envy(to_integer, [?FUNCTION_NAME, Name], 80).


options(M) ->
    case debug_options(M, [log, trace]) of
        [] ->
            [];

        Options ->
            [{debug, Options}]
    end.


debug_options(M, Options) ->
    ?FUNCTION_NAME(M, Options, []).


debug_options(M, [log = Option | Options], A) ->
    case envy(to_boolean, [M, Option], false) of
        true ->
            try
                ?FUNCTION_NAME(
                   M,
                   Options,
                   [{log, envy(to_integer, [M, Option, n])} | A])
            catch
                error:badarg ->
                    ?FUNCTION_NAME(
                       M,
                       Options,
                       [log | A])

            end;

        false ->
            ?FUNCTION_NAME(M, Options, A)
    end;

debug_options(M, [Option | Options], A) ->
    ?FUNCTION_NAME(
       M,
       Options,
       [Option || envy(to_boolean, [M, Option], false)] ++ A);

debug_options(_, [], A) ->
    A.


envy(To, Names) ->
    case envy:get_env(pgec, pgmp_util:snake_case(Names), [os_env, app_env]) of
        undefined ->
            error(badarg, [To, Names]);

        Value ->
            any:To(Value)
    end.


envy(To, Names, Default) ->
    try
        envy:To(pgec, pgec_util:snake_case(Names), default(Default))

    catch
        error:badarg ->
            Default
    end.


default(Default) ->
    %% Enable all configuration to be overriden by OS environment
    %% variables, very useful for Docker.
    [os_env, app_env, {default, Default}].
