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


-module(pgec_util).


-export([application_name/0]).
-export([db/0]).
-export([scope/0]).
-export([snake_case/1]).
-export([tl_snake_case/1]).


snake_case([_ | _] = Labels) ->
    list_to_atom(lists:concat(lists:join("_", Labels))).


split_on_snake_case(Name) ->
    string:split(atom_to_list(Name), "_").

tl_snake_case(Name) ->
    case split_on_snake_case(Name) of
        [_] ->
            Name;

        Names ->
            snake_case(tl(Names))
    end.


scope() ->
    #{scope := Scope} = db(),
    Scope.


application_name() ->
    #{application_name := ApplicationName} = db(),
    ApplicationName.


db() ->
    ?FUNCTION_NAME(maps:next(maps:iterator(pgmp_dbs:all()))).

db({_, Configuration, _}) ->
    Configuration.
