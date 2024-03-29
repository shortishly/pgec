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


-module(pgec_pg).


-export([get_members/1]).
-export([join/1]).
-export([leave/1]).
-export([which_groups/0]).
-include_lib("kernel/include/logger.hrl").


leave(Group) ->
    ?LOG_DEBUG(#{group => Group}),
    pg:leave(pgec_util:scope(), Group, self()).


join(Group) ->
    ?LOG_DEBUG(#{group => Group}),
    pg:join(pgec_util:scope(), Group, self()).


get_members(Group) ->
    Scope = pgec_util:scope(),
    ?LOG_DEBUG(#{scope => Scope, group => Group}),
    pg:get_members(Scope, Group).


which_groups() ->
    Scope = pgec_util:scope(),
    ?LOG_DEBUG(#{scope => Scope}),
    pg:which_groups(Scope).
