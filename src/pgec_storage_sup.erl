%% Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
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


-module(pgec_storage_sup).


-behaviour(supervisor).
-export([init/1]).
-export([start_link/0]).
-import(pgec_sup, [worker/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok,
     {#{auto_shutdown => any_significant},
      children()}}.


children() ->
    [worker(#{m => leveled_bookie,
              f => book_start,
              id => leveled,
              args => [lists:map(
                         fun
                             (Name) ->
                                 {Name, pgec_config:leveled(Name)}
                         end,
                         [root_path,
                          log_level])]}),
     worker(pgec_storage)].
