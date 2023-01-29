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


-module(pgec_telemetry).


-export([callback_mode/0]).
-export([init/1]).
-export([start_link/0]).
-export([start_link/1]).
-export([terminate/3]).


start_link() ->
    ?FUNCTION_NAME(#{}).


start_link(Arg) ->
    gen_statem:start_link(?MODULE, [Arg], envy_gen:options(?MODULE)).


init([_Arg]) ->
    try
        M = pgec_config:telemetry(module),
        F = pgec_config:telemetry(function),

        Telemetry = fun M:F/4,

        case telemetry:attach_many(
               ?MODULE,
               phrase_file:consult(pgec_config:telemetry(event_names)),
               fun M:F/4,
               pgec_config:telemetry(config)) of

            ok ->
                {ok, ready, #{telemetry => Telemetry}, hibernate};

            {error, Reason} ->
                {stop, Reason}
        end

    catch
        error:badarg ->
            ignore
    end.


callback_mode() ->
    handle_event_function.


terminate(_Reason, _State, _Data) ->
    telemetry:detach(?MODULE).
