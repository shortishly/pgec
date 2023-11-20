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


-module(pgec_storage_backfill).


-export([callback_mode/0]).
-export([handle_event/4]).
-import(pgec_statem, [nei/1]).
-import(pgec_storage_common, [pt/1]).
-include("pgec_storage.hrl").
-include_lib("kernel/include/logger.hrl").


callback_mode() ->
    handle_event_function.


handle_event({call, _}, _, _, _) ->
    {keep_state_and_data, postpone};

handle_event(internal, {missing, Detail}, _, Data) ->
    BK = #{bucket => <<"pgec/mapping">>,
           key => pt(Detail)},
    {next_state,
     {waiting_for, BK},
     Data,
     nei({get, BK})};

handle_event(internal,
             {response,
              #{reply := {ok, Value},
                label := #{key := Key,
                           bucket := Bucket}}} = EventContent,
             {waiting_for,
              #{key := Key,
                bucket := Bucket}} = State,
             #{mappings := Mappings} = Data) ->
    ?LOG_DEBUG(#{event_content => EventContent,
                 state => State,
                 data => Data}),
    ets:insert(Mappings, {Key, Value}),
    {next_state,
     ready,
     Data,
     pop_callback_module};

handle_event(EventType, EventContent, State, Data) ->
    pgec_storage_common:handle_event(EventType,
                                     EventContent,
                                     State,
                                     Data).
