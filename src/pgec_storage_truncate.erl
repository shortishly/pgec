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


-module(pgec_storage_truncate).


-export([callback_mode/0]).
-export([handle_event/4]).
-import(pgec_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


callback_mode() ->
    handle_event_function.


handle_event({call, _}, _, {truncate, _}, _) ->
    {keep_state_and_data, postpone};

handle_event(internal,
             {truncate = Action, #{bucket := Bucket}},
             {Action, Ref},
             _) ->
    {keep_state_and_data,
     nei({keys,
          #{bucket => Bucket,
            folder => fun
                          (_, Key, A) ->
                              [Key | A]
                      end,
            ref => Ref,
            accumulator => []}})};

handle_event(internal,
             {response,
              #{reply := {async, F},
                label := #{bucket := Bucket, ref := Ref}}},
             {truncate, Ref},
             #{previous := {ready, _} = Previous} = Data) ->
    {next_state,
     Previous,
     maps:without([previous], Data),
     lists:foldl(
       fun
           (Key, A) ->
               [nei({delete,
                     #{bucket => Bucket,
                       key => Key}}) | A]
       end,
       [pop_callback_module],
       F())};

handle_event(EventType, EventContent, State, Data) ->
    pgec_storage_common:handle_event(EventType,
                                     EventContent,
                                     State,
                                     Data).
