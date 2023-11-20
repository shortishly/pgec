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
-export([send_request/1]).


-type request_id() :: gen_statem:request_id().
-type request_id_collection() :: gen_statem:request_id_collection().
-type server_ref() :: gen_statem:server_ref().

-type collection_req() :: #{server_ref := server_ref(),
                            request := any(),
                            label := any(),
                            requests := request_id_collection()}.

-type id_req() :: #{server_ref := server_ref(),
                    request := any()}.

-spec send_request(collection_req()) -> request_id_collection();
                  (id_req()) -> request_id().

send_request(#{server_ref := ServerRef,
               request := Request,
               label := Label,
               requests := Requests}) ->
    gen_statem:send_request(ServerRef, Request, Label, Requests);

send_request(#{requests := _} = Arg) ->
    error(badarg, [Arg]);

send_request(#{server_ref := ServerRef, request := Request}) ->
    gen_statem:send_request(ServerRef, Request).


-spec nei(any()) -> {next_event, internal, any()}.

nei(Event) ->
    {next_event, internal, Event}.


generic_timeout(Name) ->
    ?FUNCTION_NAME(Name, pgec_config:timeout(Name)).


cancel_generic_timeout(Name) ->
    generic_timeout(Name, infinity).


generic_timeout(Name, Timeout) ->
    {{timeout, Name}, Timeout, Name}.
