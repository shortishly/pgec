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


-module(pgec_storage_sync).


-export([delete/1]).
-export([keys/1]).
-export([metadata/1]).
-export([position/1]).
-export([position_update/1]).
-export([read/1]).
-export([table_map/1]).
-export([write/1]).


keys(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


position(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


position_update(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


read(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


delete(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


write(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


table_map(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


metadata(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


receive_response(Function, Arg) ->
    M = pgec_storage,
    case gen_statem:receive_response(
           M:Function(
             maps:merge(
               #{server_ref => M},
               Arg))) of
        {reply, Reply} ->
            Reply;

        {error, _} = Error ->
            Error
    end.
