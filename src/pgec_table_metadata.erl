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


-module(pgec_table_metadata).


-export([callback_mode/0]).
-export([handle_event/4]).
-export([init/1]).
-export([start_link/1]).
-export([terminate/3]).
-import(pgec_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


start_link(Arg) ->
    gen_statem:start_link(?MODULE,
                          [Arg],
                          envy_gen:options(?MODULE)).


callback_mode() ->
    handle_event_function.


init([Arg]) ->
    {ok,
     ready,
     Arg#{requests => gen_statem:reqids_new()},
     [nei(join), nei(get_members)]}.


handle_event({call, _},
             {notify, #{action := progress} = Notification},
             ready,
             _) ->
    ?LOG_DEBUG(#{notification => Notification}),
    keep_state_and_data;

handle_event({call, _}, {notify, Notification}, ready, _) ->
    ?LOG_DEBUG(#{notification => Notification}),
    {keep_state_and_data, nei(get_members)};

handle_event(internal, join, _, #{publication := Publication}) ->
    pgec_pg:join([pgec_replica, Publication, notifications]),
    keep_state_and_data;

handle_event(internal, get_members, _, #{publication := Publication}) ->
    {keep_state_and_data,
     nei({get_members, pgec_pg:get_members([pgec_replica, Publication])})};

handle_event(internal, {get_members, []}, _, _) ->
    {keep_state_and_data, pgec_statem:generic_timeout(no_members)};

handle_event({timeout, no_members}, no_members, _, _) ->
    {keep_state_and_data, nei(get_members)};

handle_event(internal, {get_members, [Member]}, _, _) ->
    {keep_state_and_data, nei({when_ready, Member})};

handle_event(internal, {when_ready, Member}, _, _) ->
    {keep_state_and_data,
     nei({send_request, when_ready, #{server_ref => Member}})};

handle_event(internal, {member, Member}, _, _) ->
    {keep_state_and_data,
     nei({send_request,
          metadata,
          #{server_ref => Member}})};

handle_event(
  internal,
  {response,
   #{label := #{f := when_ready,
                server_ref := ServerRef},
     reply := ok}},
  ready,
  _) ->
    {keep_state_and_data,
     nei({send_request, metadata, #{server_ref => ServerRef}})};

handle_event(
  internal,
  {response, #{label := #{f := metadata}, reply := Metadata}},
  _,
  #{publication := Publication}) ->
    Types = pgmp_types:cache(pgec_util:db()),

    ets:insert(
      pgec_metadata,
      maps:fold(
        fun
            ({Namespace, Name},
             #{columns := Columns, oids := OIDs} = Data,
             A) ->
                [{{Publication, Name},
                  Data#{coids => maps:map(
                                   fun
                                       (_, OID) ->
                                           maps:get(OID, Types)
                                   end,
                                   maps:from_list(lists:zip(Columns, OIDs))),
                        namespace => Namespace}} | A]
        end,
        [],
        Metadata)),
    keep_state_and_data;

handle_event(internal,
             {send_request, F, #{server_ref := ServerRef} = A},
             _,
             #{requests := Requests} = Data) ->
    {keep_state,
     Data#{requests := pgec_replica:F(A#{requests => Requests,
                                         label => #{f => F,
                                                    server_ref => ServerRef}})}};

handle_event(info, Msg, _, #{requests := Existing} = Data) ->
    case gen_statem:check_response(Msg, Existing, true) of
        {{reply, Reply}, Label, Updated} ->
            {keep_state,
             Data#{requests := Updated},
             nei({response, #{label => Label, reply => Reply}})};

        {{error, {Reason, _}}, _, UpdatedRequests} ->
            {stop, Reason, Data#{requests := UpdatedRequests}}
    end.


terminate(_Reason, _State, #{publication := Publication}) ->
    pgec_pg:leave([pgec_replica, Publication, notifications]);

terminate(_Reason, _State, _Data) ->
    ok.
