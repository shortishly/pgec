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


-module(pgec_replica).


-export([begin_transaction/1]).
-export([callback_mode/0]).
-export([commit/1]).
-export([delete/1]).
-export([handle_event/4]).
-export([init/1]).
-export([insert/1]).
-export([lsn/1]).
-export([metadata/1]).
-export([snapshot/1]).
-export([start_link/1]).
-export([terminate/3]).
-export([truncate/1]).
-export([update/1]).
-export([when_ready/1]).
-import(pgec_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


start_link(Arg) ->
    gen_statem:start_link(?MODULE, [Arg], envy_gen:options(?MODULE)).


snapshot(Arg) ->
    send_request(?FUNCTION_NAME, [id], Arg).


lsn(Arg) ->
    send_request(?FUNCTION_NAME, [], Arg).


begin_transaction(Arg) ->
    send_request(?FUNCTION_NAME,
                 [commit_timestamp, final_lsn, xid, x_log],
                 Arg).


commit(Arg) ->
    send_request(?FUNCTION_NAME,
                 [commit_lsn, commit_timestamp, end_lsn, x_log],
                 Arg).


insert(Arg) ->
    send_request(?FUNCTION_NAME, [relation, tuple, x_log], Arg).


update(Arg) ->
    send_request(?FUNCTION_NAME, [relation, tuple, x_log], Arg).


delete(Arg) ->
    send_request(?FUNCTION_NAME, [relation, tuple, x_log], Arg).


truncate(Arg) ->
    send_request(?FUNCTION_NAME, [relations, x_log], Arg).


metadata(Arg) ->
    send_request(?FUNCTION_NAME, [], Arg).


send_request(Action, Keys, Arg) ->
    send_request(
      maps:without(
        Keys,
        Arg#{request => {Action, maps:with(Keys, Arg)}})).


when_ready(Arg) ->
    send_request(
      maps:merge(
        Arg,
        #{request => ?FUNCTION_NAME})).


send_request(#{label := _} = Arg) ->
    pgec_statem:send_request(Arg);

send_request(Arg) ->
    pgec_statem:send_request(Arg#{label => ?MODULE}).


init([Arg]) ->
    process_flag(trap_exit, true),
    {ok,
     unready,
     #{requests => gen_statem:reqids_new(),
       config => Arg,
       monitors => #{},
       metadata => #{}},
     nei(join)}.


callback_mode() ->
    handle_event_function.


handle_event(internal = EventType,
             join = EventContent,
             State,
             #{config := #{scope := Scope, publication := Publication}} = Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    pg:join(Scope, [?MODULE, Publication], self()),
    keep_state_and_data;

handle_event({call, From} = EventType,
             {metadata, #{}} = EventContent,
             ready = State,
             #{metadata := Metadata} = Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {keep_state_and_data, {reply, From, Metadata}};

handle_event({call, From} = EventType,
             when_ready = EventContent,
             ready = State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {keep_state_and_data, {reply, From, ok}};

handle_event({call, _} = EventType,
             when_ready = EventContent,
             State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {keep_state_and_data, postpone};

handle_event({call, From} = EventType,
             {begin_transaction, _} = EventContent,
             State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {keep_state_and_data, {reply, From, ok}};

handle_event({call, From} = EventType,
             {commit, #{end_lsn := LSN}} = EventContent,
             State,
             #{config := #{publication := Publication}} = Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {keep_state_and_data,
     [{reply, From, ok},
      nei({storage_request,
           #{action => position_update,
             publication => Publication,
             position => LSN}})]};

handle_event({call, From} = EventType,
             {Action,
              #{relation := #{namespace := Namespace, name := Name} = Relation,
                tuple := Tuple}} = EventContent,
             State,
             #{metadata := Metadata,
               config := #{publication := Publication}} = Data)
  when Action == insert;
       Action == update;
       Action == delete ->

    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),

    case Metadata of
        #{{Namespace, Name} := #{state := dropped}} ->
            {keep_state_and_data, {reply, From, ok}};

        #{{Namespace, Name} := _} ->
            {keep_state_and_data,
             [{reply, From, ok},

              nei({storage_request,
                   #{action => storage_action(Action),
                     publication => Publication,
                     schema => Namespace,
                     table => Name,
                     row => Tuple}}),

              nei({change,
                   #{namespace => Namespace,
                     action => Action,
                     row => Tuple,
                     name => Name}})]};
        #{} ->
            {next_state,
             unready,
             Data,
             [{push_callback_module, pgec_replica_backfill},
              nei({relation, Relation}),
              postpone]}
    end;

handle_event({call, From} = EventType,
             {truncate, #{relations := Relations}} = EventContent,
             State,
             #{metadata := Metadata,
               config := #{publication := Publication}} = Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    case lists:filter(
           fun
               (#{namespace := Namespace, name := Name}) ->
                   case maps:find({Namespace, Name}, Metadata) of
                       {ok, _} ->
                           false;

                       error ->
                           true
                   end
           end,
           Relations) of

        [] ->
            {keep_state_and_data,
             lists:foldl(
               fun
                   (#{namespace := Namespace, name := Name}, A) ->
                       [nei({storage_request,
                             #{action => truncate,
                               publication => Publication,
                               namespace => Namespace,
                               table => Name}}) | A]
               end,
               [{reply, From, ok}],
               Relations)};

        [Relation | _] ->
            {keep_state_and_data,
             [{push_callback_module, pgec_replica_backfill},
              nei({relation, Relation}),
              postpone]}
    end;

handle_event({call, Stream} = EventType,
             {snapshot, #{id := Id}} = EventContent,
             State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    ?LOG_INFO(#{snapshot => Id}),
    {keep_state,
     Data#{stream => Stream},
     [nei(storage),
      {push_callback_module, pgec_replica_snapshot},
      nei(begin_transaction),
      nei({set_transaction_snapshot, Id}),
      nei(sync_publication_tables)]};

handle_event({call, Stream},
             {lsn, LSN},
             _,
             #{config := #{publication := Publication}}) ->
    ?LOG_INFO(#{lsn => LSN}),
    {keep_state_and_data,
     [nei(storage),

      nei({storage_request,
           #{action => position,
             publication => Publication,
             from => Stream}}),

      %% inform storage that the publication is now ready, enabling
      %% read requests.
      nei({storage_request,
           #{action => ready,
             publication => Publication}})]};

handle_event(info = EventType,
             {'DOWN', _, process, _, shutdown = Reason} = EventContent,
             State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {stop, Reason};

handle_event(Type, Content, State, Data) ->
    pgec_replica_common:handle_event(Type, Content, State, Data).


terminate(_Reason,
          _State,
          #{config := #{scope := Scope, publication := Publication}}) ->
    pg:leave(Scope, [?MODULE, Publication], self()).


storage_action(Action) when Action == insert;
                            Action == update ->
    write;
storage_action(delete = Action) ->
    Action.
