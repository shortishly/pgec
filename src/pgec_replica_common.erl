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


-module(pgec_replica_common).


-export([handle_event/4]).
-export([metadata/4]).
-export([table_name/3]).
-import(pgec_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


handle_event(internal,
             {notify, Notification},
             _,
             #{config := #{scope := Scope,
                           publication := Publication}}) ->
    {keep_state_and_data,
     nei({notify,
          pg:get_members(
            Scope,
            [pgec_replica,
             Publication,
             notifications]),
          Notification})};

handle_event(internal, {notify, [], _}, _, _) ->
    keep_state_and_data;

handle_event(internal,
             {notify, [Recipient | Recipients], Arg},
             _,
             #{requests := Requests} = Data) ->
    {keep_state,
     Data#{requests := gen_statem:send_request(
                         Recipient,
                         {notify, Arg},
                         #{notify => Recipient},
                         Requests)},
     nei({notify, Recipients, Arg})};

handle_event(internal,
             {change,
              #{namespace := Namespace,
                action := Action,
                row := Row,
                name := Name}},
             _,
             #{metadata := Metadata,
               config := #{scope := Scope,
                           publication := Publication}})
  when is_map_key({Namespace, Name}, Metadata) ->
    case pg:get_members(
           Scope,
           #{m => pgec_replica,
             publication => Publication,
             name => Name}) of

        [] ->
            keep_state_and_data;

        Members ->
            #{{Namespace, Name} := #{keys := Keys}} = Metadata,

            ChangedKeys = key(Row, Keys),
            Relation = table_name(Publication, Namespace, Name),

            lists:foreach(
              fun
                  (Member) ->
                      Member ! {notify,
                                #{publication => Publication,
                                  namespace => Namespace,
                                  name => Name,
                                  relation => Relation,
                                  keys => ChangedKeys,
                                  action => Action}}
              end,
              Members),
            keep_state_and_data
    end;

handle_event(
  internal,
  {storage_request, #{action := Action} = Arg},
  _,
  #{storage := Storage, requests := Requests} = Data) ->
    ?LOG_DEBUG(#{arg => Arg}),
    {keep_state,
     Data#{requests := pgec_storage:Action(
                         maps:merge(
                           #{server_ref => Storage,
                             label => Arg,
                             requests => Requests},
                           maps:without(
                             [action], Arg)))}};

handle_event(internal, storage, _, #{monitors := Monitors} = Data) ->
    case pgec_sup:get_child(pgec_storage_sup, storage) of
        {_, Storage, worker, _} when is_pid(Storage) ->
            {keep_state,
             Data#{storage => Storage,
                   monitors := Monitors#{Storage => erlang:monitor(
                                                      process,
                                                      Storage)}},
             nei(available)};

        {_, _, _, _} = Reason ->
            {stop, Reason};

        false ->
            {stop, no_storage}
    end;

handle_event(internal,
             available,
             _,
             #{config := #{publication := Publication}}) ->
    %% inform storage that this publication is available, but not
    %% ready yet.
    {keep_state_and_data,
     nei({storage_request,
          #{action => available,
            publication => Publication}})};

handle_event(internal,
             {response,
              #{label := #{from := From},
                reply := Reply}},
             _,
             _) ->
    {keep_state_and_data, {reply, From, Reply}};

handle_event(internal,
             {response,
              #{label := #{action := Action},
                reply := Reply}},
             _,
             _) when Action == table_map;
                     Action == delete;
                     Action == truncate;
                     Action == ready;
                     Action == available;
                     Action == position_update;
                     Action == write ->
    {keep_state_and_data,
     nei({telemetry,
          storage,
          #{count => 1},
          #{action => Action, reply => Reply}})};

handle_event(info, Msg, _, #{requests := Existing} = Data) ->
    case gen_server:check_response(Msg, Existing, true) of
        {{reply, Reply}, Label, Updated} ->
            {keep_state,
             Data#{requests := Updated},
             [nei({telemetry,
                   reqids_size,
                   #{value => gen_server:reqids_size(Updated)}}),
                  nei({response, #{label => Label, reply => Reply}})]};

        {{error, {Reason, _}}, _, UpdatedRequests} ->
            {stop, Reason, Data#{requests := UpdatedRequests}}
    end;

handle_event(internal,
             {telemetry, EventName, Measurements},
             _,
             _) ->
    {keep_state_and_data,
     nei({telemetry, EventName, Measurements, #{}})};

handle_event(internal,
             {telemetry, EventName, Measurements, Metadata},
             _,
             _) when is_atom(EventName) ->
    {keep_state_and_data,
     nei({telemetry, [EventName], Measurements, Metadata})};

handle_event(internal,
             {telemetry, EventName, Measurements, Metadata},
             _,
             _) ->
    ok = telemetry:execute([pgec, replica] ++ EventName,
                           Measurements,
                           Metadata),
    keep_state_and_data.


metadata({_Namespace, _Name} = Relation, Key,  Value, Metadata) ->
    case Metadata of
        #{Relation := TMD} ->
            Metadata#{Relation := TMD#{Key => Value}};

        #{} ->
            Metadata#{Relation => #{Key => Value}}
    end.

key(Tuple, [Primary]) ->
    [element(Primary, Tuple)];

key(Tuple, Composite) when length(Composite) > 1 ->
    list_to_tuple([element(Position, Tuple) || Position <- Composite]).


table_name(Publication, Schema, Table) ->
    pgmp_util:snake_case(
      lists:filtermap(
        fun
            ([]) ->
                false;

            ([Value]) ->
                {true, Value};

            (_) ->
                true
        end,
        [pgmp_config:rep_log_ets(prefix_table_name),
         [binary_to_list(Publication) || pgmp_config:enabled(
                                           rep_log_ets_pub_in_table_name)],
         [binary_to_list(Schema) || pgmp_config:enabled(
                                      rep_log_ets_schema_in_table_name)],
         binary_to_list(Table)])).
