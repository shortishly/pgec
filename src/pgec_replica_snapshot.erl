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


-module(pgec_replica_snapshot).


-export([callback_mode/0]).
-export([handle_event/4]).
-import(pgec_replica_common, [metadata/4]).
-import(pgec_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


callback_mode() ->
    handle_event_function.


handle_event({call, _} = EventType,
             EventContent,
             State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {keep_state_and_data, postpone};

handle_event(internal = EventType,
             {response, #{reply := [{command_complete, 'begin'}]}} = EventContent,
             query = State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {next_state, unready, Data};

handle_event(internal = EventType,
             {response, #{reply := [{command_complete, commit}]}} = EventContent,
             State,
             #{config := #{publication := Publication},
               stream := Stream} = Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {next_state,
     ready,
     maps:without([stream], Data),
     [pop_callback_module,

      %% this reply will start streaming replication with pgmp.
      {reply, Stream, ok},

      nei({notify,
           #{action => progress,
             status => completed,
             activity => ?MODULE}}),

      %% inform storage that the publication is now ready, enabling
      %% read requests.
      nei({storage_request,
           #{action => ready,
             publication => Publication}})]};

handle_event(internal = EventType,
             {response,
              #{label := sync_publication_tables = Label,
                reply := [{parse_complete, []}]}} = EventContent,
             parse = State,
             #{config := #{publication := Publication}} = Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {next_state,
     unready,
     Data,
     nei({bind, #{label => Label, args => [Publication]}})};

handle_event(internal = EventType,
             {response,
              #{label := sync_publication_tables = Label,
                reply := [{bind_complete, []}]}} = EventContent,
             bind = State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {next_state,
     unready,
     Data,
     nei({execute, #{label => Label}})};

handle_event(internal = EventType,
             {response, #{label := sync_publication_tables,
                          reply := [{command_complete, {select, 0}}]}} = EventContent,
             execute = State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    %% There are no publication tables to sync, initiate streaming
    %% replication.
    %%
    {next_state, unready, Data, nei(commit)};

handle_event(internal = EventType,
             {response, #{label := sync_publication_tables,
                          reply := [{row_description, Columns} | T]}} = EventContent,
             execute = State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {command_complete, {select, _}} = lists:last(T),
    {next_state,
     unready,
     Data,
     nei({fetch,
          lists:map(
            fun
                ({data_row, Values}) ->
                    maps:from_list(lists:zip(Columns, Values))
            end,
            lists:droplast(T))})};

handle_event(internal = EventType,
             {fetch, []} = EventContent,
             unready = State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {keep_state_and_data, nei(commit)};

handle_event(internal = EventType,
             {fetch, _} = Label = EventContent,
             unready = State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {keep_state_and_data,
     nei({parse,
          #{label => Label,
            sql => <<"select i.indkey from pg_catalog.pg_index i"
                     ", pg_catalog.pg_namespace n"
                     ", pg_catalog.pg_class c"
                     " where "
                     "i.indisprimary"
                     " and "
                     "i.indrelid = c.oid"
                     " and "
                     "c.relnamespace = n.oid"
                     " and "
                     "n.nspname = $1"
                     " and "
                     "c.relname = $2">>}})};

handle_event(internal = EventType,
             {response,
              #{label := {fetch, [#{<<"schemaname">> := Schema, <<"tablename">> := Table} | _]} = Label,
                reply := [{parse_complete, []}]}} = EventContent,
             parse = State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {next_state,
     unready,
     Data,
     nei({bind,
          #{label => Label,
            args => [Schema, Table]}})};

handle_event(internal = EventType,
             {response,
              #{label := {fetch, _} = Label,
                reply := [{bind_complete, []}]}} = EventContent,
             bind = State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {next_state,
     unready,
     Data,
     nei({execute, #{label => Label}})};

handle_event(
  internal = EventType,
  {response,
   #{label := {fetch, [#{<<"tablename">> := _}  = Publication | T]},
     reply := [{command_complete, {select, 0}}]}} = EventContent,
  execute = State,
  Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    ?LOG_WARNING(
       #{publication => Publication,
         reason => "no primary key found"}),
    {next_state, unready, Data, nei({fetch, T})};

handle_event(
  internal = EventType,
  {response,
   #{label := {fetch,
               [#{<<"schemaname">> := Namespace,
                  <<"tablename">> := Name} = Info | T]},
     reply := [{row_description, [<<"indkey">>]},
               {data_row, [Key]},
               {command_complete, {select, 1}}]}} = EventContent,
  execute = State,
  #{metadata := Metadata} = Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {next_state,
     unready,
     Data#{metadata := metadata(
                         {Namespace, Name},
                         keys,
                         Key,
                         Metadata)},
     [nei({parse,
           #{label => {table, #{namespace => Namespace, name => Name}},
             sql => pub_fetch_sql(Info)}}),
      nei({fetch, T})]};

handle_event(internal = EventType,
             {response,
              #{label := {table, _} = Label,
                reply := [{parse_complete, []}]}} = EventContent,
             parse = State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {next_state,
     unready,
     Data,
     nei({describe, #{type => $S, label => Label}})};

handle_event(internal = EventType,
             {response,
              #{label := {table, _} = Label,
                reply := [{bind_complete, []}]}} = EventContent,
             bind = State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {next_state,
     unready,
     Data,
     nei({execute,
          #{label => Label,
            max_rows => pgmp_config:replication(
                          logical,
                          max_rows)}})};

handle_event(internal = EventType,
             {response,
              #{label := {table,
                          #{namespace := Namespace, name := Name}} = Label,
                reply := [{parameter_description,[]},
                          {row_description, Columns}]}} = EventContent,
             describe = State,
             #{metadata := Metadata, config := Config} = Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {next_state,
     unready,
     Data#{metadata := metadata(
                         {Namespace, Name},
                         columns,
                         [FieldName || #{field_name := FieldName} <- Columns],
                         metadata(
                           {Namespace, Name},
                           oids,
                           [OID || #{type_oid := OID} <- Columns],
                           Metadata))},
     nei({bind, #{label => Label, result => column_format(Config, Columns)}})};

handle_event(internal = EventType,
             {response, #{label := {table, #{namespace := Namespace, name := Name}},
                          reply := [{command_complete, {select, 0}}]}} = EventContent,
             execute = State,
             #{metadata := Metadata,
               config := #{publication := Publication}} = Data)
  when is_map_key({Namespace, Name}, Metadata) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    #{{Namespace, Name} := Mapping} = Metadata,
    {next_state,
     unready,
     Data,
     nei({storage_request,
          maps:merge(
            #{action => table_map,
              publication => Publication,
              schema => Namespace,
              table => Name},
            Mapping)})};

handle_event(internal = EventType,
             {response,
              #{label := {table,
                          #{namespace := Namespace,
                            name := Name}} = Label,
                          reply := [{row_description, Columns} | T]}} = EventContent,
             execute = State,
             #{metadata := Metadata,
               config := #{publication := Publication}} = Data)
  when is_map_key({Namespace, Name}, Metadata) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),

    #{{Namespace, Name} := #{keys := Keys, oids := OIDs}} = Metadata,

    {next_state,
     unready,
     Data#{metadata := metadata(
                         {Namespace, Name},
                         columns,
                         Columns,
                         Metadata)},
     [nei({storage_request,
           #{action => table_map,
             publication => Publication,
             schema => Namespace,
             table => Name,
             keys => Keys,
             oids => OIDs,
             columns => Columns}}) |
      lists:foldr(
        fun
            ({data_row, Row}, A) ->
                [nei({storage_request,
                      #{action => write,
                        publication => Publication,
                        schema => Namespace,
                        table => Name,
                       row => list_to_tuple(Row)}}) | A]
        end,
        case lists:last(T) of
            {command_complete, {select, _}} ->
                [];

            {portal_suspended, _} ->
                [nei({execute,
                      #{label => Label,
                        max_rows => pgmp_config:replication(
                                      logical,
                                      max_rows)}})]
        end,
        lists:droplast(T))]};

handle_event(internal = EventType,
             {response, #{reply := [{command_complete, set}]}} = EventContent,
             query = State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),

    {next_state, unready, Data};

handle_event(internal = EventType,
             sync_publication_tables = Label = EventContent,
             State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {keep_state_and_data,
     nei({parse,
          #{label => Label,
            sql => <<"select * from pg_catalog.pg_publication_tables "
                     "where pubname = $1">>}})};

handle_event(internal = EventType,
             begin_transaction = Label = EventContent,
             State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {keep_state_and_data,
     nei({query,
          #{label => Label,
            sql => <<"begin isolation level repeatable read">>}})};

handle_event(internal = EventType,
             commit = Label = EventContent,
             State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {keep_state_and_data,
     nei({query, #{label => Label, sql => <<"commit">>}})};

handle_event(internal = EventType,
             {set_transaction_snapshot = Label, Id} = EventContent,
             State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {keep_state_and_data,
     nei({query,
          #{label => Label,
            sql => io_lib:format(
                     "SET TRANSACTION SNAPSHOT '~s'",
                     [Id])}})};

handle_event(internal = EventType,
             {Action, Arg} = EventContent,
             unready = State,
             #{requests := Requests, config := Config} = Data)
  when Action == query;
       Action == parse;
       Action == bind;
       Action == describe;
       Action == execute ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {next_state,
     Action,
     Data#{requests := pgmp_connection:Action(
                         Arg#{requests => Requests,
                              server_ref => pgmp_connection:server_ref(
                                              Config)})}};

handle_event(internal = EventType,
             {Action, _} = EventContent,
             State,
             Data)
  when Action == query;
       Action == parse;
       Action == bind;
       Action == describe;
       Action == execute ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {keep_state_and_data, postpone};

handle_event(internal = EventType,
             {fetch, _} = EventContent,
             State,
             Data) ->
    ?LOG_DEBUG(#{event_type => EventType,
                 event_content => EventContent,
                 state => State,
                 data => Data}),
    {keep_state_and_data, postpone};

handle_event(info, Msg, _, #{requests := Existing} = Data) ->
    case gen_statem:check_response(Msg, Existing, true) of
        {{reply, Reply}, Label, Updated} ->
            {keep_state,
             Data#{requests := Updated},
             nei({response, #{label => Label, reply => Reply}})};

        {{error, {Reason, ServerRef}}, Label, UpdatedRequests} ->
                {stop,
                 #{reason => Reason,
                   server_ref => ServerRef,
                   label => Label},
                 Data#{requests := UpdatedRequests}}
    end;

handle_event(Type, Content, State, Data) ->
    pgec_replica_common:handle_event(Type, Content, State, Data).


pub_fetch_sql(#{<<"schemaname">> := Schema,
                <<"tablename">> := Table} = Publication) ->
    ?LOG_DEBUG(#{publication => Publication}),
    ["select ",
     pub_fetch_columns(Publication),
     " from ",
     Schema,
     ".",
     Table,
     case maps:find(<<"rowfilter">>, Publication) of
        {ok, RowFilter} when RowFilter /= null ->
             [" where ", RowFilter];

        _Otherwise ->
             []
     end].


pub_fetch_columns(#{<<"attnames">> := Attributes}) ->
    ?LOG_DEBUG(#{attributes => Attributes}),
    lists:join(",", Attributes);

pub_fetch_columns(#{}) ->
    "*".

column_format(Config, Columns) ->
    ?LOG_DEBUG(#{config => Config, columns => Columns}),
    Types = pgmp_types:cache(Config),
    case lists:any(
           fun
               (OID) ->
                   #{<<"typreceive">> := R,
                     <<"typsend">> := S} = maps:get(OID, Types),
                   R == <<"-">> orelse S == <<"-">>
           end,
           [OID || #{type_oid := OID} <- Columns]) of

        true ->
            text;

        false ->
            binary
    end.
