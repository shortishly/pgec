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


-module(pgec_replica_backfill).


-export([callback_mode/0]).
-export([handle_event/4]).
-import(pgec_replica_common, [metadata/4]).
-import(pgec_replica_common, [table_name/3]).
-import(pgec_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


callback_mode() ->
    handle_event_function.


handle_event({call, _}, _, _, _) ->
    {keep_state_and_data, postpone};

handle_event(internal, {relation, _} = Label, _, _) ->
    {keep_state_and_data,
     nei({parse,
          #{label => Label,
             sql => <<"select * from pg_catalog.pg_publication_tables "
                     "where pubname = $1 and schemaname = $2 and tablename = $3">>}})};

handle_event(internal,
             {response,
              #{label := {relation, #{namespace := Namespace, name := Name}} = Label,
                reply := [{parse_complete, []}]}},
             parse,
             #{config := #{publication := Publication}} = Data) ->
    {next_state,
     unready,
     Data,
     nei({bind, #{label => Label, args => [Publication, Namespace, Name]}})};

handle_event(internal,
             {response,
              #{label := {relation, _} = Label,
                reply := [{bind_complete, []}]}},
             bind,
             Data) ->
    {next_state,
     unready,
     Data,
     nei({execute, #{label => Label}})};

handle_event(internal,
             {response, #{label := {relation, _},
                          reply := [{row_description, Columns} | T]}},
             execute,
             Data) ->
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

handle_event(internal,
             {Action, Arg},
             unready,
             #{config := Config, requests := Requests} = Data)
  when Action == query;
       Action == parse;
       Action == bind;
       Action == describe;
       Action == execute ->
    {next_state,
     Action,
     Data#{requests := pgmp_connection:Action(
                         Arg#{server_ref => pgmp_connection:server_ref(Config),
                              requests => Requests})}};

handle_event(internal,
             {fetch, []},
             unready,
             Data) ->
    {next_state,
     ready,
     Data,
     pop_callback_module};

handle_event(internal,
             {fetch, _} = Label,
             unready,
             _) ->
    {keep_state_and_data,
     nei({parse,
          #{label => Label,
            sql => <<"select i.indkey from pg_catalog.pg_index i"
                     ", pg_catalog.pg_namespace n"
                     ", pg_catalog.pg_class c"
                     " where "
                     "i.indrelid = c.oid"
                     " and "
                     "c.relnamespace = n.oid"
                     " and "
                     "n.nspname = $1"
                     " and "
                     "c.relname = $2">>}})};

handle_event(internal,
             {response,
              #{label := {fetch,
                          [#{<<"schemaname">> := Schema,
                             <<"tablename">> := Table} | _]} = Label,
                reply := [{parse_complete, []}]}},
             parse,
             Data) ->
    {next_state,
     unready,
     Data,
     nei({bind, #{label => Label, args => [Schema, Table]}})};

handle_event(internal,
             {response,
              #{label := {fetch, _} = Label,
                reply := [{bind_complete, []}]}},
             bind,
             Data) ->
    {next_state,
     unready,
     Data,
     nei({execute, #{label => Label}})};

handle_event(
  internal,
  {response,
   #{label := {fetch,
               [#{<<"schemaname">> := Namespace,
                  <<"tablename">> := Name} = Row | T]},
     reply := [{row_description, [<<"indkey">>]},
               {data_row, [Key]},
               {command_complete, {select, 1}}]}},
  execute,
  #{metadata := Metadata} = Data) ->
    {next_state,
     unready,
     Data#{metadata := metadata(
                         {Namespace, Name},
                         keys,
                         Key,
                         Metadata)},
     [nei({parse,
           #{label => {table,
                       #{namespace => Namespace,
                         name => Name}},
             sql => pub_fetch_sql(Row)}}),
      nei({fetch, T})]};

handle_event(internal,
             {response,
              #{label := {table, _} = Label,
                reply := [{parse_complete, []}]}},
             parse,
             Data) ->
    {next_state, unready, Data, nei({bind, #{label => Label}})};

handle_event(internal,
             {response,
              #{label := {table, _} = Label,
                reply := [{bind_complete, []}]}},
             bind,
             Data) ->
    {next_state, unready, Data, nei({describe, #{type => $P, label => Label}})};

handle_event(internal,
             {response,
              #{label := {table,
                          #{namespace := Namespace,
                            name := Name}},
                reply := [{row_description, Columns}]}},
             describe,
             #{config := #{publication := Publication},
               metadata := Metadata} = Data) ->
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
     nei({notify,
          #{action => add,
            relation => table_name(Publication, Namespace, Name)}})};

handle_event(internal, {Action, _}, _, _)
  when Action == query;
       Action == parse;
       Action == bind;
       Action == describe;
       Action == execute ->
    {keep_state_and_data, postpone};

handle_event(internal, {fetch, _}, _, _) ->
    {keep_state_and_data, postpone};

handle_event(Type, Content, State, Data) ->
    pgec_replica_common:handle_event(Type, Content, State, Data).


pub_fetch_sql(#{<<"schemaname">> := Schema,
                <<"tablename">> := Table} = Publication) ->
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
    lists:join(",", Attributes);

pub_fetch_columns(#{}) ->
    "*".
