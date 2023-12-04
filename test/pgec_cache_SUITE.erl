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


-module(pgec_cache_SUITE).


-compile(export_all).
-compile(nowarn_export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
    common:all(?MODULE).


init_per_suite(Config) ->
    _ = application:load(pgmp),
    _ = application:load(pgec),

    Port = 8080,
    Table = alpha(5),
    Publication = alpha(5),

    application:set_env(pgmp,
                        replication_logical_publication_names,
                        Publication),
    application:set_env(pgmp,
                        replication_logical_temporary,
                        false),
    application:set_env(pgmp,
                        replication_logical_module,
                        pgec_replica),

    application:set_env(pgmp, pgmp_replication_enabled, false),
    application:set_env(pgmp, mm_trace, false),
    application:set_env(pgmp, mm_log, true),
    application:set_env(pgmp, mm_log_n, 50),

    application:set_env(pgmp, rep_log_trace, false),
    application:set_env(pgmp, rep_log_ets_trace, false),

    application:set_env(pgec, http_port, Port),
    application:set_env(pgec, table_metadata_trace, false),

    RootPath = filename:join(
                 ?config(manager, Config),
                 "leveled"),
    ok = filelib:ensure_dir(RootPath),
    application:set_env(pgec, leveled_root_path, RootPath),

    application:set_env(mcd, protocol_callback, pgec_mcd_emulator),
    application:set_env(resp, protocol_callback, pgec_resp_emulator),

    {ok, _} = pgec:start(),

    ct:log("pgmp logical replication name: ~p~n",
           [pgmp_config:replication(logical, publication_names)]),

    logger:set_module_level([], debug),

    [{command_complete,
      create_table}] = pgmp_connection_sync:query(
                         #{sql => io_lib:format(
                                    "create table ~s (k serial primary key, v text)",
                                    [Table])}),


    [{command_complete,
      create_publication}] = pgmp_connection_sync:query(
                               #{sql => io_lib:format(
                                          "create publication ~s for table ~s",
                                          [Publication, Table])}),

    [{command_complete, 'begin'}] = pgmp_connection_sync:query(#{sql => "begin"}),


    [{parse_complete, []}] = pgmp_connection_sync:parse(
                               #{sql => io_lib:format(
                                          "insert into ~s (v) values ($1) returning *",
                                          [Table])}),

    lists:map(
      fun
          (_) ->
              [{bind_complete, []}] = pgmp_connection_sync:bind(
                                        #{args => [alpha(5)]}),

              [{row_description, _},
               {data_row, Row},
               {command_complete,
                {insert, 1}}] =  pgmp_connection_sync:execute(#{}),

              list_to_tuple(Row)
      end,
      lists:seq(1, 50)),

    Columns = [<<"pubname">>,
               <<"schemaname">>,
               <<"tablename">>],

    [{parse_complete,[]}] =  pgmp_connection_sync:parse(
                               #{sql => lists:join(
                                          " ",
                                          ["select",
                                           lists:join(",", Columns),
                                           "from pg_catalog.pg_publication_tables",
                                           "where pubname = $1"])}),

    [{bind_complete, []}] = pgmp_connection_sync:bind(#{args => [Publication]}),

    [{row_description,
      Columns},
     {data_row,
      [Publication,
       Schema,
       Table]},
     {command_complete,
      {select,1}}] = pgmp_connection_sync:execute(#{}),

    [{command_complete, commit}] = pgmp_connection_sync:query(#{sql => "commit"}),

    [{_, DbSup, supervisor, [pgmp_db_sup]}] = supervisor:which_children(
                                                pgmp_sup:get_child_pid(
                                                  pgmp_sup,
                                                  dbs_sup)),
    DB = pgmp_sup:get_child_pid(DbSup, db),

    {ok, LogRepSup} = pgmp_db:start_replication_on_publication(
                        DB,
                        Publication),

    {_, Manager, worker, _} = pgmp_sup:get_child(LogRepSup, manager),

    ct:log("manager: ~p~n", [sys:get_state(Manager)]),

    wait_for(ready,
             fun
                 () ->
                     element(1, sys:get_state(Manager))
             end,
             5),

    ct:log("manager: ~p~n", [sys:get_state(Manager)]),
    ct:log("which_groups: ~p~n", [pgec_pg:which_groups()]),
    ct:log("publication: ~p~n", [[pgec_replica, Publication]]),
    ct:log("get_members: ~p~n", [pgec_pg:get_members([pgec_replica, Publication])]),


    wait_for(false,
             fun
                 () ->
                     pgec_storage_sync:keys(
                       #{publication => Publication,
                         table => Table}) == []
             end,
             5),

    ct:log("keys: ~p~n",
           [pgec_storage_sync:keys(
              #{publication => Publication,
                table => Table})]),

    [{manager, Manager},
     {db, DB},
     {publication, Publication},
     {schema, Schema},
     {table, Table},
     {port, 8080} | Config].


update_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Schema = ?config(schema, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),

    ct:log("schema: ~p,~ntable: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgec_replica:when_ready(
                      #{server_ref => Manager})),

    K = pick_one(
          pgec_storage_sync:keys(
            #{publication => Publication,
              table => Table})),

    ct:log("k: ~p~n", [K]),

    {ok, Existing} = pgec_storage_sync:read(
                       #{publication => Publication,
                         table => Table,
                         key => K}),

    ct:log("existing: ~p~n", [Existing]),

    [{command_complete, 'begin'}] = pgmp_connection_sync:query(#{sql => "begin"}),

    [{parse_complete, []}] = pgmp_connection_sync:parse(
                               #{sql => io_lib:format(
                                          "update ~s set v = $2 where k = $1 returning *",
                                          [Table])}),

    V = alpha(5),

    [{bind_complete, []}] = pgmp_connection_sync:bind(
                              #{args => [K, V]}),

    [{row_description, _},
     {data_row, [K, V] = Updated},
     {command_complete,
      {update, 1}}] =  pgmp_connection_sync:execute(#{}),

    ct:log("updated: ~p~n", [Updated]),

    [{command_complete, commit}] = pgmp_connection_sync:query(#{sql => "commit"}),

    wait_for(
      {ok, V},
      fun () ->
              pgec_storage_sync:read(
                #{publication => Publication,
                  table => Table,
                  key => K})
      end),

    URL = uri_string:recompose(
            #{host => "localhost",
              path => lists:join("/", [Publication, Table, integer_to_list(K)]),
              port => Port,
              scheme => "http"}),

    ct:log("~p~n", [URL]),

    {ok,
     {{"HTTP/1.1", 200, "OK"},
      [{"date", _},
       {"server", "Cowboy"},
       {"content-length", _},
       {"content-type", "application/json"}],
      JSON}} = httpc:request(
                 get,
                 {URL, []},
                 [{timeout, 1_000}],
                 [{body_format, binary}]),

    #{<<"v">> := V} = jsx:decode(JSON).


delete_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),
    Schema = ?config(schema, Config),

    ct:log("schema: ~p,~ntable: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgec_replica:when_ready(
                      #{server_ref => Manager})),

    K = pick_one(
          pgec_storage_sync:keys(
            #{publication => Publication,
              table => Table})),

    ct:log("k: ~p~n", [K]),

    {ok, V} = pgec_storage_sync:read(
                       #{publication => Publication,
                         table => Table,
                         key => K}),

    ct:log("v: ~p~n", [V]),

    [{command_complete, 'begin'}] = pgmp_connection_sync:query(#{sql => "begin"}),

    [{parse_complete, []}] = pgmp_connection_sync:parse(
                               #{sql => io_lib:format(
                                          "delete from ~s where k = $1 returning *",
                                          [Table])}),

    [{bind_complete, []}] = pgmp_connection_sync:bind(
                              #{args => [K]}),

    [{row_description, _},
     {data_row, [K, V] = Deleted},
     {command_complete,
      {delete, 1}}] =  pgmp_connection_sync:execute(#{}),

    ct:log("deleted: ~p~n", [Deleted]),

    [{command_complete, commit}] = pgmp_connection_sync:query(#{sql => "commit"}),

    wait_for(
      not_found,
      fun () ->
              pgec_storage_sync:read(
                #{publication => Publication,
                  table => Table,
                  key => K})
      end),

    URL = uri_string:recompose(
            #{host => "localhost",
              path => lists:join("/", [Publication, Table, integer_to_list(K)]),
              port => Port,
              scheme => "http"}),

    ct:log("~p~n", [URL]),

    {ok,
     {{"HTTP/1.1", 404, "Not Found"},
      [{"date", _},
       {"server", "Cowboy"},
       {"content-length", _},
       {"content-type", "application/json"}],
      JSON}} = httpc:request(
                 get,
                 {URL, []},
                 [{timeout, 1_000}],
                 [{body_format, binary}]),

    ct:log("~p~n", [JSON]),

    ?assertEqual(
    #{<<"keys">> => [integer_to_binary(K)],
      <<"publication">> => Publication,
      <<"table">> => Table},
       jsx:decode(JSON)).


insert_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),
    Schema = ?config(schema, Config),

    ct:log("schema: ~p,~ntable: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgec_replica:when_ready(
                      #{server_ref => Manager})),


    [{command_complete, 'begin'}] = pgmp_connection_sync:query(#{sql => "begin"}),

    [{parse_complete, []}] = pgmp_connection_sync:parse(
                               #{sql => io_lib:format(
                                          "insert into ~s (v) values ($1) returning *",
                                          [Table])}),

    [{bind_complete, []}] = pgmp_connection_sync:bind(
                              #{args => [alpha(5)]}),

    [{row_description, _},
     {data_row, [K, V] = Inserted},
     {command_complete,
      {insert, 1}}] =  pgmp_connection_sync:execute(#{}),

    ct:log("inserted: ~p~n", [Inserted]),

    [{command_complete, commit}] = pgmp_connection_sync:query(#{sql => "commit"}),

    wait_for(
      {ok, V},
      fun () ->
              pgec_storage_sync:read(
                #{publication => Publication,
                  table => Table,
                  key => K})
      end),

    URL = uri_string:recompose(
            #{host => "localhost",
              path => lists:join("/", [Publication, Table, integer_to_list(K)]),
              port => Port,
              scheme => "http"}),

    ct:log("~p~n", [URL]),

    {ok,
     {{"HTTP/1.1", 200, "OK"},
      [{"date", _},
       {"server", "Cowboy"},
       {"content-length", _},
       {"content-type", "application/json"}],
      JSON}} = httpc:request(
                 get,
                 {URL, []},
                 [{timeout, 1_000}],
                 [{body_format, binary}]),

    #{<<"v">> := V} = jsx:decode(JSON).


wait_for(Expected, Check) ->
    ct:log("expected: ~p, check: ~p~n", [Expected, Check]),
    ?FUNCTION_NAME(Expected, Check, 5).

wait_for(Expected, Check, 0 = N) ->
    case Check() of
        Expected ->
            Expected;

        Unexpected ->
            ct:log("expected: ~p~ncheck: ~p~nn: ~p~nactual: ~p~n",
                   [Expected, Check, N, Unexpected]),
            Expected = Unexpected
    end;

wait_for(Expected, Check, N) ->
    case Check() of
        Expected ->
            ct:log("matched: ~p,~ncheck: ~p,~nn: ~p~n",
                   [Expected, Check, N]),
            Expected;

        Unexpected ->
            ct:log("expected: ~p,~ncheck: ~p,~nn: ~p,~nactual: ~p~n",
                   [Expected, Check, N, Unexpected]),
            timer:sleep(timer:seconds(1)),
            ?FUNCTION_NAME(Expected, Check, N - 1)
    end.


end_per_suite(Config) ->
    Table = ?config(table, Config),
    Publication = ?config(publication, Config),
    DB = ?config(db, Config),

    ct:log(
      "stop_replication: ~p~n",
      [pgmp_db:stop_replication_on_publication(
         DB,
         Publication)]),

    ct:log(
      "~p~n",
      [common:pbe(#{sql => "select pg_drop_replication_slot($1)",
                    args => [pgmp_rep_log:slot_name(Publication)]})]),

    ct:log("~s: ~p~n",
           [Table,
            pgmp_connection_sync:query(
              #{sql => iolist_to_binary(
                         io_lib:format(
                           "drop table ~s cascade",
                           [Table]))})]),

    common:stop_applications().


alpha(N) ->
    list_to_binary(pick(N, lists:seq($a, $z))).


pick_one(Pool) ->
    ct:log("pool: ~p~n", [Pool]),
    [Victim] = pick(1, Pool),
    Victim.


pick(N, Pool) ->
    ?FUNCTION_NAME(N, Pool, []).


pick(0, _, A) ->
    A;

pick(N, Pool, A) ->
    ?FUNCTION_NAME(N - 1,
                   Pool,
                   [lists:nth(rand:uniform(length(Pool)), Pool) | A]).
