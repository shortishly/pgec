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


-module(pgec_resp_cache_SUITE).


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
    application:set_env(pgmp, pgmp_replication_enabled, false),
    application:set_env(pgmp, mm_trace, false),
    application:set_env(pgmp, mm_log, true),
    application:set_env(pgmp, mm_log_n, 50),
    application:set_env(pgmp, codec_jsonb, jsx),
    application:set_env(pgmp, codec_json, jsx),

    application:set_env(pgmp, rep_log_trace, false),
    application:set_env(pgmp, rep_log_ets_trace, false),

    application:set_env(pgec, http_port, Port),
    application:set_env(pgec, table_metadata_trace, false),

    application:set_env(mcd, protocol_callback, pgec_mcd_emulator),
    application:set_env(resp, protocol_callback, pgec_resp_emulator),
    application:set_env(resp, listener_enabled, true),

    {ok, _} = pgec:start(),

    ct:log("pgmp logical replication name: ~p~n",
           [pgmp_config:replication(logical, publication_names)]),

    logger:set_module_level([], debug),

    [{command_complete,
      create_table}] = pgmp_connection_sync:query(
                         #{sql => io_lib:format(
                                    "create table ~s (k uuid default gen_random_uuid() primary key, v text, w integer default null)",
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

    {ok, Sup} = pgmp_rep_sup:start_child(Publication),

    {_, Manager, worker, _} = pgmp_sup:get_child(Sup, manager),

    ct:log("manager: ~p~n", [sys:get_state(Manager)]),

    wait_for(ready,
             fun
                 () ->
                     element(1, sys:get_state(Manager))
             end,
             5),

    ct:pal("codec(json): ~p~n", [pgmp_config:codec(json)]),
    ct:pal("codec(jsonb): ~p~n", [pgmp_config:codec(jsonb)]),
    ct:log("manager: ~p~n", [sys:get_state(Manager)]),
    ct:log("which_groups: ~p~n", [pgmp_pg:which_groups()]),
    ct:log("publication: ~p~n", [[pgmp_rep_log_ets, Publication]]),
    ct:log("get_members: ~p~n", [pgmp_pg:get_members([pgmp_rep_log_ets, Publication])]),

    {ok, Client} = resp_client:start(),

    [{manager, Manager},
     {publication, Publication},
     {schema, Schema},
     {table, Table},
     {port, 8080},
     {client, Client},
     {replica, binary_to_atom(Table)} | Config].


ping_simple_test(Config) ->
    [{string,
      <<"pong">>}] = send_sync(
                       Config,
                       {array,
                        [{bulk, "ping"}]}).


ping_bulk_test(Config) ->
    Greeting = alpha(15),
    [{bulk, Greeting}] = send_sync(
                           Config,
                           {array,
                            [{bulk, "ping"},
                             {bulk, Greeting}]}).


resp_hello3_test(Config) ->
    ?assertMatch(
       [{map,
         [{{bulk, <<"server">>}, {bulk, <<"pgec">>}},
          {{bulk, <<"version">>}, {bulk, _}},
          {{bulk,<<"proto">>}, {integer, 3}},
          {{bulk, <<"id">>}, {integer, _}},
          {{bulk, <<"mode">>}, {bulk, <<"standalone">>}},
          {{bulk, <<"role">>}, {bulk, <<"master">>}},
          {{bulk, <<"modules">>}, {array, []}}]}],
       send_sync(
         Config,
         {array,
          [{bulk, "hello"},
           {bulk, "3"}]})).


resp_hello2_test(Config) ->
    ?assertMatch(
       [{array,
         [{bulk, <<"server">>}, {bulk, <<"pgec">>},
          {bulk, <<"version">>}, {bulk, _},
          {bulk, <<"proto">>}, {integer, 2},
          {bulk, <<"id">>}, {integer, _},
          {bulk, <<"mode">>}, {bulk, <<"standalone">>},
          {bulk, <<"role">>}, {bulk, <<"master">>},
          {bulk, <<"modules">>}, {array, []}]}],
       send_sync(
         Config,
         {array,
          [{bulk, "hello"},
           {bulk, "2"}]})).


resp_hello_test(Config) ->
    ?assertMatch(
       [{array,
         [{bulk, <<"server">>}, {bulk, <<"pgec">>},
          {bulk, <<"version">>}, {bulk, _},
          {bulk, <<"proto">>}, {integer, 2},
          {bulk, <<"id">>}, {integer, _},
          {bulk, <<"mode">>}, {bulk, <<"standalone">>},
          {bulk, <<"role">>}, {bulk, <<"master">>},
          {bulk, <<"modules">>}, {array, []}]}],
       send_sync(
         Config,
         {array,
          [{bulk, "hello"}]})).


exists_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Schema = ?config(schema, Config),
    Replica = ?config(replica, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),

    ct:log("schema: ~p,~ntable: ~p,~nreplica: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Replica, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgmp_rep_log_ets:when_ready(
                      #{server_ref => Manager})),

    {K, _, _} = Existing = pick_one(ets:tab2list(Replica)),
    ct:log("existing: ~p~n", [Existing]),

    [{row_description, _},
     {data_row, [NotPresent]},
     {command_complete, {select, 1}}] = pgmp_connection_sync:query(
                                          #{sql => "select gen_random_uuid()"}),

    [{integer, 1}] = send_sync(
                       Config,
                       {array,
                        [{bulk, "exists"},

                         {bulk,
                          lists:join(
                            ".", [Publication, Table, K])},

                         {bulk,
                          lists:join(
                            ".", ["zzzz", Table, K])},

                         {bulk,
                          lists:join(
                            ".", [Publication, "zzzz", K])},

                         {bulk,
                          lists:join(
                            ".", [Publication, Table, NotPresent])}]}).


hexists_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Schema = ?config(schema, Config),
    Replica = ?config(replica, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),

    ct:log("schema: ~p,~ntable: ~p,~nreplica: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Replica, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgmp_rep_log_ets:when_ready(
                      #{server_ref => Manager})),

    {K, _, _} = Existing = pick_one(ets:tab2list(Replica)),
    ct:log("existing: ~p~n", [Existing]),

    [{integer, 0}] = send_sync(
                       Config,
                       {array,
                        [{bulk, "hexists"},
                         {bulk,
                          [lists:join(
                             ".",
                             ["zzzz", Table, K])]},
                         {bulk, "k"}]}),

    [{integer, 0}] = send_sync(
                       Config,
                       {array,
                        [{bulk, "hexists"},
                         {bulk,
                          lists:join(
                            ".",
                            [Publication, "zzzz", K])},
                         {bulk, "k"}]}),

    [{integer, 0}] = send_sync(
                       Config,
                       {array,
                        [{bulk, "hexists"},
                         {bulk,
                          lists:join(
                            ".",
                            [Publication, Table, K])},
                         {bulk, "zzzz"}]}),

    [{integer, 1}] = send_sync(
                       Config,
                       {array,
                        [{bulk, "hexists"},
                         {bulk,
                          lists:join(
                            ".",
                            [Publication, Table, K])},
                         {bulk, "k"}]}),

    [{integer, 1}] = send_sync(
                       Config,
                       {array,
                        [{bulk, "hexists"},
                         {bulk,
                          lists:join(
                            ".",
                            [Publication, Table, K])},
                         {bulk, "v"}]}).


hget_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Schema = ?config(schema, Config),
    Replica = ?config(replica, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),

    ct:log("schema: ~p,~ntable: ~p,~nreplica: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Replica, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgmp_rep_log_ets:when_ready(
                      #{server_ref => Manager})),

    {K, V, _} = Existing = pick_one(ets:tab2list(Replica)),
    ct:log("existing: ~p~n", [Existing]),

    [{bulk, null}] = send_sync(
                       Config,
                       {array,
                        [{bulk, "hget"},
                         {bulk,
                          [lists:join(
                             ".",
                             ["zzzz", Table, K])]},
                         {bulk, "k"}]}),

    [{bulk, null}] = send_sync(
                       Config,
                       {array,
                        [{bulk, "hget"},
                         {bulk,
                          lists:join(
                            ".",
                            [Publication, "zzzz", K])},
                         {bulk, "k"}]}),

    [{row_description, _},
     {data_row, [NotPresent]},
     {command_complete, {select, 1}}] = pgmp_connection_sync:query(
                                          #{sql => "select gen_random_uuid()"}),

    [{bulk, null}] = send_sync(
                       Config,
                       {array,
                        [{bulk, "hget"},
                         {bulk,
                          lists:join(
                            ".",
                            [Publication, Table, NotPresent])},
                         {bulk, "k"}]}),

    [{bulk, null}] = send_sync(
                       Config,
                       {array,
                        [{bulk, "hget"},
                         {bulk,
                          lists:join(
                            ".",
                            [Publication, Table, K])},
                         {bulk, "zzzz"}]}),

    ?assertEqual(
       [{bulk, K}],
       send_sync(
         Config,
         {array,
          [{bulk, "hget"},
           {bulk,
            lists:join(
              ".",
              [Publication, Table, K])},
           {bulk, "k"}]})),

    [{bulk, V}] = send_sync(
                    Config,
                    {array,
                     [{bulk, "hget"},
                      {bulk,
                       lists:join(
                         ".",
                         [Publication, Table, K])},
                      {bulk, "v"}]}).


hgetall_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Schema = ?config(schema, Config),
    Replica = ?config(replica, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),

    ct:log("schema: ~p,~ntable: ~p,~nreplica: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Replica, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgmp_rep_log_ets:when_ready(
                      #{server_ref => Manager})),

    {K, V, _} = Existing = pick_one(ets:tab2list(Replica)),
    ct:log("existing: ~p~n", [Existing]),

    [{array, []}] = send_sync(
                      Config,
                      {array,
                       [{bulk, "hgetall"},
                        {bulk,
                         [lists:join(
                            ".",
                            ["zzzz", Table, K])]}]}),

    [{array, []}] = send_sync(
                      Config,
                      {array,
                       [{bulk, "hgetall"},
                        {bulk,
                         lists:join(
                           ".",
                           [Publication, "zzzz", K])}]}),

    [{row_description, _},
     {data_row, [NotPresent]},
     {command_complete, {select, 1}}] = pgmp_connection_sync:query(
                                          #{sql => "select gen_random_uuid()"}),

    [{array, []}] = send_sync(
                      Config,
                      {array,
                       [{bulk, "hgetall"},
                        {bulk,
                         lists:join(
                           ".",
                           [Publication, Table, NotPresent])}]}),

    ?assertEqual(
       [{array,
         [{bulk, <<"v">>},
          {bulk, V},
          {bulk, <<"k">>},
          {bulk, K}]}],
       send_sync(
         Config,
         {array,
          [{bulk, "hgetall"},
           {bulk,
            lists:join(
              ".",
              [Publication, Table, K])}]})).


hlen_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Schema = ?config(schema, Config),
    Replica = ?config(replica, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),

    ct:log("schema: ~p,~ntable: ~p,~nreplica: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Replica, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgmp_rep_log_ets:when_ready(
                      #{server_ref => Manager})),

    {K, _V, _} = Existing = pick_one(ets:tab2list(Replica)),
    ct:log("existing: ~p~n", [Existing]),

    [{integer, 0}] = send_sync(
                       Config,
                       {array,
                        [{bulk, "hlen"},
                         {bulk,
                          [lists:join(
                             ".",
                             ["zzzz", Table, K])]}]}),

    [{integer, 0}] = send_sync(
                       Config,
                       {array,
                        [{bulk, "hlen"},
                         {bulk,
                          lists:join(
                            ".",
                            [Publication, "zzzz", K])}]}),

    [{row_description, _},
     {data_row, [NotPresent]},
     {command_complete, {select, 1}}] = pgmp_connection_sync:query(
                                          #{sql => "select gen_random_uuid()"}),

    [{integer, 0}] = send_sync(
                       Config,
                       {array,
                        [{bulk, "hlen"},
                         {bulk,
                          lists:join(
                            ".",
                            [Publication, Table, NotPresent])}]}),

    ?assertEqual(
       [{integer, 2}],
       send_sync(
         Config,
         {array,
          [{bulk, "hlen"},
           {bulk,
            lists:join(
              ".",
              [Publication, Table, K])}]})).


hkeys_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Schema = ?config(schema, Config),
    Replica = ?config(replica, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),

    ct:log("schema: ~p,~ntable: ~p,~nreplica: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Replica, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgmp_rep_log_ets:when_ready(
                      #{server_ref => Manager})),

    {K, _V, _} = Existing = pick_one(ets:tab2list(Replica)),
    ct:log("existing: ~p~n", [Existing]),

    [{array, []}] = send_sync(
                      Config,
                      {array,
                       [{bulk, "hkeys"},
                        {bulk,
                         [lists:join(
                            ".",
                            ["zzzz", Table, K])]}]}),

    [{array, []}] = send_sync(
                      Config,
                      {array,
                       [{bulk, "hkeys"},
                        {bulk,
                         lists:join(
                           ".",
                           [Publication, "zzzz", K])}]}),

    [{row_description, _},
     {data_row, [NotPresent]},
     {command_complete, {select, 1}}] = pgmp_connection_sync:query(
                                          #{sql => "select gen_random_uuid()"}),

    [{array, []}] = send_sync(
                      Config,
                      {array,
                       [{bulk, "hkeys"},
                        {bulk,
                         lists:join(
                           ".",
                           [Publication, Table, NotPresent])}]}),

    ?assertEqual(
       [{array,
        [{bulk, <<"v">>},
         {bulk, <<"k">>}]}],
       send_sync(
         Config,
         {array,
          [{bulk, "hkeys"},
           {bulk,
            lists:join(
              ".",
              [Publication, Table, K])}]})).

hset_update_invalid_field_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Schema = ?config(schema, Config),
    Replica = ?config(replica, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),

    ct:log("schema: ~p,~ntable: ~p,~nreplica: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Replica, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgmp_rep_log_ets:when_ready(
                      #{server_ref => Manager})),

    {K, _, _} = Existing = pick_one(ets:tab2list(Replica)),
    ct:log("existing: ~p~n", [Existing]),

    V1 = alpha(5),

    ?assertMatch(
       [{error, _}],
       send_sync(
         Config,
         {array,
          [{bulk, "hset"},
           {bulk,
            lists:join(
              ".",
              [Publication, Table, K])},
           {bulk, "zzzz"},
           {bulk, V1}]})).

hset_insert_invalid_field_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Schema = ?config(schema, Config),
    Replica = ?config(replica, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),

    ct:log("schema: ~p,~ntable: ~p,~nreplica: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Replica, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgmp_rep_log_ets:when_ready(
                      #{server_ref => Manager})),


    [{row_description, _},
     {data_row, [NotPresent]},
     {command_complete, {select, 1}}] = pgmp_connection_sync:query(
                                          #{sql => "select gen_random_uuid()"}),
    V1 = alpha(5),

    ?assertMatch(
       [{error, _}],
       send_sync(
         Config,
         {array,
          [{bulk, "hset"},
           {bulk,
            lists:join(
              ".",
              [Publication, Table, NotPresent])},
           {bulk, "zzzz"},
           {bulk, V1}]})).


hset_update_v_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Schema = ?config(schema, Config),
    Replica = ?config(replica, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),

    ct:log("schema: ~p,~ntable: ~p,~nreplica: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Replica, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgmp_rep_log_ets:when_ready(
                      #{server_ref => Manager})),

    {K, _, W} = Existing = pick_one(ets:tab2list(Replica)),
    ct:log("existing: ~p~n", [Existing]),

    V1 = alpha(5),

    ?assertEqual(
       [{integer, 1}],
       send_sync(
         Config,
         {array,
          [{bulk, "hset"},
           {bulk,
            lists:join(
              ".",
              [Publication, Table, K])},
           {bulk, "v"},
           {bulk, V1}]})),

    wait_for(
      [{K, V1, W}],
      fun
          () ->
              ets:lookup(Replica, K)
      end),

    ?assertEqual(
       [{array,
         [{bulk, <<"v">>},
          {bulk, V1},
          {bulk, <<"k">>},
          {bulk, K}]}],
       send_sync(
         Config,
         {array,
          [{bulk, "hgetall"},
           {bulk,
            lists:join(
              ".",
              [Publication, Table, K])}]})).

hset_update_w_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Schema = ?config(schema, Config),
    Replica = ?config(replica, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),

    ct:log("schema: ~p,~ntable: ~p,~nreplica: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Replica, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgmp_rep_log_ets:when_ready(
                      #{server_ref => Manager})),

    {K, V, null} = Existing = pick_one(ets:tab2list(Replica)),
    ct:log("existing: ~p~n", [Existing]),

    W1 = 12321,

    ?assertEqual(
       [{integer, 1}],
       send_sync(
         Config,
         {array,
          [{bulk, "hset"},
           {bulk,
            lists:join(
              ".",
              [Publication, Table, K])},
           {bulk, "w"},
           {bulk, integer_to_list(W1)}]})),

    wait_for(
      [{K, V, W1}],
      fun
          () ->
              ets:lookup(Replica, K)
      end),

    ?assertEqual(
       [{array,
         [{bulk, <<"w">>},
          {bulk, integer_to_binary(W1)},
          {bulk, <<"v">>},
          {bulk, V},
          {bulk, <<"k">>},
          {bulk, K}]}],
       send_sync(
         Config,
         {array,
          [{bulk, "hgetall"},
           {bulk,
            lists:join(
              ".",
              [Publication, Table, K])}]})).


hset_update_invalid_type_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Schema = ?config(schema, Config),
    Replica = ?config(replica, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),

    ct:log("schema: ~p,~ntable: ~p,~nreplica: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Replica, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgmp_rep_log_ets:when_ready(
                      #{server_ref => Manager})),

    {K, V, W} = Existing = pick_one(ets:tab2list(Replica)),
    ct:log("existing: ~p~n", [Existing]),

    W1 = alpha(5),

    ?assertMatch(
       [{error, _}],
       send_sync(
         Config,
         {array,
          [{bulk, "hset"},
           {bulk,
            lists:join(
              ".",
              [Publication, Table, K])},
           {bulk, "w"},
           {bulk, W1}]})),

    wait_for(
      [{K, V, W}],
      fun
          () ->
              ets:lookup(Replica, K)
      end),

    ?assertEqual(
       [{array,
         [{bulk, <<"v">>},
          {bulk, V},
          {bulk, <<"k">>},
          {bulk, K}]}],
       send_sync(
         Config,
         {array,
          [{bulk, "hgetall"},
           {bulk,
            lists:join(
              ".",
              [Publication, Table, K])}]})).


hset_insert_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Schema = ?config(schema, Config),
    Replica = ?config(replica, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),

    ct:log("schema: ~p,~ntable: ~p,~nreplica: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Replica, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgmp_rep_log_ets:when_ready(
                      #{server_ref => Manager})),

    ct:log("~p~n", [lists:sort(ets:tab2list(Replica))]),

    [{row_description, _},
     {data_row, [K]},
     {command_complete, {select, 1}}] = pgmp_connection_sync:query(
                                          #{sql => "select gen_random_uuid()"}),
    V = alpha(5),

    ct:log("k: ~p, v: ~p~n", [K, V]),

    ?assertEqual(
       [{integer, 1}],
       send_sync(
         Config,
         {array,
          [{bulk, "hset"},
           {bulk,
            lists:join(
              ".",
              [Publication, Table, K])},
           {bulk, "v"},
           {bulk, V}]})),

    wait_for(
      [{K, V, null}],
      fun
          () ->
              ets:lookup(Replica, K)
      end),

    ?assertEqual(
       [{array,
         [{bulk, <<"v">>},
          {bulk, V},
          {bulk, <<"k">>},
          {bulk, K}]}],
       send_sync(
         Config,
         {array,
          [{bulk, "hgetall"},
           {bulk,
            lists:join(
              ".",
              [Publication, Table, K])}]})).


update_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Schema = ?config(schema, Config),
    Replica = ?config(replica, Config),
    Port = ?config(port, Config),
    Publication = ?config(publication, Config),

    ct:log("schema: ~p,~ntable: ~p,~nreplica: ~p,~nport: ~p,~npublication: ~p~n",
           [Schema, Table, Replica, Port, Publication]),

    {reply, ok} = gen_statem:receive_response(
                    pgmp_rep_log_ets:when_ready(
                      #{server_ref => Manager})),

    {K, _, W} = Existing = pick_one(ets:tab2list(Replica)),
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
     {data_row, [K, V, W] = Updated},
     {command_complete,
      {update, 1}}] =  pgmp_connection_sync:execute(#{}),

    ct:log("updated: ~p~n", [Updated]),

    [{command_complete, commit}] = pgmp_connection_sync:query(#{sql => "commit"}),

    wait_for(
      [list_to_tuple(Updated)],
      fun () ->
              ets:lookup(Replica, K)
      end),

    [{array, _} = KV] = send_sync(
                          Config,
                          {array,
                           [{bulk, "hgetall"},
                            {bulk,
                             lists:join(
                               ".", [Publication, Table, K])}]}),

    ct:log("hgetall: ~p~n", [KV]),

    #{<<"v">> := V} = as_map(KV).


delete_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Replica = ?config(replica, Config),
    Publication = ?config(publication, Config),

    {reply, ok} = gen_statem:receive_response(
                    pgmp_rep_log_ets:when_ready(
                      #{server_ref => Manager})),

    {K, V, W} = Existing = pick_one(ets:tab2list(Replica)),
    ct:log("existing: ~p~n", [Existing]),

    [{command_complete, 'begin'}] = pgmp_connection_sync:query(#{sql => "begin"}),

    [{parse_complete, []}] = pgmp_connection_sync:parse(
                               #{sql => io_lib:format(
                                          "delete from ~s where k = $1 returning *",
                                          [Table])}),

    [{bind_complete, []}] = pgmp_connection_sync:bind(
                              #{args => [K]}),

    [{row_description, _},
     {data_row, [K, V, W] = Deleted},
     {command_complete,
      {delete, 1}}] =  pgmp_connection_sync:execute(#{}),

    ct:log("deleted: ~p~n", [Deleted]),

    [{command_complete, commit}] = pgmp_connection_sync:query(#{sql => "commit"}),

    wait_for(
      [],
      fun () ->
              ets:lookup(Replica, K)
      end),

    [{array,[]}] = send_sync(
                     Config,
                     {array,
                      [{bulk, "hgetall"},
                       {bulk,
                        lists:join(
                          ".", [Publication, Table, K])}]}).


insert_test(Config) ->
    Manager = ?config(manager, Config),
    Table = ?config(table, Config),
    Replica = ?config(replica, Config),
    Publication = ?config(publication, Config),

    {reply, ok} = gen_statem:receive_response(
                    pgmp_rep_log_ets:when_ready(
                      #{server_ref => Manager})),


    [{command_complete, 'begin'}] = pgmp_connection_sync:query(#{sql => "begin"}),

    [{parse_complete, []}] = pgmp_connection_sync:parse(
                               #{sql => io_lib:format(
                                          "insert into ~s (v) values ($1) returning *",
                                          [Table])}),

    [{bind_complete, []}] = pgmp_connection_sync:bind(
                              #{args => [alpha(5)]}),

    [{row_description, _},
     {data_row, [K, V, null] = Inserted},
     {command_complete,
      {insert, 1}}] =  pgmp_connection_sync:execute(#{}),

    ct:log("inserted: ~p~n", [Inserted]),

    [{command_complete, commit}] = pgmp_connection_sync:query(#{sql => "commit"}),

    wait_for(
      [list_to_tuple(Inserted)],
      fun
          () ->
              ets:lookup(Replica, K)
      end),

    [{array, _} = KV] = send_sync(
                          Config,
                          {array,
                           [{bulk, "hgetall"},
                            {bulk,
                             lists:join(
                               ".", [Publication, Table, K])}]}),

    ct:log("hgetall: ~p~n", [KV]),

    #{<<"v">> := V} = as_map(KV).


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
    C = ?config(client, Config),

    ok = gen_statem:stop(C),

    ct:log("~s: ~p~n",
           [Table,
            pgmp_connection_sync:query(
              #{sql => iolist_to_binary(
                         io_lib:format(
                           "drop table ~s cascade",
                           [Table]))})]),

    common:purge_applications().


alpha(N) ->
    list_to_binary(pick(N, lists:seq($a, $z))).


pick_one(Pool) ->
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


send_sync(Config, Data) ->
    Client = ?config(client, Config),
    {reply, Reply} = gen_statem:receive_response(
                       resp_client:send(
                         #{server_ref => Client,
                           data => Data})),
    Reply.


as_map({array, L}) ->
    ?FUNCTION_NAME(L, #{}).


as_map([{bulk, K}, {bulk, V} | T], A) ->
    ?FUNCTION_NAME(T, A#{K => V});

as_map([], A) ->
    A.
