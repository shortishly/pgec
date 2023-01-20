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


-module(pgec_telemetry_pgmp_metrics).


-export([handle/4]).
-include_lib("kernel/include/logger.hrl").


%% This clause handles telemetry from pgmp middleman dealing with
%% replication events
%%
handle([pgmp, mm, rep, _] = EventName,
       #{wal := WAL} = Measurements,
       #{identify_system := #{<<"dbname">> := DBName,
                              <<"systemid">> := SystemId}} = Metadata,
       Config) ->

    ?LOG_DEBUG(#{event_name => EventName,
                 measurements => Measurements,
                 metadata => Metadata,
                 config => Config}),

    Prefix = lists:sublist(EventName, 3),

    Label = maps:merge(
              #{dbname => DBName, systemid => SystemId},
              maps:with([publication], Metadata)),

    %% A gauge for each WAL metric
    %%
    metrics:gauge(
      maps:fold(
        fun
            (WALMetricName, Value, A) ->
                [#{name => pgec_util:snake_case(Prefix ++ [wal, WALMetricName]),
                   label => Label,
                   value => Value} | A]
        end,
        [],
        WAL)),

    %% A counter for each processed replication event. Truncate is a
    %% special case because it is on a list of relations, rather than
    %% a single relation.
    %%
    metrics:counter(
      mm_rep_count(EventName, Measurements, Metadata, Config, Label));


%% This clause handles telemetry from pgmp middleman dealing with
%% parse and query, to include the SQL being processed
%%
handle([pgmp, mm, Action, stop] = EventName,
       #{duration := Duration},
       #{args := #{sql := SQL}}, _)
  when Action == parse;
       Action == query ->
    Prefix = lists:sublist(EventName, 3),

    %% Maintain a count of each action processed with their cumulative
    %% duration.
    %%
    metrics:counter(
      [#{name => pgec_util:snake_case(Prefix ++ [count]),
         label => #{sql => SQL},
         delta => 1},

       #{name => pgec_util:snake_case(Prefix ++ [duration, ms]),
         label => #{sql => SQL},
         delta => erlang:convert_time_unit(
                    Duration,
                    native,
                    millisecond)}]);


%% This clause handles telemetry from pgmp middleman dealing with
%% execute to include the numnber of rows that were returned
%%
handle([pgmp, mm, execute, stop] = EventName,
       #{duration := Duration, rows := Rows},
       Metadata,
       _) ->
    Prefix = lists:sublist(EventName, 3),
    metrics:counter(
      [#{name => pgec_util:snake_case(Prefix ++ [count]),
         label => maps:with([command], Metadata),
         delta => 1},

       #{name => pgec_util:snake_case(Prefix ++ [rows]),
         label => maps:with([command], Metadata),
         delta => Rows},

       #{name => pgec_util:snake_case(Prefix ++ [duration, ms]),
         label => maps:with([command], Metadata),
         delta => erlang:convert_time_unit(
                    Duration,
                    native,
                    millisecond)}]);


%% This clause handles telemetry from pgmp middleman for bind or
%% describe.
%%
handle([pgmp, mm, Action, stop] = EventName,
       #{duration := Duration},
       _,
       _)
  when Action == bind;
       Action == describe ->
    Prefix = lists:sublist(EventName, 3),
    metrics:counter(
      [#{name => pgec_util:snake_case(Prefix ++ [count]), delta => 1},

       #{name => pgec_util:snake_case(Prefix ++ [duration, ms]),
         delta => erlang:convert_time_unit(
                    Duration,
                    native,
                    millisecond)}]);


%% Nothing to do for a start of span from pgmp middleman.
%%
handle([pgmp, mm, _, start], _, _, _) ->
    ok;


%% This clause handles pgmp socket tagged messages forming the lower
%% level PostgreSQL protocol.
%%
handle([pgmp, socket, tag_msg] = EventName,
       #{bytes := Bytes, count := N},
       Metadata,
       _) ->
    %% Maintain a count of each tagged message with the cumulative
    %% bytes processed.
    %%
    metrics:counter(
      [#{name => pgec_util:snake_case(EventName ++ [count]),
         label => maps:with([tag], Metadata),
         delta => N},

       #{name => pgec_util:snake_case(EventName ++ [bytes]),
         label => maps:with([tag], Metadata),
         delta => Bytes}]);


%% This generic clause catches any other telemetry from pgmp that
%% includes the number of bytes that were processed.
%%
handle(EventName, #{bytes := N}, _, _) ->
    metrics:counter(
      #{name => pgec_util:snake_case(EventName ++ [bytes]),
        delta => N});


%% This generic clause catches any other telemetry from pgmp that is a
%% count.
%%
handle(EventName, #{count := N}, _, _) ->
    metrics:counter(
      #{name => pgec_util:snake_case(EventName ++ [count]),
        delta => N});


%% Fall through clause to log any missed telemetry events from pgmp.
%%
handle(EventName, Measurements, Metadata, Config) ->
    ?LOG_INFO(#{event_name => EventName,
                measurements => Measurements,
                metadata => Metadata,
                config => Config}).


%% A truncate can be applied to a list of relations.
%%
mm_rep_count(EventName, #{count := N}, #{relations := Relations}, _Config, Label) ->
    lists:map(
      fun
          (Relation) ->
              #{name => pgec_util:snake_case(EventName ++ [count]),
                label => Label#{relation => Relation},
                delta => N}
      end,
      Relations);


%% This clause deals with actions other than truncate that only apply
%% to a single relation.
%%
mm_rep_count(EventName, #{count := N}, Metadata, _Config, Label) ->
    #{name => pgec_util:snake_case(EventName ++ [count]),
      label => maps:merge(
                 maps:with([relation], Metadata),
                 Label),
      delta => N}.
