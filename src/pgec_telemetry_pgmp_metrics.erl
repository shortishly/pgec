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


handle([pgmp, mm, Action, stop] = EventName,
       #{duration := Duration},
       #{args := #{sql := SQL}}, _)
  when Action == parse;
       Action == query ->
    Prefix = lists:sublist(EventName, 3),
    metrics:counter(
      [#{name => pgec_util:snake_case(Prefix ++ [count]),
         label => #{sql => SQL},
         delta => 1},
       #{name => pgec_util:snake_case(Prefix ++ [duration]),
         label => #{sql => SQL},
         delta => Duration}]);

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

       #{name => pgec_util:snake_case(Prefix ++ [duration]),
         label => maps:with([command], Metadata),
         delta => Duration}]);

handle([pgmp, mm, Action, stop] = EventName,
       #{duration := Duration},
       _,
       _)
  when Action == bind;
       Action == describe->
    Prefix = lists:sublist(EventName, 3),
    metrics:counter(
      [#{name => pgec_util:snake_case(Prefix ++ [count]), delta => 1},
       #{name => pgec_util:snake_case(Prefix ++ [duration]), delta => Duration}]);

handle([pgmp, mm, _, start], _, _, _) ->
    ok;

handle([_, _, socket, tag_msg] = EventName,
       #{bytes := Bytes, count := N},
       Metadata,
       _) ->
    metrics:counter(
      [#{name => pgec_util:snake_case(EventName ++ [count]),
         label => maps:with([tag], Metadata),
         delta => N},

       #{name => pgec_util:snake_case(EventName ++ [bytes]),
         label => maps:with([tag], Metadata),
         delta => Bytes}]);

handle(EventName, #{bytes := N}, _, _) ->
    metrics:counter(
      #{name => pgec_util:snake_case(EventName ++ [bytes]),
        delta => N});

handle(EventName, #{count := N}, _, _) ->
    metrics:counter(
      #{name => pgec_util:snake_case(EventName ++ [count]),
        delta => N});

handle(EventName, Measurements, Metadata, Config) ->
    ?LOG_INFO(#{event_name => EventName,
                measurements => Measurements,
                metadata => Metadata,
                config => Config}).
