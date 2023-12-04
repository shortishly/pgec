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


-module(pgec_telemetry_metrics).


-export([handle/4]).
-include_lib("kernel/include/logger.hrl").

handle([cowboy, request, start], _Measurements, _Metadata, _Config) ->
    ok;

handle([cowboy, request, stop] = EventName,
       Measurements,
       #{req := Req},
       _Config) ->
    Prefix = lists:sublist(EventName, 2),
    Label = maps:with([host, port], Req),
    metrics:counter(
      maps:fold(
        fun
            (duration = Metric, Delta, A) ->
                [#{name => pgec_util:snake_case(Prefix ++ [Metric, ms]),
                   label => Label,
                   delta => erlang:convert_time_unit(
                              Delta,
                              native,
                              millisecond)} | A];

            (Metric, Delta, A) ->
                [#{name => pgec_util:snake_case(Prefix ++ [Metric]),
                   label => Label,
                   delta => Delta} | A]
        end,
        [#{name => pgec_util:snake_case(Prefix ++ [count]),
           label => Label,
           delta => 1}],
        Measurements));

handle([cowboy, request, exception] = EventName,
       Measurements,
       #{req := Req},
       _Config) ->
    Prefix = lists:sublist(EventName, 2),
    Label = maps:with([host, port], Req),
    metrics:counter(
      maps:fold(
        fun
            (duration = Metric, Delta, A) ->
                [#{name => pgec_util:snake_case(Prefix ++ [error, Metric, ms]),
                   label => Label,
                   delta => erlang:convert_time_unit(
                              Delta,
                              native,
                              millisecond)} | A];

            (Metric, Delta, A) ->
                [#{name => pgec_util:snake_case(Prefix ++ [error, Metric]),
                   label => Label,
                   delta => Delta} | A]
        end,
        [#{name => pgec_util:snake_case(Prefix ++ [error, count]),
           label => Label,
           delta => 1}],
        Measurements));

handle([pgec, System, _] = EventName,
       #{count := N} = Measurements,
       Metadata,
       Config) when System == storage;
                    System == replica ->
    ?LOG_DEBUG(#{event_name => EventName,
                 measurements => Measurements,
                 metadata => Metadata,
                 config => Config}),

    metrics:counter(
      [#{name => pgec_util:snake_case(EventName ++ [count]),
         label => Metadata,
         delta => N}]);

handle([pgec, System, _] = EventName,
       #{value := N} = Measurements,
       Metadata,
       Config) when System == storage;
                    System == replica ->
    ?LOG_DEBUG(#{event_name => EventName,
                 measurements => Measurements,
                 metadata => Metadata,
                 config => Config}),

    metrics:gauge(
      [#{name => pgec_util:snake_case(EventName),
         label => Metadata,
         value => N}]);

handle(EventName, Measurements, Metadata, Config) ->
    ?LOG_INFO(#{event_name => EventName,
                measurements => Measurements,
                metadata => Metadata,
                config => Config}).
