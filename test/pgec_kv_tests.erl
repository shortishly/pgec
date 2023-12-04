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


-module(pgec_kv_tests).


-include_lib("eunit/include/eunit.hrl").


value_test_() ->
    lists:map(
      fun
          ({Expected, [ContentType, ColumnType, Value]} = Test) ->
              {nm(Test),
               ?_assertEqual(
                  Expected,
                  case pgec_kv:value(ContentType, ColumnType, Value) of
                      Expected ->
                          Expected;
                      Otherwise ->
                          ?debugVal(Expected, -1),
                          ?debugVal(Otherwise, -1),
                          ?debugVal(ContentType, -1),
                          ?debugVal(ColumnType, -1),
                          ?debugVal(Value, -1),
                          Otherwise
                  end)}
      end,
      phrase_file:consult("test/kv-value.terms")).


row_test_() ->
    Types = maps:from_list(phrase_file:consult("test/type.terms")),
    lists:map(
      fun
          ({Expected, [Metadata, ContentType, Values]} = Test) ->
              {nm(Test),
               ?_assertEqual(
                  Expected,
                  case pgec_kv:row(Metadata,
                                   ContentType,
                                   Values,
                                   Types) of

                      Expected ->
                          Expected;

                      Otherwise ->
                          ?debugVal(Expected, -1),
                          ?debugVal(Otherwise, -1),
                          ?debugVal(ContentType, -1),
                          ?debugVal(Values, -1),
                          Otherwise
                  end)}
      end,
      phrase_file:consult("test/kv-row.terms")).


nm(Test) ->
    iolist_to_binary(io_lib:fwrite("~p", [Test])).
