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


-module(pgec_storage_common_tests).


-include_lib("eunit/include/eunit.hrl").


bucket_test() ->
    ?assertEqual(
       <<"pgec/abc/123">>,
       pgec_storage_common:bucket(
         #{publication => <<"abc">>,
           table => <<"123">>})).


pt_test() ->
    ?assertEqual(
       {<<"abc">>, <<"123">>},
       pgec_storage_common:pt(
         #{publication => <<"abc">>,
           table => <<"123">>})).


value_test_() ->
    lists:map(
      fun
          ({Expected, [Tuple, Metadata]} = Test) ->
              {nm(Test),
               ?_assertEqual(
                  Expected,
                  case pgec_storage_common:value(Tuple, Metadata) of
                      Expected ->
                          Expected;

                      Otherwise ->
                          ?debugVal(Expected, -1),
                          ?debugVal(Otherwise, -1),
                          ?debugVal(Tuple, -1),
                          ?debugVal(Metadata, -1),
                          Otherwise
                  end)}
      end,
      [{b, [{a, b}, #{keys => [1]}]},
       {a, [{a, b}, #{keys => [2]}]},

       {{b, c}, [{a, b, c}, #{keys => [1]}]},
       {{a, c}, [{a, b, c}, #{keys => [2]}]},
       {{a, b}, [{a, b, c}, #{keys => [3]}]},

       {c, [{a, b, c}, #{keys => [1, 2]}]},
       {b, [{a, b, c}, #{keys => [1, 3]}]},
       {a, [{a, b, c}, #{keys => [2, 3]}]},

       {{c, d}, [{a, b, c, d}, #{keys => [1, 2]}]},
       {{a, d}, [{a, b, c, d}, #{keys => [2, 3]}]},
       {{a, b}, [{a, b, c, d}, #{keys => [3, 4]}]}]).


key_test_() ->
    lists:map(
      fun
          ({Expected, [Tuple, Metadata]} = Test) ->
              {nm(Test),
               ?_assertEqual(
                  Expected,
                  case pgec_storage_common:key(Tuple, Metadata) of
                      Expected ->
                          Expected;

                      Otherwise ->
                          ?debugVal(Expected, -1),
                          ?debugVal(Otherwise, -1),
                          ?debugVal(Tuple, -1),
                          ?debugVal(Metadata, -1),
                          Otherwise
                  end)}
      end,
      [{a, [{a, b, c}, #{keys => [1]}]},
       {b, [{a, b, c}, #{keys => [2]}]},
       {c, [{a, b, c}, #{keys => [3]}]},

       {{a, b}, [{a, b, c}, #{keys => [1, 2]}]},
       {{a, c}, [{a, b, c}, #{keys => [1, 3]}]},
       {{b, c}, [{a, b, c}, #{keys => [2, 3]}]},

       {{a, b}, [{a, b, c, d}, #{keys => [1, 2]}]},
       {{b, c}, [{a, b, c, d}, #{keys => [2, 3]}]},
       {{c, d}, [{a, b, c, d}, #{keys => [3, 4]}]}]).


row_test_() ->
    lists:map(
      fun
          ({Expected, [K, V, Metadata]} = Test) ->
              {nm(Test),
               ?_assertEqual(
                  Expected,
                  case pgec_storage_common:row(K, V, Metadata) of
                      Expected ->
                          Expected;

                      Otherwise ->
                          ?debugVal(Expected, -1),
                          ?debugVal(Otherwise, -1),
                          ?debugVal(K, -1),
                          ?debugVal(V, -1),
                          ?debugVal(Metadata, -1),
                          Otherwise
                  end)}
      end,
      [{{a, b, c}, [a, {b, c}, #{keys => [1]}]},
       {{a, b, c}, [b, {a, c}, #{keys => [2]}]},
       {{a, b, c}, [c, {a, b}, #{keys => [3]}]},

       {{a, b}, [a, b, #{keys => [1]}]},
       {{b, a}, [a, b, #{keys => [2]}]},

       {{a, b, c, d}, [a, {b, c, d}, #{keys => [1]}]},
       {{a, b, c, d}, [b, {a, c, d}, #{keys => [2]}]},
       {{a, b, c, d}, [c, {a, b, d}, #{keys => [3]}]},
       {{a, b, c, d}, [d, {a, b, c}, #{keys => [4]}]},

       {{a, b, c, d}, [{a, b}, {c, d}, #{keys => [1, 2]}]},
       {{a, b, c}, [{a, b}, c, #{keys => [1, 2]}]}]).


nm(Test) ->
    iolist_to_binary(io_lib:fwrite("~p", [Test])).
