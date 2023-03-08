#!/usr/bin/env bats
# -*- mode: shell-script; sh-shell: bash; -*-
# Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


@test "hget pub.hw_200.1 height" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "65.78" ]
}

@test "hget pub.hw_200.1 weight" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "112.99" ]
}


@test "hget pub.hw_200.17 height" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "66.46" ]
}

@test "hget pub.hw_200.17 weight" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "129.5" ]
}


@test "hget pub.hw_200.63 height" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "68.3" ]
}

@test "hget pub.hw_200.63 weight" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "129.76" ]
}
