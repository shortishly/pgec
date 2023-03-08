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


@test "hget pub.grades.123-45-6789 last" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "Alfalfa" ]
}

@test "hget pub.grades.123-45-6789 first" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "Aloysius" ]
}

@test "hget pub.grades.123-45-6789 test1" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "40.0" ]
}

@test "hget pub.grades.123-45-6789 test2" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "90.0" ]
}

@test "hget pub.grades.123-45-6789 test3" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "100.0" ]
}

@test "hget pub.grades.123-45-6789 test4" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "83.0" ]
}

@test "hget pub.grades.123-45-6789 final" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "49.0" ]
}

@test "hget pub.grades.123-45-6789 grade" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "D-" ]
}
