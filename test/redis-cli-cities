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


@test "hget pub.cities.Tulsa/OK lat_d" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "36" ]
}

@test "hget pub.cities.Tulsa/OK lat_m" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "9" ]
}

@test "hget pub.cities.Tulsa/OK lat_s" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "35" ]
}

@test "hget pub.cities.Tulsa/OK ns" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "N" ]
}

@test "hget pub.cities.Tulsa/OK lon_d" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "95" ]
}

@test "hget pub.cities.Tulsa/OK lon_m" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "54" ]
}

@test "hget pub.cities.Tulsa/OK lon_s" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "36" ]
}

@test "hget pub.cities.Tulsa/OK ew" {
    run redis-cli $BATS_TEST_DESCRIPTION
    [ "$output" = "W" ]
}
