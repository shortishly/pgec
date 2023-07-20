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


@test "hset, hget" {
    x=$RANDOM
    y=$RANDOM
    run redis-cli hset "pub.xy.$x" y "$y"
    [ "$output" = "1" ]
    run redis-cli hget "pub.xy.$x" y
    [ "$output" = "$y" ]
}

@test "hset, hget, hset, hget" {
    x=$RANDOM
    y=$RANDOM
    run redis-cli hset "pub.xy.$x" y "$y"
    [ "$output" = "1" ]
    run redis-cli hget "pub.xy.$x" y
    [ "$output" = "$y" ]

    y=$RANDOM
    run redis-cli hset "pub.xy.$x" y "$y"
    [ "$output" = "1" ]
    run redis-cli hget "pub.xy.$x" y
    [ "$output" = "$y" ]
}

@test "exists" {
    x=$RANDOM
    y=$RANDOM
    run redis-cli hset "pub.xy.$x" y "$y"
    [ "$output" = "1" ]
    run redis-cli exists "pub.xy.$x"
    [ "$output" = "1" ]
}

@test "exists, key not found" {
    x=$RANDOM
    run redis-cli exists "pub.xy.$x"
    [ "$output" = "0" ]
}

@test "hset, del" {
    x=$RANDOM
    y=$RANDOM
    run redis-cli hset "pub.xy.$x" y "$y"
    [ "$output" = "1" ]
    run redis-cli del "pub.xy.$x"
    [ "$output" = "1" ]
}

@test "del, key not found" {
    x=$RANDOM
    run redis-cli del "pub.xy.$x"
    [ "$output" = "0" ]
}
