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


@test "get pub.col_boolean.0" {
    run bash -c "echo $BATS_TEST_DESCRIPTION | nc -C -q 5 localhost 11211 | sed 's/\r$//'"
    [ "${lines[0]}" = "END" ]
}

@test "get pub.col_boolean.1" {
    run bash -c "echo $BATS_TEST_DESCRIPTION | nc -C -q 5 localhost 11211 | sed 's/\r$//'"
    [ "${lines[0]}" = "VALUE pub.col_boolean.1 0 17" ]
    [ "${lines[1]}" = "{\"a\":true,\"id\":1}" ]
    [ "${lines[2]}" = "END" ]
}
