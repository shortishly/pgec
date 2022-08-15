#-*- mode: makefile-gmake -*-
# Copyright (c) 2022 Peter Morgan <peter.james.morgan@gmail.com>
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
#
PROJECT = pgec
PROJECT_DESCRIPTION = PostgreSQL Edge Cache
PROJECT_VERSION = 0.1.0

DEPS = \
	cowboy \
	jsx \
	pgmp

SHELL_DEPS = \
	sync

SHELL_OPTS = \
	-config dev.config \
	-s $(PROJECT) \
	-s sync \
	+pc unicode


dep_pgmp = git https://github.com/shortishly/pgmp.git


dep_cowboy_commit = 2.9.0
dep_pgmp_commit = develop


include erlang.mk


all:: dialyze eunit