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
PROJECT_VERSION = ${shell git describe --tags}


BUILD_DEPS += relx
RELX_TAR = 0

DEPS = \
	cowboy \
	cowboy_telemetry \
	jsx \
	mcd \
	metrics \
	pgmp

SHELL_DEPS = \
	sync

LOCAL_DEPS = \
	inets

SHELL_OPTS = \
	-config dev.config \
	-s $(PROJECT) \
	-s sync \
	+pc unicode

dep_cowboy_telemetry = git https://github.com/beam-telemetry/cowboy_telemetry.git
dep_mcd = git https://github.com/shortishly/mcd.git
dep_metrics = git https://github.com/shortishly/metrics.git
dep_pgmp = git https://github.com/shortishly/pgmp.git


dep_cowboy_commit = 2.9.0
dep_cowboy_telemetry_commit = v0.4.0
dep_jsx_commit = v3.1.0
dep_mcd_commit = 0.5.0
dep_metrics_commit = 0.2.0
dep_pgmp_commit = 0.11.0


PLT_APPS = \
	any \
	asn1 \
	bbmustache \
	compiler \
	cowboy \
	cowlib \
	crypto \
	envy \
	inets \
	jsx \
	mnesia \
	pgmp \
	phrase \
	public_key \
	ranch \
	runtime_tools \
	ssl \
	stdlib \
	syntax_tools \
	telemetry \
	tools \
	xmerl


include erlang.mk
