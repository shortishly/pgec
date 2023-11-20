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

COVER = 1
COVER_REPORT_DIR = _site/cover
CT_LOGS_DIR = _site/ct
EDOC_OPTS = {preprocess, true}, {dir, "_site/edoc"}


BUILD_DEPS += relx
RELX_TAR = 0

DEPS += cowboy
DEPS += cowboy_telemetry
DEPS += jsx
DEPS += leveled
DEPS += mcd
DEPS += metrics
DEPS += pgmp
DEPS += resp

SHELL_DEPS += sync

LOCAL_DEPS += inets

SHELL_OPTS += +pc unicode
SHELL_OPTS += -config dev.config
SHELL_OPTS += -s $(PROJECT)
SHELL_OPTS += -s sync

dep_cowboy_telemetry = git https://github.com/beam-telemetry/cowboy_telemetry.git
dep_leveled = $(if $(DEP_LN),ln ../../../martinsumner/leveled,git https://github.com/martinsumner/leveled.git)
dep_mcd = $(if $(DEP_LN),ln ../../mcd,git https://github.com/shortishly/mcd.git)
dep_metrics = $(if $(DEP_LN),ln ../../metrics,git https://github.com/shortishly/metrics.git)
dep_pgmp = $(if $(DEP_LN),ln ../../pgmp,git https://github.com/shortishly/pgmp.git)
dep_resp = $(if $(DEP_LN),ln ../../resp,git https://github.com/shortishly/resp.git)

dep_cowboy_commit = 2.10.0
dep_cowboy_telemetry_commit = v0.4.0
dep_jsx_commit = v3.1.0
dep_metrics_commit = 0.2.0


PLT_APPS += any
PLT_APPS += asn1
PLT_APPS += backoff
PLT_APPS += bbmustache
PLT_APPS += compiler
PLT_APPS += cowboy
PLT_APPS += cowlib
PLT_APPS += crypto
PLT_APPS += envy
PLT_APPS += inets
PLT_APPS += jsx
PLT_APPS += leveled
PLT_APPS += mnesia
PLT_APPS += pgmp
PLT_APPS += phrase
PLT_APPS += public_key
PLT_APPS += ranch
PLT_APPS += resp
PLT_APPS += runtime_tools
PLT_APPS += ssl
PLT_APPS += stdlib
PLT_APPS += syntax_tools
PLT_APPS += telemetry
PLT_APPS += tools
PLT_APPS += xmerl


include erlang.mk
