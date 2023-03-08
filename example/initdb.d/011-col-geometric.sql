-- -*- mode: sql; sql-product: postgres; -*-
-- Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

create table col_geometric (id serial primary key,
                            a point,
                            b line,
                            c lseg,
                            d box,
                            e path,
                            f path,
                            g polygon,
                            h circle);

insert into col_geometric (a, b, c, d, e, f, g, h) values ('(1, 2)', '{1, 2, 3}', '((1, 1), (6, 6))', '((1, 2), (3, 5))', '((1, 1), (6, 9), (11, 15), (8, 11))', '[(1, 1), (6, 9), (11, 15), (8, 11)]', '((1, 1), (6, 9), (11, 15), (8, 11))', '((3, 2), 8)');
