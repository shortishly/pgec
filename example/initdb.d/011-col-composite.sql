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


create type complex as (r double precision,
                        i double precision);

create type inventory_item as (name text,
                               supplier_id integer,
                               price numeric);

create table on_hand (id serial primary key,
                      item inventory_item,
                      count integer);

-- insert into on_hand (item, count) values (ROW('fuzzy dice', 42, 1.99), 1000);
