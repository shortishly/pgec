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


create table col_bitstring (id serial primary key,
                            a bit(3),
                            b bit varying (5));

insert into col_bitstring (a, b) values (b'101', b'00');
insert into col_bitstring (a, b) values (b'000', b'101');
insert into col_bitstring (a, b) values (b'100', b'101');
