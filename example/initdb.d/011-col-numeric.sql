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


create table col_numeric (id serial primary key,
                          sma smallint,
                          int integer,
                          big bigint,
                          dec decimal,
                          num numeric,
                          rea real,
                          dou double precision,
                          smaser smallserial,
                          ser serial,
                          bigser bigserial);

insert into col_numeric (sma, int, big, dec, num, rea, dou) values (1, 1, 1, 1, 1, 1.11, 1.11);

-- insert into col_numeric (num) values ('Infinity');
-- insert into col_numeric (num) values ('-Infinity');
-- insert into col_numeric (num) values ('NaN');
