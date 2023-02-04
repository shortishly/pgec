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

begin;

create table randload (
  id serial primary key,
  x integer,
  uuid uuid default gen_random_uuid(),
  created_time timestamptz default now(),
  updated_time timestamptz default now()
);

insert into randload (x) select generate_series(1, 1000000);

SELECT setval('randload_id_seq', (select max(id) from randload));

commit;
