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


create table col_network (id serial primary key,
                          a cidr,
                          b inet,
                          c macaddr,
                          d macaddr8);

-- insert into col_network (a) values ('192.168.100.128/25');
-- insert into col_network (a) values ('192.168/24');
-- insert into col_network (a) values ('192.168/25');
-- insert into col_network (a) values ('192.168.1');
-- insert into col_network (a) values ('192.168');
-- insert into col_network (a) values ('128.1');
-- insert into col_network (a) values ('128');
-- insert into col_network (a) values ('128.1.2');
-- insert into col_network (a) values ('10.1.2');
-- insert into col_network (a) values ('10.1');
-- insert into col_network (a) values ('10');
-- insert into col_network (a) values ('10.1.2.3/32');
-- insert into col_network (a) values ('2001:4f8:3:ba::/64');
-- insert into col_network (a) values ('2001:4f8:3:ba:​2e0:81ff:fe22:d1f1/128');
-- insert into col_network (a) values ('::ffff:1.2.3.0/120');
-- insert into col_network (a) values ('::ffff:1.2.3.0/128');

insert into col_network (c) values ('08:00:2b:01:02:03');
insert into col_network (c) values ('08-00-2b-01-02-03');
insert into col_network (c) values ('08002b:010203');
insert into col_network (c) values ('08002b-010203');
insert into col_network (c) values ('0800.2b01.0203');
insert into col_network (c) values ('0800-2b01-0203');
insert into col_network (c) values ('08002b010203');

insert into col_network (d) values ('08:00:2b:01:02:03:04:05');
insert into col_network (d) values ('08-00-2b-01-02-03-04-05');
insert into col_network (d) values ('08002b:0102030405');
insert into col_network (d) values ('08002b-0102030405');
insert into col_network (d) values ('0800.2b01.0203.0405');
insert into col_network (d) values ('0800-2b01-0203-0405');
insert into col_network (d) values ('08002b01:02030405');
insert into col_network (d) values ('08002b0102030405');
