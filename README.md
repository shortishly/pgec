# PostgreSQL Edge Cache (PGEC)

pgec is an Apache licensed real-time in memory PostgreSQL logical
replication cache with Redis, Memcached and REST APIs. It supports
column lists and row filters with the latest features of replication
in PostgreSQL 15.

![High Level Architecture](pgec-hla-2023-03-08.svg)

The replication process creates a transaction checkpoint ensuring data
integrity. Once the initial data has been collected, streaming
replication starts, receiving changes that have been applied
subsequent to the checkpoint, ensuring no loss of data. Streaming
replication continues keeping [pgec][shortishly-pgec] up to date in
real-time.

![main](https://github.com/shortishly/pgmp/actions/workflows/main.yml/badge.svg)

## Features

- PostgreSQL [logical replication support][pgmp] for [cache
  consistency][shortishly-ccwsr]
- [Redis compatible API][resp]
- [Memcached compatible API][mcd]
- REST API
- a [compose](docs/compose.md) having PostgreSQL with example data,
  [Grafana][grafana], and [Prometheus][prometheus-io].
- a [GitHub Codespace](docs/codespaces.md) for build and development
- Support for [row filters and column lists][shortishly-pgec] in
  PostgreSQL 15

## Quick Start

![demo](pgec-demo-compose-2023-02-22.svg)

Clone this repository for the [docker][docker-com-get-docker]
[compose.yaml](compose.yaml) with sample [PostgreSQL][postgresql-org]
data installed.

```shell
git clone https://github.com/shortishly/pgec.git
```

Alternatively, with the [Github CLI][cli-github-com] installed use:

```shell
gh repo clone shortishly/pgec
```

Change to the newly cloned directory:

```shell
cd pgec
```

Start up everything with:

```shell
docker compose --profile all up --detach --remove-orphans
```

Sample data is populated from the scripts in [this
directory](example/initdb.d), using this
[publication](example/initdb.d/020-create-publication.sql). The
compose includes a small load generator using table
`randload`. Grafana dashboards: <http://localhost:3000/>.

![Replication Dashboard](pgec-demo-grafana.gif)

The `grades` table is populated with data from:

```csv
"Last name","First name","SSN","Test1","Test2","Test3","Test4","Final","Grade"
"Alfalfa","Aloysius","123-45-6789",40.0,90.0,100.0,83.0,49.0,"D-"
"Alfred","University","123-12-1234",41.0,97.0,96.0,97.0,48.0,"D+"
"Gerty","Gramma","567-89-0123",41.0,80.0,60.0,40.0,44.0,"C"
"Android","Electric","087-65-4321",42.0,23.0,36.0,45.0,47.0,"B-"
"Bumpkin","Fred","456-78-9012",43.0,78.0,88.0,77.0,45.0,"A-"
"Rubble","Betty","234-56-7890",44.0,90.0,80.0,90.0,46.0,"C-"
"Noshow","Cecil","345-67-8901",45.0,11.0,-1.0,4.0,43.0,"F"
"Buff","Bif","632-79-9939",46.0,20.0,30.0,40.0,50.0,"B+"
"Airpump","Andrew","223-45-6789",49.0,1.0,90.0,100.0,83.0,"A"
"Backus","Jim","143-12-1234",48.0,1.0,97.0,96.0,97.0,"A+"
"Carnivore","Art","565-89-0123",44.0,1.0,80.0,60.0,40.0,"D+"
"Dandy","Jim","087-75-4321",47.0,1.0,23.0,36.0,45.0,"C+"
"Elephant","Ima","456-71-9012",45.0,1.0,78.0,88.0,77.0,"B-"
"Franklin","Benny","234-56-2890",50.0,1.0,90.0,80.0,90.0,"B-"
"George","Boy","345-67-3901",40.0,1.0,11.0,-1.0,4.0,"B"
"Heffalump","Harvey","632-79-9439",30.0,1.0,20.0,30.0,40.0,"C"
```

Betty Rubble's grades are <http://localhost:8080/pub/grades/234-56-7890>:

```shell
curl -s http://localhost:8080/pub/grades/234-56-7890 | jq
```

```json
{
  "final": 46,
  "first": "Betty",
  "grade": "C-",
  "last": "Rubble",
  "ssn": "234-56-7890",
  "test1": 44,
  "test2": 90,
  "test3": 80,
  "test4": 90
}
```

A 'C-' seems harsh, lets give her a 'C' instead:

```shell
docker compose exec \
    --no-TTY \
    postgres \
    psql \
    --command="update grades set grade='C' where ssn='234-56-7890'"
```

Fetching the same row, with the redis API instead:

```shell
redis-cli HGETALL pub.grades.234-56-7890
 1) "test4"
 2) "90"
 3) "test3"
 4) "80"
 5) "test2"
 6) "90"
 7) "test1"
 8) "44"
 9) "ssn"
10) "234-56-7890"
11) "last"
12) "Rubble"
13) "grade"
14) "C"
15) "first"
16) "Betty"
17) "final"
18) "46"
```

Fetching the same row, but with the memcached API instead:

```shell
telnet localhost 11211
Trying ::1...
Connected to localhost.
Escape character is '^]'.
get pub.grades.234-56-7890
VALUE pub.grades.234-56-7890 0 120
{"final":46,"first":"Betty","grade":"C","last":"Rubble","ssn":"234-56-7890","test1":44,"test2":90,"test3":80,"test4":90}
END
```

To retrieve the whole table via the REST API:

```shell
curl -s http://localhost:8080/pub/grades | jq
```

Primary keys via REST API:

```shell
curl -s http://localhost:8080/pub/deniro/Casino | jq
```

```json
{
  "score": 80,
  "title": "Casino",
  "year": 1995
}
```

The same via the Redis API:

```shell
redis-cli HGETALL pub.deniro.Casino

1) "year"
2) "1995"
3) "title"
4) "Casino"
5) "score"
6) "80"
```

Composite keys:

```shell
curl -s http://localhost:8080/pub/cities/Tulsa/OK | jq
```

```json
{
  "city": "Tulsa",
  "ew": "W",
  "lat_d": 36,
  "lat_m": 9,
  "lat_s": 35,
  "lon_d": 95,
  "lon_m": 54,
  "lon_s": 36,
  "ns": "N",
  "state": "OK"
}
```

The Redis compatible API will write-through to PostgreSQL on mutating
operations (e.g., `DEL` or `HSET`), and also publish notifications on
cache changes.

![redis api](/demos/pgec-redis-api-2023-03-08.svg)

Read only operations such as [EXISTS][redis-commands-exists],
[HGETALL][redis-commands-hgetall] or [HGET][redis-commands-hget] are
handled directly by the [in memory ETS cache][erlang-ets]. These commands
ultimately resolve in a call to [ets:lookup/2][erlang-ets-lookup],
with the resultant [tuple][erlang-types-tuple] converted into a
(usually) string representation for Redis.

Mutating operations, such as [DEL][redis-commands-del] or
[HSET][redis-commands-hset] automatically write through to PostgreSQL
(after checking the cache to determine whether an
[insert][postgresql-insert] or [update][postgresql-update] is the
appropriate SQL statement to use). Streaming replication updates
the in memory cache with the updated values from the database.

The real-time replication stream from PostgreSQL is also published to
subscribers with table level granularity. For example a subscription
to `__key*__:pub.grades.*` will result in any changes to the `grades`
table being published to that subscriber. Internally [erlang message
passing][erlang-message-passing] is used with a [process group per
table][erlang-org-pg]. Row level granularity is not currently
implemented, but is on the back log.

[cli-github-com]: https://cli.github.com
[docker-com-get-docker]: https://docs.docker.com/get-docker/
[erlang-ets-lookup]: https://www.erlang.org/doc/man/ets.html#lookup-2
[erlang-ets]: https://www.erlang.org/doc/man/ets.html
[erlang-message-passing]: https://www.erlang.org/blog/message-passing/#sending-messages
[erlang-org-pg]: https://www.erlang.org/doc/man/pg.html
[erlang-types-tuple]: https://www.erlang.org/doc/reference_manual/data_types.html#tuple
[grafana]: https://grafana.com/
[mcd]: https://github.com/shortishly/mcd
[pgmp]: https://github.com/shortishly/pgmp
[postgresql-insert]: https://www.postgresql.org/docs/current/sql-insert.html
[postgresql-org]: https://www.postgresql.org/
[postgresql-update]: https://www.postgresql.org/docs/current/sql-update.html
[prometheus-io]: https://prometheus.io
[redis-commands-del]: https://redis.io/commands/del/
[redis-commands-exists]: https://redis.io/commands/exists/
[redis-commands-hget]: https://redis.io/commands/hget/
[redis-commands-hgetall]: https://redis.io/commands/hgetall/
[redis-commands-hset]: https://redis.io/commands/hset/
[resp]: https://github.com/shortishly/resp
[shortishly-ccwsr]: https://shortishly.com/blog/cache-consistency-with-streaming-replication/
[shortishly-pgec]: https://shortishly.com/blog/postgresql-edge-cache/
