# Docker Compose

Below is a [compose.yaml][compose-spec-io] including the following
services:

- [prometheus][prometheus-io] configured using
  [prometheus.yml](../example/prometheus.yml)
- [PostgreSQL][postgresql-org] with [init.sql](../example/init.sql)
- [pgec][pgec] with a Redis, [memcached][memcached-org] and REST API.

```shell
docker compose exec db psql postgres postgres
```

To bring all services up:

```shell
docker compose \
       up \
       --detach \
       --remove-orphans
```

The database will be populated with data loaded from the scripts in
this [directory](../example/initdb.d).

To check that all services are running OK:

```shell
docker compose ps
```

You can check cached data in pgec with:

```shell
curl http://localhost:8080/pub/world_cup_teams_2022/Liberia
{"country": "Liberia",
 "fifth": "Spain",
 "fourth": "Brazil",
 "second": "Argentina",
 "third": "Portugal",
 "top_searched": "Qatar"}
```

and:

```shell
curl http://localhost:8080/pub/xy
{"rows": [{"x":1,"y":"foo"},
          {"x":2,"y":"bar"},
          {"x":4,"y":"boo"},
          {"x":3,"y":"baz"}]}

```

The database uses a persistent volume called "pgec_db", which can be
removed with:

```shell
docker volume rm pgec_db
```

To bring all services down and remove the persistent DB volume:

```shell
docker compose \
       down \
       --remove-orphans \
       --volumes
```

[compose-spec-io]: https://compose-spec.io
[memcached-org]: https://memcached.org/
[pgec]: https://github.com/shortishly/pgec
[postgresql-org]: https://www.postgresql.org/
[prometheus-io]: https://prometheus.io
