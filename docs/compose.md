# Docker Compose

Below is a [compose.yaml][compose-spec-io] including the following
services:

- [prometheus][prometheus-io] configured using
  [prometheus.yml](../example/prometheus.yml)
- [PostgreSQL][postgresql-org] with [init.sql](../example/init.sql)
- [pgec][pgec] with a [memcached][memcached-org] and REST API.

It defines the following [profiles][compose-spec-profiles]:

- all (postgres, prometheus, pgec).
- monitoring (prometheus).
- db (postgres).
- cache (pgec).

```shell
docker compose exec postgres psql postgres postgres
```

To bring all services up:

```shell
docker compose --profile all up -d
```

To check that all services are running OK:

```shell
docker compose --profile all ps
```

The database will use a persistent volume called "pgec_db", if you
want to recreate from scratch:

```shell
docker volume rm pgec_db
```

[compose-spec-io]: https://compose-spec.io
[compose-spec-profiles]: https://github.com/compose-spec/compose-spec/blob/master/spec.md#profiles
[memcached-org]: https://memcached.org/
[pgec]: https://github.com/shortishly/pgec
[postgresql-org]: https://www.postgresql.org/
[prometheus-io]: https://prometheus.io
