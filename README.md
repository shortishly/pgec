# PostgreSQL Edge Cache (PGEC)

A JSON cache of PostgreSQL data with a simple REST API. Automatic
database replication pushes changes ensuring that `pgec` remains upto
date.

![main](https://github.com/shortishly/pgmp/actions/workflows/main.yml/badge.svg)


A simple example, with a local `postgres` via `docker` to demostrate
the concepts:

```shell
docker run \
    --rm \
    --name postgres \
    -d \
    -p 5432:5432 \
    -e POSTGRES_PASSWORD=postgres \
    postgres:14 \
    -c wal_level=logical
```

Run an interactive SQL shell so that we can create some demo data:

```shell
docker exec --interactive --tty postgres psql postgres postgres
```

Create a demo table to explore:

```sql
create table xy (x integer primary key, y text);
insert into xy values (1, 'foo');
```

With a PostgreSQL publication for that table:

```sql
create publication pub for table xy;
```

Leaving the SQL shell running, start `pgec` in another
terminal. `pgec` will act as an edge cache for publication we have
just created.

All data from the tables in the publication are retrieved, from
transaction snapshot (using an extended query with batched
execute). Once the initial data has been collected, streaming
replication is then started, receiving changes that have applied since
the transaction snapshot to ensure no loss of data. Streaming
replication continues keeping `pgec` as an upto date cache of data.

```shell
docker run \
    --rm \
    -d \
    --name pgec \
    -p 8080:80 \
    -e PGMP_REPLICATION_LOGICAL_PUBLICATION_NAMES=pub \
    -e PGMP_DATABASE_USER=postgres \
    -e PGMP_DATABASE_PASSWORD=postgres \
    -e PGMP_DATABASE_HOSTNAME=host.docker.internal \
    ghcr.io/shortishly/pgec:0.2.1
```

The manifest of `pgec` docker image versions are
[here](https://github.com/shortishly/pgec/pkgs/container/pgec).

The `-p 8080:80` option above, is linking port 8080 on localhost to port
80 on the `pgec` container. We can make http requests directly
to `pgec`.

Taking a look at the `xy` table via the JSON API:

```shell
curl http://localhost:8080/pub/xy
```
```json
{"rows": [{"x":1,"y":"foo"}]}
```

Changes that are applied to the PostgreSQL table are automatically
streamed to `pgec` and applied to the edge cache.

```sql
insert into xy values (2, 'bar');
```

Any CRUD changes to the table are automatically pushed via logical
replication to the `pgec` cache:

```shell
curl http://localhost:8080/pub/xy
```
```json
{"rows": [{"x":1, "y":"foo"}, {"x":2, "y":"bar"}]}
```

To request the value for key `2`:

```shell
curl http://localhost:8080/pub/xy/2
```
```json
{"x":2,"y":"bar"}
```

