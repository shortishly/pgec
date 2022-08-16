# PostgreSQL Edge Cache (PGEC)

A JSON cache for your reference data, using PostgreSQL
logical replication to stay upto date.

Please follow the [Quick
Setup](https://www.postgresql.org/docs/current/logical-replication-quick-setup.html)
for logical replication on your own PostgreSQL instance.

Alternatively, with a local `postgres` via `docker`:

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

A SQL shell:

```shell
docker exec --interactive --tty postgres psql postgres postgres

psql (14.4 (Debian 14.4-1.pgdg110+1))
Type "help" for help.

postgres=#
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

Start `pgec`, acting as an edge cache for publication. All data from
the tables in the publication are retrieved, in a single transaction
(using an extended query with batched execute). Once the initial data
has been collected, streaming replication is then started, receiving
changes that have applied since the transaction snapshot to ensure no
loss of data.

```shell
docker run \
    --rm \
    -p 8080:80 \
    -e PGMP_DATABASE_USER=postgres \
    -e PGMP_DATABASE_PASSWORD=postgres \
    -e PGMP_DATABASE_HOSTNAME=host.docker.internal \
    ghcr.io/shortishly/pgec:0.1.0
```

Taking a look at the `xy` table via the JSON API:

```shell
curl http://localhost:8080/pub/xy
```
```json
[{"x":1,"y":"foo"}]
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
[{"x":1,"y":"foo"},{"x":2,"y":"bar"}]
```

To request the value for key `1`:

```shell
curl http://localhost:8080/pub/xy/2
```
```json
{"x":2,"y":"bar"}
```
