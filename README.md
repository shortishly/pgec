# PostgreSQL Edge Cache (PGEC)

A JSON cache for your PostgreSQL data using logical replication to
stay upto date.

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

Leave the SQL shell running, start `pgec` in another terminal. `pgec`
will act as an edge cache for publication we have just created. All
data from the tables in the publication are retrieved, in a single
transaction (using an extended query with batched execute). Once the
initial data has been collected, streaming replication is then
started, receiving changes that have applied since the transaction
snapshot to ensure no loss of data. Streaming replication continues
keeping `pgec` as an upto date cache of data.

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


## Container Environment

The following environment variables can be used to configure the `pgec` docker container:

|Variable | Default | Description |
|-|-|-|
|PGMP_REPLICATION_LOGICAL_PUBLICATION_NAMES | pub | Comma separated list of publication names to replicate |
|PGMP_REPLICATION_LOGICAL_MAX_ROWS | 5000 | Maximum rows to page the initial replication table state |
|PGMP_DATABASE_HOSTNAME | localhost | Database hostname |
|PGMP_DATABASE_PORT | 5432 | Database port |
|PGMP_DATABASE_USER | `os:getenv("USER")` | Database username |
|PGMP_DATABASE_PASSWORD | "" | Database password |
|PGMP_DATABASE_NAME | PGMP_DATABASE_USER | Database name |
