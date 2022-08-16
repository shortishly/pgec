# PostgreSQL Edge Cache (PGEC)


A read only API for replicated tables 

Using PostgreSQL logical replication


```shell
docker run \
    --rm \
    --name postgres \
    -d \
    -p 5432:5432 \
    -e POSTGRES_PASSWORD=postgres \
    postgres:14 \
    -c shared_buffers=256MB \
    -c max_connections=200 \
    -c wal_level=logical
```


```shell
docker exec --interactive --tty postgres psql postgres postgres

psql (14.4 (Debian 14.4-1.pgdg110+1))
Type "help" for help.

postgres=#
```


```sql
create table xy (x integer primary key, y text);
insert into xy values (1, 'foo');
```

Create a PostgreSQL publication for that table:

```sql
create publication pub for table xy;
```


```shell
docker run \
    --rm \
    -p 8080:80 \
    -e PGMP_DATABASE_USER=postgres \
    -e PGMP_DATABASE_PASSWORD=postgres \
    -e PGMP_DATABASE_HOSTNAME=host.docker.internal \
    ghcr.io/shortishly/pgec:0.1.0
```


```shell
curl http://localhost:8080/pub/xy
```

```json
[{"x":1,"y":"foo"}]
```

```sql
insert into xy values (2, 'bar');
```

```shell
curl http://localhost:8080/pub/xy
```

```json
[{"x":1,"y":"foo"},{"x":2,"y":"bar"}]
```
