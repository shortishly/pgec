# Redis Protocol

RESP (the Redis Protocol) is handled by
[pgec_resp_emulator](src/pgec_resp_emulator.erl), supporting the
following commands.

## server

### info

Get (very basic) information about the server. Note that statistics
are available via the Prometheus and Grafana interfaces.

```shell
$ redis-cli info
# Server
redis_version:0.2.0-1-g525aba8
```

### command

Get an array of command details. Currently returns an empty array,
to support command line tools that expect this command to be
available.

```shell
$ redis-cli command
(empty array)
```

## connection

### ping

Ping the server.

```shell
$ redis-cli ping
pong
```

```shell
$ redis-cli ping "hello world"
"hello world"
```

### hello

Handshake with the server.

## generic

### del

Delete a key.

### exists

Determine if a key exists.

```shell
$ redis-cli exists pub.grades.234-56-7890
(integer) 1
```

## hash

### hexists

Determine if a hash field exists.

```shell
$ redis-cli hexists pub.grades.234-56-7890 test1
(integer) 1

$ redis-cli hexists pub.grades.234-56-7890 test6
(integer) 0
```

### hget

Get the value of a hash field stored in memory.

```shell
$ redis-cli hget pub.grades.234-56-7890 test1
"44.0"
```

Returns `nil` when the key is not present, or the field is not present
in the hash.

```shell
$ redis-cli hget pub.grades.234-56-7890 test6
(nil)
```

### hgetall

Get all fields and values in a hash for a key stored in memory.

```shell
$ redis-cli hgetall pub.grades.234-56-7890
 1) "test4"
 2) "90.0"
 3) "test3"
 4) "80.0"
 5) "test2"
 6) "90.0"
 7) "test1"
 8) "44.0"
 9) "ssn"
10) "234-56-7890"
11) "last"
12) "Rubble"
13) "grade"
14) "C-"
15) "first"
16) "Betty"
17) "final"
18) "46.0"
```

### hkeys

Get all the fields in a hash.

```shell
$ redis-cli hkeys pub.grades.234-56-7890
1) "test4"
2) "test3"
3) "test2"
4) "test1"
5) "ssn"
6) "last"
7) "grade"
8) "first"
9) "final"
```

An empty array is returned when the key is not present.

```shell
$ redis-cli hkeys pub.grades.234-56-7823
(empty array)
```

### hlen

Get the number of fields in a hash.

```shell
$ redis-cli hlen pub.grades.234-56-7890
(integer) 9
```

Zero is returned when the key is not present.

```shell
$ redis-cli hlen pub.grades.234-56-8790 
(integer) 0
```

### hset

Set the string value of a hash field.

#### insert

A SQL statement is executed by pgec to insert the row into the
underlying PostgreSQL table. The inserted row is then replicated
updating the in memory cache.

```shell
redis-cli hset pub.grades.321-21-4321 last Zella first Xiggy test1 47 test2 68 test3 54 test4 73 final 23 grade D+
```

#### update

A SQL statement is executed by pgec to update the row in the
underlying PostgreSQL table. The updated row is then replicated
updating the in memory cache.

```shell
$ redis-cli hset pub.grades.234-56-7890 test1 45.0
(integer) 1
```

Zero is returned if the field is not a member of the hash.

## pubsub

### psubscribe

```shell
$ redis-cli psubscribe '__key*__:pub.grades.*'

Reading messages... (press Ctrl-C to quit)
1) "subscribe"
2) "__key*__:pub.grades.*"
3) (integer) 1
```

In another shell update one of the grades:

```shell
redis-cli hset pub.grades.234-56-7890 test1 45.0
(integer) 1
```

Or via SQL:

```shell
docker compose exec \
    --no-TTY \
    postgres \
    psql \
    --command="update grades set grade='C' where ssn='234-56-7890'"
```

A message will be streamed to the subscriber detailing the affected key.

```shell
1) "message"
2) "__keyspace@0__:pub.grades.234-56-7890"
3) "set"
```
