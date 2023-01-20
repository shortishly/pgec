# Monitoring

pgec is instrumented using [telemetry][telemetry] with a
[Prometheus][prometheus-io] http adapter listening on port 9100.

```shell
docker run \
    --rm \
    --detach \
    --name prometheus \
    --publish 9090:9090 \
    --volume $(pwd)/prometheus.yml:/etc/prometheus/prometheus.yml \
    --pull always \
    prom/prometheus
```

The following sections describe the metrics that are exposed by pgec.

## BEAM

Metrics for the BEAM itself:

* beam_memory, is a gauge recording memory usage in bytes classified
  by type. ETS is the memory store used to replicate data from
  PostgreSQL.
* beam_ports, is a gauge recording file descriptor usage.
* beam_processes, is a gauge recording the number and various types of
  processes.
  
## memcached endpoint

Metrics for the memcached protocol subsystem:

* mcd_tcp_connection_recv_bytes, in the memcached subsystem, the
  number of bytes that have been received.
* mcd_tcp_connection_send_bytes, in the memcached subsystem, the
  number of bytes that have been sent.
  
## REST endpoint

Metrics for the REST endpoint:

* cowboy_request_count, the number of HTTP requests made.
* cowboy_request_duration_ms, cumulative milliseconds taken processing
  all requests.
* cowboy_request_resp_body_length, the cumulative length of all
  response bodies.

## PostgreSQL replication

Metrics for PostgreSQL replication:
  
* pgmp_mm_execute_rows, includes the number of rows processed during
  initial data collection, prior to streaming replication starting.
* pgmp_mm_rep_keepalive_count, a counter of the keep alive requests
  that have been processed.
* pgmp_mm_rep_wal_applied, the index of the WAL that has been applied.
* pgmp_mm_rep_wal_clock, the latest clock value received from the WAL.
* pgmp_mm_rep_wal_flushed, the index of the WAL that has been flushed.
* pgmp_mm_rep_wal_received, the latest index of the WAL that been received.
* pgmp_mm_rep_begin_transaction_count, a counter of the number of
  transactions that have begun.
* pgmp_mm_rep_commit_count, a counter of the number of transactions
  that have been committed.
* pgmp_mm_rep_insert_count, a counter of the number of rows that have
  been inserted during streaming replication.
* pgmp_mm_rep_update_count, a counter of the number of rows that have
  been updated during streaming replication.
* pgmp_mm_rep_delete_count, a counter of the number of rows that have
  been deleted during streaming replication.
* pgmp_mm_rep_truncate_count, a counter of the number table truncates
  that have been processed.

[prometheus-io]: https://prometheus.io
[telemetry]: https://github.com/beam-telemetry/telemetry
