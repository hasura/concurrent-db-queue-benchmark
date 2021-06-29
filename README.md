benchmark for concurrent work queues on databases that speak the postgres wire protocol.

intended to benchmark yugabyte against postgres.

```sh
# start pg
docker run --rm --name postgres -e "POSTGRES_PASSWORD=password" -p 8000:5432 -d postgres

# start yb
docker run -d --name yugabyte -p7000:7000 -p9000:9000 -p5433:5433 -p9042:9042 \
  yugabytedb/yugabyte:latest bin/yugabyted start \
  --daemon=false

# bench pg
cabal run -O2 -j exe:concurrent-queue-benchmark -- pg

# bench yb
cabal run -O2 -j exe:concurrent-queue-benchmark -- yb
```
