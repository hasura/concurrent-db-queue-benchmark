
```sh
# start pg
docker run --rm --name postgres -e "POSTGRES_PASSWORD=password" -p 8000:5432 -d postgres

# start yb
docker run -d --name yugabyte -p7000:7000 -p9000:9000 -p5433:5433 -p9042:9042 \
  yugabytedb/yugabyte:latest bin/yugabyted start \
  --daemon=false
```
