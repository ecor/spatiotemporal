# spatiotemporal-db: PostgreSQL + PostGIS + TimescaleDB

*Versione italiana: [LEGGIMI.md](LEGGIMI.md)*

Docker image that creates a PostgreSQL 17 / PostGIS 3.5 / TimescaleDB 2
database with the schema of the `db` variable of the R package
**spatiotemporal** and, on first start, fills it with the data exported from R.

```
inst/docker/
├── Dockerfile              # postgres:17-bookworm + PostGIS (PGDG) + timescaledb (packagecloud)
├── docker-compose.yml      # "stdb" service, persistent volume, port on localhost
├── .env.example            # credentials and parameters (copy to .env)
├── export_db_csv.R         # db (R) -> data/<table>.csv.gz
└── initdb/
    ├── 20_schema.sql       # tables, FKs, indexes, hypertable, views
    └── 30_load_db.sh       # COPY of the CSV files, sequence alignment, optional compression
```

## Schema

| R (`data(db)`)    | PostgreSQL  | Notes                                                       |
|-------------------|-------------|-------------------------------------------------------------|
| `db$operator`     | `operator`  |                                                             |
| `db$project`      | `project`   |                                                             |
| `db$provider`     | `provider`  |                                                             |
| `db$action`       | `action`    | FK → `operator`, `project`                                  |
| `db$p` (sf)       | `p`         | `geom geometry(Point,4326)` + GiST index; FK → `provider`   |
| `db$variable`     | `variable`  | FK → `provider`; unique (provider_id, name)                 |
| `db$attribute`    | `attribute` |                                                             |
| `db$ts`           | `ts`        | **hypertable** on `timestamptz` (5-year chunks); FK → `p`, `variable`, `attribute`, `action` |

* `ID` columns become `id` (lower case, no quoting needed in SQL).
* The primary key of `ts` is `(variable_id, p_id, timestamptz)`: TimescaleDB
  requires every unique constraint to include the time column. The R `id`
  is kept as a column backed by a sequence.
* Ready-made views: `v_p` (stations with provider, lon, lat) and `v_ts`
  (time series with station, variable and attribute names, the equivalent
  of `get_ts()`).
* Columnar compression is configured (`segmentby = variable_id, p_id`) and
  applied only if `STDB_COMPRESS=true`.

## Installation on the server (rendena103.cloud or rendena101.cloud)

Requirements: Docker Engine with the `docker compose` plugin (Ubuntu:
`sudo apt install docker.io docker-compose-v2`), R with `sf` and `data.table`
(only for the export, which can also be run on another machine, copying
`data/` afterwards).

```bash
git clone https://github.com/ecor/spatiotemporal.git
cd spatiotemporal/inst/docker

# 1. credentials
cp .env.example .env
nano .env                       # at least POSTGRES_PASSWORD

# 2. data: the package db (GSOD Lake Tanganyika dataset, ~7.6 M rows, ~45 MB gz)
Rscript export_db_csv.R ../../data/db.rda data
#    or any other db with the same schema:
#    Rscript export_db_csv.R /path/to/db_icpac_v2.rds data

# 3. build and start (the first load takes a few minutes)
docker compose up -d --build
docker compose logs -f stdb     # wait for "[spatiotemporal] Caricamento completato."
```

The scripts in `initdb/` run **only on the first start**, when the `pgdata`
volume is empty. To reload from scratch:

```bash
docker compose down -v          # WARNING: deletes the volume holding the database
docker compose up -d
```

Without CSV files in `data/`, only the empty schema is created.

### Dockerfile only (without compose)

```bash
docker build -t spatiotemporal-db inst/docker
docker run -d --name stdb \
  -e POSTGRES_DB=spatiotemporal -e POSTGRES_USER=ecor -e POSTGRES_PASSWORD=... \
  -v stdb_pgdata:/var/lib/postgresql/data \
  -v "$PWD/inst/docker/data":/data:ro \
  -p 127.0.0.1:5432:5432 spatiotemporal-db
```

## Access

From the server:

```bash
docker exec -it stdb psql -U ecor -d spatiotemporal
```

From your own computer, through an SSH tunnel (the port is not exposed to the internet):

```bash
ssh -N -L 5432:localhost:5432 user@rendena103.cloud
```

From R:

```r
library(DBI); library(sf)
con <- dbConnect(RPostgres::Postgres(), host = "localhost", port = 5432,
                 dbname = "spatiotemporal", user = "ecor",
                 password = Sys.getenv("STDB_PASSWORD"))

p  <- st_read(con, query = "SELECT * FROM p")            # sf object, EPSG:4326
ts <- dbGetQuery(con, "
  SELECT timestamptz, value FROM v_ts
  WHERE station LIKE '%KIGOMA%' AND variable = 'PRCP'
  ORDER BY timestamptz")

# stations within 100 km of a point
near <- st_read(con, query = "
  SELECT * FROM v_p
  WHERE ST_DWithin(geom::geography, ST_MakePoint(29.5, -4.9)::geography, 100000)")

# monthly aggregation with TimescaleDB
m <- dbGetQuery(con, "
  SELECT time_bucket('1 month', timestamptz) AS month, p_id, sum(value) AS prcp
  FROM ts WHERE variable_id = 11 GROUP BY 1, 2 ORDER BY 1")
```

### With the spatiotemporal functions

The connection can be used directly in place of `data(db)`:

```r
library(spatiotemporal)
con <- stdb_connect(host = "localhost", port = 5432, dbname = "spatiotemporal",
                    user = "ecor", password = Sys.getenv("STDB_PASSWORD"))
prcp <- get_ts(x = con, p_id = 49, get_variable_args = list(name = "PRCP"))
```

## Security

* Use a strong password in `.env` and never commit `.env` to git.
* If you need to open the port (`STDB_BIND=0.0.0.0`), restrict the allowed
  IPs with the server firewall (`ufw`) or the Aruba control panel. Note that
  Docker bypasses `ufw` for published ports, so it is safer to keep
  `127.0.0.1` and use the SSH tunnel.
* Backup: `docker exec stdb pg_dump -U ecor -Fc spatiotemporal > stdb.dump`.
