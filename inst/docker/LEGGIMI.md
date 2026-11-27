# spatiotemporal-db: PostgreSQL + PostGIS + TimescaleDB

*English version: [README.md](README.md)*

Immagine Docker che crea un database PostgreSQL 17 / PostGIS 3.5 / TimescaleDB 2
con lo schema della variabile `db` del pacchetto R **spatiotemporal** e,
al primo avvio, lo popola con i dati esportati da R.

```
inst/docker/
├── Dockerfile              # postgres:17-bookworm + PostGIS (PGDG) + timescaledb (packagecloud)
├── docker-compose.yml      # servizio "stdb", volume persistente, porta su localhost
├── .env.example            # credenziali e parametri (copiare in .env)
├── export_db_csv.R         # db (R) -> data/<tabella>.csv.gz
└── initdb/
    ├── 20_schema.sql       # tabelle, FK, indici, hypertable, viste
    └── 30_load_db.sh       # COPY dei CSV, allineamento sequenze, compressione opzionale
```

## Schema

| R (`data(db)`)    | PostgreSQL  | Note                                                        |
|-------------------|-------------|-------------------------------------------------------------|
| `db$operator`     | `operator`  |                                                             |
| `db$project`      | `project`   |                                                             |
| `db$provider`     | `provider`  |                                                             |
| `db$action`       | `action`    | FK → `operator`, `project`                                  |
| `db$p` (sf)       | `p`         | `geom geometry(Point,4326)` + indice GiST; FK → `provider`  |
| `db$variable`     | `variable`  | FK → `provider`; unico (provider_id, name)                  |
| `db$attribute`    | `attribute` |                                                             |
| `db$ts`           | `ts`        | **hypertable** su `timestamptz` (chunk 5 anni); FK → `p`, `variable`, `attribute`, `action` |

* Le colonne `ID` diventano `id` (minuscolo, niente virgolette in SQL).
* La chiave primaria di `ts` è `(variable_id, p_id, timestamptz)`: TimescaleDB
  richiede che ogni vincolo unico contenga la colonna temporale. L'`id` di R
  resta come colonna con sequenza.
* Viste pronte: `v_p` (stazioni con provider, lon, lat) e `v_ts` (serie con
  nomi di stazione, variabile e attributo, l'equivalente di `get_ts()`).
* Compressione colonnare configurata (`segmentby = variable_id, p_id`) e
  applicata solo se `STDB_COMPRESS=true`.

## Installazione sul server (rendena103.cloud o rendena101.cloud)

Prerequisiti: Docker Engine con il plugin `docker compose` (Ubuntu:
`sudo apt install docker.io docker-compose-v2`), R con `sf` e `data.table`
(solo per l'esportazione, che si può fare anche su un altro PC copiando poi `data/`).

```bash
git clone https://github.com/ecor/spatiotemporal.git
cd spatiotemporal/inst/docker

# 1. credenziali
cp .env.example .env
nano .env                       # almeno POSTGRES_PASSWORD

# 2. dati: db del pacchetto (dataset GSOD Tanganyika, ~7.6 M righe, ~45 MB gz)
Rscript export_db_csv.R ../../data/db.rda data
#    oppure un altro db con lo stesso schema:
#    Rscript export_db_csv.R /percorso/db_icpac_v2.rds data

# 3. build e avvio (il primo caricamento richiede qualche minuto)
docker compose up -d --build
docker compose logs -f stdb     # attendere "[spatiotemporal] Caricamento completato."
```

Gli script in `initdb/` girano **solo al primo avvio**, quando il volume
`pgdata` è vuoto. Per ricaricare da zero:

```bash
docker compose down -v          # ATTENZIONE: cancella il volume con il database
docker compose up -d
```

Senza CSV in `data/` viene creato solo lo schema vuoto.

### Solo Dockerfile (senza compose)

```bash
docker build -t spatiotemporal-db inst/docker
docker run -d --name stdb \
  -e POSTGRES_DB=spatiotemporal -e POSTGRES_USER=ecor -e POSTGRES_PASSWORD=... \
  -v stdb_pgdata:/var/lib/postgresql/data \
  -v "$PWD/inst/docker/data":/data:ro \
  -p 127.0.0.1:5432:5432 spatiotemporal-db
```

## Accesso

Dal server:

```bash
docker exec -it stdb psql -U ecor -d spatiotemporal
```

Dal proprio PC, con un tunnel SSH (la porta non è esposta su internet):

```bash
ssh -N -L 5432:localhost:5432 utente@rendena103.cloud
```

Da R:

```r
library(DBI); library(sf)
con <- dbConnect(RPostgres::Postgres(), host = "localhost", port = 5432,
                 dbname = "spatiotemporal", user = "ecor",
                 password = Sys.getenv("STDB_PASSWORD"))

p  <- st_read(con, query = "SELECT * FROM p")            # oggetto sf, EPSG:4326
ts <- dbGetQuery(con, "
  SELECT timestamptz, value FROM v_ts
  WHERE station LIKE '%KIGOMA%' AND variable = 'PRCP'
  ORDER BY timestamptz")

# stazioni entro 100 km da un punto
near <- st_read(con, query = "
  SELECT * FROM v_p
  WHERE ST_DWithin(geom::geography, ST_MakePoint(29.5, -4.9)::geography, 100000)")

# aggregazione mensile con TimescaleDB
m <- dbGetQuery(con, "
  SELECT time_bucket('1 month', timestamptz) AS month, p_id, sum(value) AS prcp
  FROM ts WHERE variable_id = 11 GROUP BY 1, 2 ORDER BY 1")
```

### Con le funzioni di spatiotemporal

La connessione si usa direttamente al posto di `data(db)`:

```r
library(spatiotemporal)
con <- stdb_connect(host = "localhost", port = 5432, dbname = "spatiotemporal",
                    user = "ecor", password = Sys.getenv("STDB_PASSWORD"))
prcp <- get_ts(x = con, p_id = 49, get_variable_args = list(name = "PRCP"))
```

## Sicurezza

* Usare una password robusta in `.env` e non mettere `.env` in git.
* Se serve aprire la porta (`STDB_BIND=0.0.0.0`), limitare gli IP con il
  firewall del server (`ufw`) o del pannello Aruba. Attenzione: Docker
  scavalca `ufw` per le porte pubblicate, quindi conviene restare su
  `127.0.0.1` e usare il tunnel SSH.
* Backup: `docker exec stdb pg_dump -U ecor -Fc spatiotemporal > stdb.dump`.
