# spatiotemporal-db: PostgreSQL + PostGIS + TimescaleDB
# per lo schema `db` del pacchetto R spatiotemporal.
#
# Build:   docker build -t spatiotemporal-db inst/docker
# Avvio:   vedi docker-compose.yml e README.md
#
# Immagine base: PostgreSQL 17 + PostGIS 3.5 (Debian bookworm).
ARG BASE_IMAGE=postgis/postgis:17-3.5
FROM ${BASE_IMAGE}

LABEL org.opencontainers.image.title="spatiotemporal-db" \
      org.opencontainers.image.description="PostgreSQL/PostGIS/TimescaleDB per lo schema db del pacchetto R spatiotemporal" \
      org.opencontainers.image.source="https://github.com/ecor/spatiotemporal" \
      org.opencontainers.image.licenses="EUPL-1.2" \
      org.opencontainers.image.authors="Emanuele Cordano <emanuele.cordano@gmail.com>"

# --- TimescaleDB dal repository ufficiale (packagecloud) ----------------------
# PG_MAJOR è già definita dall'immagine postgres ufficiale (qui 17).
RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends ca-certificates curl gnupg; \
    . /etc/os-release; \
    curl -fsSL https://packagecloud.io/timescale/timescaledb/gpgkey \
      | gpg --dearmor -o /usr/share/keyrings/timescaledb.gpg; \
    echo "deb [signed-by=/usr/share/keyrings/timescaledb.gpg] https://packagecloud.io/timescale/timescaledb/debian/ ${VERSION_CODENAME} main" \
      > /etc/apt/sources.list.d/timescaledb.list; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
      "timescaledb-2-postgresql-${PG_MAJOR}" \
      timescaledb-tools; \
    apt-get purge -y --auto-remove gnupg; \
    rm -rf /var/lib/apt/lists/*

# TimescaleDB deve essere caricato all'avvio del server.
# Si modifica il template usato da initdb, così vale per ogni nuovo cluster.
RUN set -eux; \
    sed -ri "s/^#?(shared_preload_libraries)\s*=.*/\1 = 'timescaledb'/" \
      /usr/share/postgresql/postgresql.conf.sample; \
    { echo "timescaledb.telemetry_level = off"; } >> /usr/share/postgresql/postgresql.conf.sample; \
    grep -E "^shared_preload_libraries" /usr/share/postgresql/postgresql.conf.sample

# --- Script di inizializzazione ----------------------------------------------
# Eseguiti (in ordine alfabetico) solo al PRIMO avvio, con volume dati vuoto.
# 10_postgis.sh è già fornito dall'immagine postgis/postgis.
COPY initdb/20_schema.sql  /docker-entrypoint-initdb.d/20_schema.sql
COPY initdb/30_load_db.sh  /docker-entrypoint-initdb.d/30_load_db.sh
RUN chmod 0755 /docker-entrypoint-initdb.d/30_load_db.sh

# Cartella dove montare i CSV prodotti da export_db_csv.R (sola lettura).
ENV STDB_DATA_DIR=/data \
    STDB_COMPRESS=false
RUN mkdir -p /data

HEALTHCHECK --interval=30s --timeout=5s --start-period=60s --retries=5 \
  CMD pg_isready -U "${POSTGRES_USER:-postgres}" -d "${POSTGRES_DB:-postgres}" || exit 1

EXPOSE 5432
