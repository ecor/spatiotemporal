#!/usr/bin/env bash
# Carica i CSV prodotti da export_db_csv.R (uno per tabella di `db`) nel database.
# Eseguito dall'entrypoint postgres solo al primo avvio (volume dati vuoto).
#
# Variabili:
#   STDB_DATA_DIR  cartella con <tabella>.csv.gz o <tabella>.csv   (default /data)
#   STDB_COMPRESS  true -> comprime i chunk TimescaleDB e aggiunge la policy
set -euo pipefail

DATA_DIR="${STDB_DATA_DIR:-/data}"
TABLES=(operator project provider action variable attribute p ts)

PSQL=(psql -v ON_ERROR_STOP=1 --no-psqlrc --username "$POSTGRES_USER" --dbname "$POSTGRES_DB")

# Restituisce il file CSV per una tabella (gz preferito), o niente.
csv_for() {
  local t="$1"
  if   [ -f "$DATA_DIR/$t.csv.gz" ]; then echo "$DATA_DIR/$t.csv.gz"
  elif [ -f "$DATA_DIR/$t.csv"    ]; then echo "$DATA_DIR/$t.csv"
  fi
}

cat_csv() { case "$1" in *.gz) gzip -dc "$1" ;; *) cat "$1" ;; esac; }

if [ -z "$(csv_for ts)" ]; then
  echo "[spatiotemporal] Nessun CSV in $DATA_DIR: creato solo lo schema vuoto."
  exit 0
fi

for t in "${TABLES[@]}"; do
  f="$(csv_for "$t")"
  if [ -z "$f" ]; then
    echo "[spatiotemporal] $t: nessun file, salto."
    continue
  fi
  # L'intestazione del CSV fornisce l'elenco colonne (ordine libero).
  cols="$(set +o pipefail; cat_csv "$f" | head -n 1 | tr -d '\r"')"
  echo "[spatiotemporal] $t <- $(basename "$f")  ($cols)"
  cat_csv "$f" | "${PSQL[@]}" -c "\\copy $t ($cols) FROM STDIN WITH (FORMAT csv, HEADER true)"
done

# Allinea le sequenze agli id caricati, così i nuovi INSERT non collidono.
"${PSQL[@]}" -q -o /dev/null <<'SQL'
SELECT setval(pg_get_serial_sequence(t, 'id'), COALESCE((SELECT max(id) FROM operator ), 0) + 1, false) FROM (VALUES ('operator'))  v(t);
SELECT setval(pg_get_serial_sequence(t, 'id'), COALESCE((SELECT max(id) FROM project  ), 0) + 1, false) FROM (VALUES ('project'))   v(t);
SELECT setval(pg_get_serial_sequence(t, 'id'), COALESCE((SELECT max(id) FROM provider ), 0) + 1, false) FROM (VALUES ('provider'))  v(t);
SELECT setval(pg_get_serial_sequence(t, 'id'), COALESCE((SELECT max(id) FROM action   ), 0) + 1, false) FROM (VALUES ('action'))    v(t);
SELECT setval(pg_get_serial_sequence(t, 'id'), COALESCE((SELECT max(id) FROM variable ), 0) + 1, false) FROM (VALUES ('variable'))  v(t);
SELECT setval(pg_get_serial_sequence(t, 'id'), COALESCE((SELECT max(id) FROM attribute), 0) + 1, false) FROM (VALUES ('attribute')) v(t);
SELECT setval(pg_get_serial_sequence(t, 'id'), COALESCE((SELECT max(id) FROM p        ), 0) + 1, false) FROM (VALUES ('p'))         v(t);
SELECT setval('ts_id_seq', COALESCE((SELECT max(id) FROM ts), 0) + 1, false);
ANALYZE;
SQL

if [ "${STDB_COMPRESS:-false}" = "true" ]; then
  echo "[spatiotemporal] Compressione dei chunk più vecchi di 1 anno..."
  "${PSQL[@]}" <<'SQL'
SELECT count(compress_chunk(c, if_not_compressed => true))
  FROM show_chunks('ts', older_than => INTERVAL '1 year') c;
SELECT add_compression_policy('ts', INTERVAL '1 year', if_not_exists => true);
SQL
fi

"${PSQL[@]}" -c "SELECT 'p' AS tabella, count(*) FROM p UNION ALL SELECT 'ts', count(*) FROM ts;"
echo "[spatiotemporal] Caricamento completato."
