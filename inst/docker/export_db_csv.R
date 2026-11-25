## Esporta una variabile `db` di spatiotemporal (lista di tabelle) in CSV
## compressi, uno per tabella, pronti per il caricamento nel container
## PostgreSQL/PostGIS/TimescaleDB (inst/docker/initdb/30_load_db.sh).
##
## Uso:
##   Rscript export_db_csv.R                        # data(db) -> ./data
##   Rscript export_db_csv.R db_icpac_v2.rds data/  # qualsiasi .rds/.rda con lo stesso schema
##
## Conversioni:
##   - nomi colonna in minuscolo ("ID" -> "id")
##   - tabella sf (p): geometria -> colonna "geom" in EWKT, trasformata in EPSG:4326
##   - POSIXct -> ISO 8601 UTC ("1960-03-01T00:00:00Z")
##   - NA -> campo vuoto (NULL in COPY)

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
})

args <- commandArgs(trailingOnly = TRUE)

read_db <- function(f) {
  if (grepl("\\.rds$", f, ignore.case = TRUE)) return(readRDS(f))
  e <- new.env()
  nm <- load(f, envir = e)
  if ("db" %in% nm) e$db else e[[nm[1]]]
}

if (length(args) >= 1) {
  db <- read_db(args[1])
} else {
  data(db, package = "spatiotemporal")
}
out <- if (length(args) >= 2) args[2] else "data"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

tables <- c("operator", "project", "provider", "action",
            "variable", "attribute", "p", "ts")
missing <- setdiff(tables, names(db))
if (length(missing)) warning("Tabelle mancanti in db: ", paste(missing, collapse = ", "))

for (it in intersect(tables, names(db))) {
  t <- db[[it]]
  if (inherits(t, "sf")) {
    t <- sf::st_transform(t, 4326)
    geom <- sf::st_as_text(sf::st_geometry(t), EWKT = TRUE)
    t <- sf::st_drop_geometry(t)
    t <- as.data.table(t)
    t[, geom := geom]
  } else {
    t <- as.data.table(t)
  }
  setnames(t, tolower(names(t)))
  f <- file.path(out, sprintf("%s.csv.gz", it))
  data.table::fwrite(t, f, na = "", dateTimeAs = "ISO", compress = "gzip")
  message(sprintf("%-10s %10d righe -> %s", it, nrow(t), f))
}
message("Scritto in ", normalizePath(out))
