## Export a spatiotemporal `db` (named list of tables) to a Parquet folder
## readable by the HoloViz app (inst/holoviz/app.py).
## Faster alternative to `python stdb.py db.rda` (seconds instead of ~2 min).
## Uses `arrow` if installed, otherwise the lightweight `nanoparquet`
## (no C++20 compiler needed: install.packages("nanoparquet")).
##
## Usage:
##   Rscript export_db_parquet.R                       # data(db) -> ./db_parquet
##   Rscript export_db_parquet.R db_icpac_v2.rds out/  # any .rds with the same schema

library(sf)

write_pq <- if (requireNamespace("arrow", quietly = TRUE)) {
  function(x, f) arrow::write_parquet(x, f)
} else if (requireNamespace("nanoparquet", quietly = TRUE)) {
  function(x, f) nanoparquet::write_parquet(x, f)
} else {
  stop("install.packages('nanoparquet') (or 'arrow') first")
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  db <- readRDS(args[1])
} else {
  data(db, package = "spatiotemporal")
}
out <- if (length(args) >= 2) args[2] else "db_parquet"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

for (it in names(db)) {
  t <- db[[it]]
  if (inherits(t, "sf")) {
    xy <- sf::st_coordinates(sf::st_transform(t, 4326))
    t <- sf::st_drop_geometry(t)
    t$lon <- xy[, 1]
    t$lat <- xy[, 2]
  }
  t <- as.data.frame(t)
  if (it == "ts") t <- t[order(t$variable_id, t$p_id, t$timestamptz), ]
  rownames(t) <- NULL
  write_pq(t, file.path(out, sprintf("%s.parquet", it)))
  message(sprintf("%-10s %10d rows", it, nrow(t)))
}
message("written to ", normalizePath(out))
