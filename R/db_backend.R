NULL
## =============================================================================
## Backend PostgreSQL / PostGIS / TimescaleDB per spatiotemporal
##
## In tutte le funzioni del pacchetto l'argomento `x` (o `db`) puo' essere:
##   - un oggetto R (lista di tabelle, come data(db));
##   - una connessione DBI (DBIConnection) a un database PostgreSQL che usa
##     lo schema di inst/docker/initdb/20_schema.sql.
##
## Convenzioni tra i due backend:
##   - in R la chiave primaria si chiama "ID", in PostgreSQL "id": la
##     conversione e' automatica in lettura, scrittura e nei filtri;
##   - la geometria della tabella p si chiama "geometry" in R e "geom" in
##     PostgreSQL (EPSG:4326);
##   - i filtri (argomento vfilter e argomenti ...) sono tradotti in SQL con
##     dbplyr ed eseguiti dal database: in R arrivano solo le righe richieste.
## =============================================================================

#' Connect to a spatiotemporal PostgreSQL/PostGIS/TimescaleDB database
#'
#' Opens a DBI connection to a database built with the schema in
#' \code{inst/docker/initdb/20_schema.sql} (e.g. the Docker image in
#' \code{inst/docker}). The returned connection can be used as argument
#' \code{x} (or \code{db}) of all the functions of the package, in place of
#' the R object \code{\link{db}}.
#'
#' @param dbname,host,port,user,password connection parameters. Defaults are
#'   taken from the standard PostgreSQL environment variables
#'   (\code{PGDATABASE}, \code{PGHOST}, \code{PGPORT}, \code{PGUSER},
#'   \code{PGPASSWORD}); \code{password} can also be stored in \code{~/.pgpass}.
#' @param driver DBI driver package: \code{"RPostgres"} (default, if installed)
#'   or \code{"RPostgreSQL"}.
#' @param ... further arguments passed to \code{\link[DBI]{dbConnect}}.
#'
#' @return a \code{DBIConnection} object. Close it with
#'   \code{DBI::dbDisconnect()}.
#'
#' @seealso \code{\link{is_db_connection}}, \code{\link{write_db_to_postgres}},
#'   \code{\link{read_db_from_postgres}}
#' @export
#'
#' @examples
#' \dontrun{
#' ## e.g. through an SSH tunnel to rendena103.cloud:
#' ##   ssh -N -L 5432:127.0.0.1:5432 ecor@rendena103.cloud
#' con <- stdb_connect(dbname = "spatiotemporal", user = "ecor")
#' variables <- get_variable(x = con)
#' prcp <- get_ts(x = con, p_id = 49, get_variable_args = list(name = "PRCP"))
#' DBI::dbDisconnect(con)
#' }
#'
stdb_connect <- function(dbname = Sys.getenv("PGDATABASE", "spatiotemporal"),
                         host = Sys.getenv("PGHOST", "localhost"),
                         port = as.integer(Sys.getenv("PGPORT", "5432")),
                         user = Sys.getenv("PGUSER", Sys.getenv("USER")),
                         password = Sys.getenv("PGPASSWORD", NA),
                         driver = c("RPostgres", "RPostgreSQL"), ...) {
  check_db_packages()
  driver <- driver[sapply(driver, requireNamespace, quietly = TRUE)]
  if (length(driver) == 0) {
    stop("Install one of the packages 'RPostgres' (recommended) or 'RPostgreSQL'.")
  }
  driver <- driver[1]
  args <- list(dbname = dbname, host = host, port = port, user = user, ...)
  if (!is.na(password) && nchar(password) > 0) args$password <- password
  if (driver == "RPostgres") {
    args <- c(list(drv = RPostgres::Postgres()), args)
    if (is.null(args$timezone)) args$timezone <- "UTC"
  } else {
    args <- c(list(drv = RPostgreSQL::PostgreSQL()), args)
  }
  con <- do.call(DBI::dbConnect, args)
  DBI::dbExecute(con, "SET TIME ZONE 'UTC'")
  con
}

#' Is the database a DBI connection?
#'
#' @param x object, e.g. \code{\link{db}} or the result of
#'   \code{\link{stdb_connect}}.
#' @return \code{TRUE} if \code{x} is a \code{DBIConnection} (database
#'   backend), \code{FALSE} if it is an R object.
#' @export
#' @examples
#' data(db)
#' is_db_connection(db)
#'
is_db_connection <- function(x) inherits(x, "DBIConnection")

## ---------------------------------------------------------------- internals

check_db_packages <- function(pkgs = c("DBI", "dbplyr")) {
  miss <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(miss)) {
    stop("The database backend needs the package(s): ",
         paste(miss, collapse = ", "), ". Please install them.")
  }
  invisible(TRUE)
}

## nomi R -> nomi PostgreSQL ("ID" -> "id") in nomi di colonna e filtri
r2pg_names <- function(x) {
  x[x == "ID"] <- "id"
  x
}
r2pg_filter <- function(x) gsub("\\bID\\b", "id", x, perl = TRUE)

## nomi PostgreSQL -> nomi R
pg2r_df <- function(x) {
  names(x)[names(x) == "id"] <- "ID"
  x
}

## colonne geometriche di una tabella (PostGIS)
db_geometry_columns <- function(con, table_name) {
  q <- sprintf(
    "SELECT f_geometry_column FROM geometry_columns WHERE f_table_schema = current_schema() AND f_table_name = %s",
    DBI::dbQuoteString(con, table_name))
  out <- tryCatch(DBI::dbGetQuery(con, q)[[1]], error = function(e) character(0))
  as.character(out)
}

## elenco tabelle e viste dello schema corrente
db_tables <- function(con) {
  DBI::dbGetQuery(con, paste(
    "SELECT table_name FROM information_schema.tables",
    "WHERE table_schema = current_schema()"))$table_name
}

## tipi delle colonne (per i cast in scrittura)
db_column_types <- function(con, table_name) {
  q <- sprintf(paste(
    "SELECT column_name, data_type, udt_name FROM information_schema.columns",
    "WHERE table_schema = current_schema() AND table_name = %s",
    "ORDER BY ordinal_position"), DBI::dbQuoteString(con, table_name))
  DBI::dbGetQuery(con, q)
}

## filtro spaziale (sf, sfc, SpatRaster, SpatVector, bbox...) -> EWKT EPSG:4326
spatial_filter_ewkt <- function(p, crop = FALSE) {
  if (inherits(p, c("sf", "sfc")) && !crop) {
    g <- p |> sf::st_as_sf() |> sf::st_geometry() |> sf::st_union()
  } else {
    if (inherits(p, c("SpatRaster", "SpatVector", "SpatExtent"))) {
      if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' needed.")
      if (inherits(p, "SpatExtent")) {
        e <- as.vector(p)
        crs <- sf::NA_crs_
      } else {
        e <- as.vector(terra::ext(p))
        crs <- sf::st_crs(terra::crs(p))
      }
      bb <- sf::st_bbox(c(xmin = e[["xmin"]], ymin = e[["ymin"]],
                          xmax = e[["xmax"]], ymax = e[["ymax"]]), crs = crs)
    } else {
      bb <- sf::st_bbox(p)
    }
    g <- sf::st_as_sfc(bb)
  }
  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  g <- sf::st_transform(g, 4326)
  sf::st_as_text(g, EWKT = TRUE)
}

## Lettura di una tabella dal database con filtri eseguiti lato server.
## spatial_ewkt: geometria (EWKT) per ST_Intersects sulla colonna geometrica.
db_get_table <- function(con, table_name = "ts", vfilter = NULL,
                         data.table = (table_name != "p"),
                         sf = (table_name == "p"),
                         further_args = list(), spatial_ewkt = NULL,
                         verbose = FALSE) {
  check_db_packages()
  tcols <- db_column_types(con, table_name)$column_name
  if (length(tcols) == 0) stop("Table '", table_name, "' not found in the database.")
  out <- dplyr::tbl(con, table_name, vars = tcols)

  if (length(vfilter) > 0) for (i in seq_along(vfilter)) {
    condition <- r2pg_filter(vfilter[[i]])
    out <- out |> dplyr::filter(!!rlang::parse_expr(condition))
  }

  lfargs <- sapply(further_args, length)
  further_args <- further_args[which(lfargs > 0)]
  if (length(further_args) > 0) {
    names(further_args) <- r2pg_names(names(further_args))
    for (it in names(further_args)) {
      values <- suppressWarnings(as.numeric(further_args[[it]]))
      if (any(is.na(values) & !is.na(further_args[[it]]))) values <- further_args[[it]]
      values <- unique(values)
      out <- out |> dplyr::filter(!!rlang::sym(it) %in% !!values)
    }
  }

  geom_cols <- db_geometry_columns(con, table_name)
  sql <- dbplyr::sql_render(out)

  if (length(geom_cols) > 0) {
    gcol <- geom_cols[1]
    qgcol <- DBI::dbQuoteIdentifier(con, gcol)
    where <- ""
    if (!is.null(spatial_ewkt)) {
      where <- sprintf("WHERE ST_Intersects(q.%s, ST_GeomFromEWKT(%s))",
                       qgcol, DBI::dbQuoteString(con, spatial_ewkt))
    }
    ## la colonna geometrica viaggia come EWKB esadecimale (indipendente dal driver)
    ocols <- setdiff(tcols, gcol)
    ocols <- paste(paste0("q.", DBI::dbQuoteIdentifier(con, ocols)), collapse = ", ")
    sql <- sprintf(
      "SELECT %s, encode(ST_AsEWKB(q.%s), 'hex') AS stdb_geom_hex__ FROM (%s) q %s",
      ocols, qgcol, sql, where)
  } else if (!is.null(spatial_ewkt)) {
    stop("Table '", table_name, "' has no geometry column: spatial filter not possible.")
  }
  if (verbose) message(sql)

  res <- DBI::dbGetQuery(con, sql)
  res <- pg2r_df(res)

  if (length(geom_cols) > 0) {
    hex <- res$stdb_geom_hex__
    res$stdb_geom_hex__ <- NULL
    geometry <- sf::st_as_sfc(structure(as.list(hex), class = "WKB"), EWKB = TRUE)
    if (is.na(sf::st_crs(geometry))) sf::st_crs(geometry) <- 4326
    if (sf) {
      res$geometry <- geometry
      return(sf::st_as_sf(res, sf_column_name = "geometry"))
    }
    res$geometry <- geometry
  }

  if (data.table) {
    res <- data.table::as.data.table(res)
  } else if (sf) {
    res <- sf::st_as_sf(res)
  }
  res
}

## valori R -> testo accettato da PostgreSQL (per i cast lato server)
to_pg_text <- function(v) {
  if (inherits(v, "POSIXt")) {
    out <- format(as.POSIXct(v), "%Y-%m-%d %H:%M:%OS6+00", tz = "UTC")
    out[is.na(v)] <- NA
    return(out)
  }
  if (inherits(v, "Date")) return(ifelse(is.na(v), NA, format(v)))
  if (is.factor(v)) return(as.character(v))
  if (is.logical(v)) return(ifelse(is.na(v), NA, ifelse(v, "true", "false")))
  if (is.numeric(v)) {
    out <- format(v, scientific = FALSE, trim = TRUE, digits = 15)
    out[is.na(v)] <- NA
    return(out)
  }
  as.character(v)
}

## Inserisce le righe di x (data.frame o sf, nomi gia' in stile PostgreSQL)
## nella tabella table_name, con una tabella temporanea e i cast ai tipi del
## database. Restituisce gli id inseriti.
db_insert <- function(con, x, table_name, keep_id = FALSE) {
  types <- db_column_types(con, table_name)
  if (nrow(types) == 0) stop("Table '", table_name, "' not found in the database.")
  geom_cols <- types$column_name[types$udt_name == "geometry"]

  if (inherits(x, "sf")) {
    x <- sf::st_transform(x, 4326)
    ewkt <- sf::st_as_text(sf::st_geometry(x), EWKT = TRUE)
    x <- sf::st_drop_geometry(x)
    if (length(geom_cols) == 0) stop("Table '", table_name, "' has no geometry column.")
    x[[geom_cols[1]]] <- ewkt
  }
  x <- as.data.frame(x)
  names(x) <- r2pg_names(names(x))
  if (!keep_id) x$id <- NULL
  cols <- intersect(types$column_name, names(x))
  if (length(cols) == 0) stop("No column of x matches table '", table_name, "'.")
  x <- x[, cols, drop = FALSE]
  for (it in cols) x[[it]] <- to_pg_text(x[[it]])

  tmp <- paste0("stdb_tmp_", table_name, "_", paste(sample(letters, 8), collapse = ""))
  DBI::dbWriteTable(con, tmp, x, temporary = TRUE, overwrite = TRUE, row.names = FALSE)
  on.exit(try(DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s",
                                          DBI::dbQuoteIdentifier(con, tmp))),
              silent = TRUE), add = TRUE)

  sel <- sapply(cols, function(it) {
    qi <- DBI::dbQuoteIdentifier(con, it)
    udt <- types$udt_name[types$column_name == it]
    if (udt == "geometry") {
      sprintf("ST_GeomFromEWKT(%s)", qi)
    } else {
      dtype <- types$data_type[types$column_name == it]
      if (dtype == "USER-DEFINED") dtype <- udt
      sprintf("CAST(%s AS %s)", qi, dtype)
    }
  })
  qcols <- paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", ")
  returning <- if ("id" %in% types$column_name) " RETURNING id" else ""
  sql <- sprintf("INSERT INTO %s (%s) SELECT %s FROM %s%s",
                 DBI::dbQuoteIdentifier(con, table_name), qcols,
                 paste(sel, collapse = ", "),
                 DBI::dbQuoteIdentifier(con, tmp), returning)
  if (nchar(returning) > 0) {
    ids <- DBI::dbGetQuery(con, sql)$id
  } else {
    DBI::dbExecute(con, sql)
    ids <- integer(0)
  }
  ids
}

## put_in_table() con database come destinazione
db_put_in_table <- function(x, con, table_name = "provider", sf = FALSE) {
  check_db_packages()
  types <- db_column_types(con, table_name)
  if (nrow(types) == 0) stop("Table '", table_name, "' not found in the database.")
  fields <- types$column_name
  ntables <- db_tables(con)

  if (!inherits(x, "sf")) x <- as.data.frame(x)

  ## chiavi esterne mancanti: si ricavano dalle colonne "<tabella>_<campo>"
  ## (es. provider_name -> provider_id), come nella versione R.
  fk <- fields[grepl("_id$", fields)]
  fk <- fk[sub("_id$", "", fk) %in% ntables]
  fk_missing <- fk[!(fk %in% names(x))]
  for (fkc in fk_missing) {
    namez <- sub("_id$", "", fkc)
    prefix <- paste0(namez, "_")
    xcols <- names(x)[startsWith(names(x), prefix)]
    if (length(xcols) == 0) next
    ot <- db_get_table(con, table_name = namez, data.table = FALSE, sf = FALSE)
    if (inherits(ot, "sf")) ot <- sf::st_drop_geometry(ot)
    ot <- as.data.frame(ot)
    ot$geometry <- NULL
    names(ot)[names(ot) == "ID"] <- "id"
    names(ot) <- paste(namez, names(ot), sep = "_")
    ot <- ot[, names(ot) %in% c(xcols, fkc), drop = FALSE]
    by <- intersect(xcols, names(ot))
    x <- dplyr::left_join(x, ot, by = by)
  }

  ## controllo chiavi esterne
  fk_present <- intersect(fk, names(x))
  if (length(fk_present) > 0) {
    xx <- if (inherits(x, "sf")) sf::st_drop_geometry(x) else x
    if (any(is.na(as.data.frame(xx)[, fk_present]))) {
      stop("foreing and/or primary keys (id) cannot be NA!")
    }
  }

  ids <- db_insert(con, x, table_name = table_name, keep_id = FALSE)
  attr(con, "inserted_id") <- ids
  con
}

## schema (nomnoml) da database
db_get_schema <- function(con, tables = NULL) {
  check_db_packages("DBI")
  if (is.null(tables)) {
    tables <- c("operator", "project", "provider", "action",
                "p", "variable", "ts", "attribute")
    tables <- tables[tables %in% db_tables(con)]
  }
  out <- list()
  for (it in tables) {
    types <- db_column_types(con, it)
    types$column_name <- ifelse(types$column_name == "id", "ID", types$column_name)
    out[[it]] <- structure(data.frame(column = types$column_name,
                            type = ifelse(types$data_type == "USER-DEFINED",
                                          types$udt_name, types$data_type)),
                           class = c("stdb_db_columns", "data.frame"))
  }
  out
}

## ---------------------------------------------------------- import / export

#' Create the spatiotemporal schema in a PostgreSQL database
#'
#' Runs the SQL script \code{docker/initdb/20_schema.sql} shipped with the
#' package (the same used by the Docker image) on an empty database.
#'
#' @param con a \code{DBIConnection} (see \code{\link{stdb_connect}}).
#' @param timescaledb logical. If \code{TRUE} \code{ts} becomes a TimescaleDB
#'   hypertable. If \code{NA} (default) TimescaleDB is used only if the
#'   extension is available on the server; if \code{FALSE} \code{ts} is a
#'   plain PostgreSQL table (PostGIS is always required).
#' @param schema_file SQL file with the schema.
#'
#' @return \code{con}, invisibly.
#' @export
#' @seealso \code{\link{write_db_to_postgres}}
#'
stdb_create_schema <- function(con, timescaledb = NA,
                               schema_file = system.file("docker/initdb/20_schema.sql",
                                                         package = "spatiotemporal")) {
  check_db_packages("DBI")
  if (!file.exists(schema_file)) stop("Schema file not found: ", schema_file)
  if (is.na(timescaledb)) {
    timescaledb <- nrow(DBI::dbGetQuery(con,
      "SELECT 1 FROM pg_available_extensions WHERE name = 'timescaledb'")) > 0
  }
  sql <- readLines(schema_file, warn = FALSE)
  sql <- sub("--.*$", "", sql)
  sql <- paste(sql, collapse = "\n")
  stmts <- trimws(strsplit(sql, ";[[:space:]]*\n")[[1]])
  stmts <- stmts[nchar(stmts) > 0]
  if (!timescaledb) {
    stmts <- stmts[!grepl("timescaledb|create_hypertable", stmts, ignore.case = TRUE)]
  }
  for (s in stmts) DBI::dbExecute(con, s)
  invisible(con)
}

#' Write a spatiotemporal R database (e.g. \code{db}) into PostgreSQL
#'
#' Copies all the tables of an R object with the schema of \code{\link{db}}
#' into a PostgreSQL/PostGIS(/TimescaleDB) database, keeping the
#' identifiers (\code{ID}). Afterwards, the connection can be used in place
#' of the R object in all the functions of the package.
#'
#' For very large \code{ts} tables, the CSV loading of the Docker image
#' (\code{inst/docker}) is faster.
#'
#' @param db R object with the tables, like \code{\link{db}}.
#' @param con a \code{DBIConnection} (see \code{\link{stdb_connect}}).
#' @param create_schema logical. If \code{TRUE} the schema is created first
#'   with \code{\link{stdb_create_schema}} (the database must be empty).
#' @param timescaledb see \code{\link{stdb_create_schema}}.
#' @param tables names of the tables to write, in order of dependency.
#' @param chunk_size number of rows of \code{ts} written per transaction.
#' @param verbose logical.
#'
#' @return \code{con}, invisibly.
#' @export
#' @seealso \code{\link{read_db_from_postgres}}
#'
#' @examples
#' \dontrun{
#' data(db)
#' con <- stdb_connect(dbname = "spatiotemporal")
#' write_db_to_postgres(db, con)
#' get_ts(x = con, p_id = 49, variable_id = 7)
#' }
#'
write_db_to_postgres <- function(db, con, create_schema = TRUE, timescaledb = NA,
                                 tables = c("operator", "project", "provider", "action",
                                            "variable", "attribute", "p", "ts"),
                                 chunk_size = 1e6, verbose = TRUE) {
  check_db_packages("DBI")
  if (create_schema) stdb_create_schema(con, timescaledb = timescaledb)
  tables <- intersect(tables, names(db))
  for (it in tables) {
    t <- db[[it]]
    n <- nrow(t)
    if (verbose) message(sprintf("%-10s %10d rows", it, n))
    if (n == 0) next
    starts <- seq(1, n, by = chunk_size)
    for (s in starts) {
      idx <- s:min(n, s + chunk_size - 1)
      DBI::dbWithTransaction(con, db_insert(con, t[idx, ], it, keep_id = TRUE))
    }
    ## allinea la sequenza/identity della chiave primaria
    seq_name <- DBI::dbGetQuery(con, sprintf("SELECT pg_get_serial_sequence(%s, 'id') AS s",
                                             DBI::dbQuoteString(con, it)))$s
    if (it == "ts" && is.na(seq_name)) seq_name <- "ts_id_seq"
    if (!is.na(seq_name)) {
      DBI::dbGetQuery(con, sprintf(
        "SELECT setval(%s, COALESCE((SELECT max(id) FROM %s), 0) + 1, false)",
        DBI::dbQuoteString(con, seq_name), DBI::dbQuoteIdentifier(con, it)))
    }
  }
  DBI::dbExecute(con, "ANALYZE")
  invisible(con)
}

#' Read a spatiotemporal PostgreSQL database into an R object
#'
#' The inverse of \code{\link{write_db_to_postgres}}: returns a list of
#' tables with the same structure as \code{\link{db}}.
#'
#' @param con a \code{DBIConnection} (see \code{\link{stdb_connect}}).
#' @param tables names of the tables to read.
#'
#' @return a list of \code{data.table} objects (and a \code{sf} object for
#'   \code{p}).
#' @export
#'
read_db_from_postgres <- function(con, tables = c("operator", "project", "provider",
                                                  "action", "p", "variable", "ts",
                                                  "attribute")) {
  tables <- intersect(tables, db_tables(con))
  out <- lapply(tables, function(it) get_table(con, table_name = it))
  names(out) <- tables
  out
}
