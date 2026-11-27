## Test del backend PostgreSQL/PostGIS(/TimescaleDB).
## Vengono eseguiti solo se e' raggiungibile un database di prova, indicato
## con le variabili d'ambiente STDB_TEST_DBNAME (obbligatoria), STDB_TEST_HOST,
## STDB_TEST_PORT, STDB_TEST_USER, STDB_TEST_PASSWORD.
## Lo schema viene creato in uno schema PostgreSQL temporaneo, poi eliminato.

stdb_test_connection <- function() {
  dbname <- Sys.getenv("STDB_TEST_DBNAME")
  if (!nzchar(dbname)) skip("STDB_TEST_DBNAME not set")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dbplyr")
  con <- tryCatch(stdb_connect(dbname = dbname,
                               host = Sys.getenv("STDB_TEST_HOST", "localhost"),
                               port = as.integer(Sys.getenv("STDB_TEST_PORT", "5432")),
                               user = Sys.getenv("STDB_TEST_USER", Sys.getenv("USER")),
                               password = Sys.getenv("STDB_TEST_PASSWORD", NA)),
                  error = function(e) NULL)
  if (is.null(con)) skip("test database not reachable")
  con
}

data(db, package = "spatiotemporal")
## sottoinsieme piccolo: 3 stazioni, tutte le variabili
sdb <- db
sdb$ts <- db$ts[db$ts$p_id %in% c(24, 49, 50), ]

con <- stdb_test_connection()
schema <- paste0("stdb_test_", paste(sample(letters, 8), collapse = ""))
DBI::dbExecute(con, "CREATE EXTENSION IF NOT EXISTS postgis")
DBI::dbExecute(con, sprintf("CREATE SCHEMA %s", schema))
DBI::dbExecute(con, sprintf("SET search_path TO %s, public", schema))
withr_defer <- function() {
  try(DBI::dbExecute(con, sprintf("DROP SCHEMA %s CASCADE", schema)), silent = TRUE)
  try(DBI::dbDisconnect(con), silent = TRUE)
}

test_that("write_db_to_postgres loads all tables", {
  expect_true(is_db_connection(con))
  expect_false(is_db_connection(db))
  write_db_to_postgres(sdb, con, timescaledb = NA, verbose = FALSE)
  for (it in names(sdb)) {
    n <- DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM %s", it))$n
    expect_equal(as.numeric(n), nrow(sdb[[it]]), label = it)
  }
})

test_that("get_table gives the same result on R object and database", {
  a <- get_table(x = sdb, table_name = "ts", p_id = 49, variable_id = 7)
  b <- get_table(x = con, table_name = "ts", p_id = 49, variable_id = 7)
  expect_s3_class(b, "data.table")
  expect_equal(nrow(a), nrow(b))
  expect_setequal(names(a), names(b))
  a <- a[order(a$timestamptz), ]
  b <- b[order(b$timestamptz), ]
  expect_equal(as.numeric(a$value), as.numeric(b$value))
  expect_equal(as.numeric(a$timestamptz), as.numeric(b$timestamptz))

  v1 <- get_variable(x = sdb, name = "PRCP")
  v2 <- get_variable(x = con, name = "PRCP")
  expect_equal(v1$ID, v2$ID)

  f1 <- get_table(x = sdb, table_name = "ts", vfilter = "p_id==49 & value > 20")
  f2 <- get_table(x = con, table_name = "ts", vfilter = "p_id==49 & value > 20")
  expect_equal(nrow(f1), nrow(f2))

  i2 <- get_table(x = con, table_name = "variable", ID = c(1, 2))
  expect_equal(sort(i2$ID), c(1, 2))
})

test_that("get_p works with spatial filters on database", {
  p1 <- get_p(x = sdb)
  p2 <- get_p(x = con)
  expect_s3_class(p2, "sf")
  expect_equal(nrow(p1), nrow(p2))
  expect_equal(sf::st_crs(p2)$epsg, 4326L)

  px <- p1[49, ] |> sf::st_buffer(dist = 250 * 1000)
  a <- suppressWarnings(get_p(x = sdb, p = px))
  b <- get_p(x = con, p = px)
  expect_setequal(a$ID, b$ID)

  bb <- sf::st_bbox(c(xmin = 29.45, ymin = -8.85, xmax = 32.15, ymax = -2.25),
                    crs = sf::st_crs(4326))
  a <- suppressWarnings(get_p(x = sdb, p = bb))
  b <- get_p(x = con, p = bb)
  expect_setequal(a$ID, b$ID)
})

test_that("get_ts works on database", {
  p <- get_p(x = sdb)
  px <- p[49, ] |> sf::st_buffer(dist = 1)
  a <- suppressWarnings(get_ts(x = sdb, get_p_args = list(p = px),
              get_variable_args = list(name = "PRCP"), regular_timestamptz = TRUE))
  b <- suppressWarnings(get_ts(x = con, get_p_args = list(p = px),
              get_variable_args = list(name = "PRCP"), regular_timestamptz = TRUE))
  expect_equal(nrow(a), nrow(b))
  expect_equal(sum(a$value, na.rm = TRUE), sum(b$value, na.rm = TRUE))
})

test_that("put_in_table inserts rows and resolves foreign keys", {
  con2 <- put_in_table(data.table::data.table(name = c("asdf", "qwerty")),
                       db = con, table_name = "provider")
  ids <- attr(con2, "inserted_id")
  expect_length(ids, 2)
  expect_true(all(ids > max(sdb$provider$ID)))

  con2 <- put_in_table(data.table::data.table(name = "rainfall", provider_name = "asdf"),
                       db = con, table_name = "variable")
  v <- get_variable(x = con, name = "rainfall")
  expect_equal(v$provider_id, ids[1])

  newp <- sf::st_as_sf(data.frame(name2 = "test station", provider_name = "asdf",
                                  lon = 11.0, lat = 46.0),
                       coords = c("lon", "lat"), crs = 4326)
  con2 <- put_in_table(newp, db = con, table_name = "p")
  pid <- attr(con2, "inserted_id")
  p <- get_p(x = con, ID = pid)
  expect_equal(p$name2, "test station")
  expect_equal(as.numeric(sf::st_coordinates(p)), c(11, 46))

  newts <- data.table::data.table(variable_id = v$ID, p_id = pid,
                                  timestamptz = as.POSIXct("2026-11-27", tz = "UTC") + (0:2) * 86400,
                                  value = c(1.5, 0, 3.2), action_id = 1)
  con2 <- put_in_table(newts, db = con, table_name = "ts")
  tsx <- get_ts(x = con, p_id = pid, variable_id = v$ID)
  expect_equal(nrow(tsx), 3)
  expect_equal(sort(tsx$value), c(0, 1.5, 3.2))

  expect_error(put_in_table(data.table::data.table(name = "x", provider_name = "missing"),
                            db = con, table_name = "variable"))
})

test_that("get_schema and read_db_from_postgres work on database", {
  s <- get_schema(con)
  expect_true(grepl("\\[<table>ts\\|", s))
  expect_true(grepl("\\[p\\] <- 1..n\\[ts\\]", s))
  rdb <- read_db_from_postgres(con, tables = c("provider", "p"))
  expect_s3_class(rdb$p, "sf")
  expect_true("ID" %in% names(rdb$provider))
})

withr_defer()
