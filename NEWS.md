# spatiotemporal 0.0.4 (branch spatiotemporaldb)

* The database `x`/`db` can now be either an R object (like `data(db)`) or a
  DBI connection to a PostgreSQL/PostGIS/TimescaleDB database with the schema
  of `inst/docker/initdb/20_schema.sql`.
* `get_table()`, `get_p()`, `get_ts()`, `get_variable()`, `get_attribute()`,
  `get_action()`, `get_providers()`, `get_projects()`, `get_operators()`:
  filters (`vfilter` and `...`) are executed by the database through `dbplyr`;
  spatial selections in `get_p()` use PostGIS `ST_Intersects`.
* `put_in_table()` inserts rows into the database (foreign keys resolved from
  `<table>_<field>` columns, e.g. `provider_name`); new ids in
  `attr(, "inserted_id")`.
* `get_schema()` works on a database connection.
* New functions: `stdb_connect()`, `is_db_connection()`, `stdb_create_schema()`,
  `write_db_to_postgres()`, `read_db_from_postgres()`.
* New tests in `tests/testthat` (run only if `STDB_TEST_DBNAME` is set).
* `DBI`, `dbplyr`, `RPostgres`/`RPostgreSQL` added to Suggests.
