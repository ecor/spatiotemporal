
# spatiotemporal

An R package for a spatiotemporal database schema. It has been created to handle weather data from weather stations. An example from GSODR.

See  manual for further details.

## Installation from R console 

```

## Installiation from Github repo through Gituh token 

remotes::install_github("ecor/spatiotemporal")
```

## Usage from R console 

```
library(spatiotemporal)

help(package="spatiotemporal")

help("get_table")

help("get_ts")

help("get_p")

help("get_variables")

help(db)

```

## Database backend: R object or PostgreSQL/PostGIS/TimescaleDB

All the functions of the package (`get_table()`, `get_p()`, `get_ts()`,
`get_variable()`, `put_in_table()`, `get_schema()`, ...) accept as `x` (or `db`)
either an R object like `data(db)` or a DBI connection to a
PostgreSQL/PostGIS/TimescaleDB database with the same schema
(see `inst/docker`). With a database, filters are translated into SQL
(via `dbplyr`) and spatial selections into PostGIS `ST_Intersects`, so only
the requested rows are transferred to R.

Extra packages needed: `DBI`, `dbplyr` and `RPostgres` (or `RPostgreSQL`).

```r
library(spatiotemporal)

## e.g. through an SSH tunnel: ssh -N -L 5432:127.0.0.1:5432 ecor@rendena103.cloud
con <- stdb_connect(dbname = "spatiotemporal", user = "ecor")   # password in ~/.pgpass or PGPASSWORD

p    <- get_p(x = con)                                   # sf, EPSG:4326
px   <- p[49, ] |> st_buffer(dist = 250 * 1000)
prcp <- get_ts(x = con, get_p_args = list(p = px),
               get_variable_args = list(name = "PRCP"))

## insert new rows (ids are assigned by the database)
con <- put_in_table(data.table(name = "asdf"), db = con, table_name = "provider")
attr(con, "inserted_id")

## copy an R db into an empty PostgreSQL database, and back
data(db)
write_db_to_postgres(db, con)        # creates the schema (TimescaleDB if available)
db2 <- read_db_from_postgres(con)

DBI::dbDisconnect(con)
```

## Database Schema 


It can retrievd by this code: 



```

library(spatiotemporal)
library(nomnoml)


data(db)


## DATA BASE SCHEMA DESIGN

schema <- lapply(db, FUN=function(t, nfks) {
  print(nfks)
  o <- sapply(as.list(t), FUN=function(x) {class(x)})
  o2 <- data.frame(column=names(o), type=as.character(o))
  o2$reference <- as.character(NA)
  iin <- which(o2$column %in% nfks)
  o2$reference[iin] <- o2$column[iin]
  o2$reference[iin] <- str_sub(o2$reference[iin], end=-4)
  return(o2)
}, nfks=sprintf("%s_id", names(db)))

out2 <- list()
for (it in names(schema)) {
  print(it)
  content <- paste(schema[[it]]$column, schema[[it]]$type, sep = ": ", collapse = "||")
  out2[[it]] <- sprintf("[<table>%s| %s]", it, content)
  inn <- which(!is.na(schema[[it]]$reference))
  if (length(inn) > 0) { 
    refs <- sprintf("[%s] <- 1..n[%s]", schema[[it]]$reference[inn], it)
    out2[[it]] <- c(out2[[it]], refs)
  }
}

nomnoml(paste(unlist(out2), collapse="\n"), svg=TRUE)



```




