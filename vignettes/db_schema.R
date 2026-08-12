## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 10,
  fig.height = 6,
  out.width = "100%",
  fig.align = "center"
)

## ----echo=TRUE, results='asis'------------------------------------------------
library(spatiotemporal)

help(package="spatiotemporal")

help("get_table")

help("get_ts")

help("get_p")

help("get_variables")

help(db)


## ----echo=TRUE, results='asis', fig.show='hold', fig.width=10, fig.height=6, out.width='100%'----

library(spatiotemporal)
library(nomnoml)
library(stringr)
library(data.table)

data(db)


## DATA BASE SCHEMA DESIGN

schema <- lapply(db, FUN=function(t, nfks) {
 #### print(nfks)
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

  content <- paste(schema[[it]]$column, schema[[it]]$type, sep = ": ", collapse = "||")
  out2[[it]] <- sprintf("[<table>%s| %s]", it, content)
  inn <- which(!is.na(schema[[it]]$reference))
  if (length(inn) > 0) {
    refs <- sprintf("[%s] <- 1..n[%s]", schema[[it]]$reference[inn], it)
    out2[[it]] <- c(out2[[it]], refs)
  }
}

nomnoml(paste(unlist(out2), collapse="\n"), png=TRUE, width=1400, height=700)




## ----echo=TRUE, results='asis'------------------------------------------------


data(db)
##
db0 <- db
##
years <- 2015:2024

country <- "Morocco"

project_name <- country
action_description <- sprintf("%s_%s",country,paste(range(years),collapse="_"))
### new project
project_id <- nrow(db0$project)+1
db$project <- rbind(db$project, data.table::data.table(ID = project_id, name = project_name))

### new action

action_id <- nrow(db0$action)+1
operator_id <- 1
new_action <- data.table::data.table(ID=action_id,description=action_description,timestamptz=Sys.time(),operator_id=operator_id,project_id=project_id)

db$action <- rbind(db$action,new_action)


## ----echo=TRUE, results='asis'------------------------------------------------
library(GSODR)
weather_ts_data_rds <- sprintf("/home/ecor/local/rpackages/jrc/spatiotemporal/inst/ext_data/%s.rds",action_description)
if (file.exists(weather_ts_data_rds)) {
  weather_ts_data <- readRDS(weather_ts_data_rds)
} else {
  weather_ts_data <- get_GSOD(years = years, country = country)
}






## ----echo=TRUE, results='asis', fig.show='hold', fig.width=10, fig.height=6, out.width='100%'----

library(terra)
library(sf)


p <- weather_ts_data |>
  dplyr::select(STNID, NAME, CTRY, COUNTRY_NAME, ISO2C, ISO3C, STATE,
                LATITUDE, LONGITUDE, ELEVATION, BEGIN, END) |>
  dplyr::filter(!duplicated(STNID))

prefix <- "GSOD station: data through GSODR R package"

# p$description <- apply(p, 1, function(row) {
#   paste(c(prefix, paste(names(row), row, sep = " : ")), collapse = " ; ")
# })
p$description <- purrr::pmap_chr(p, function(...) {
  row <- list(...)
  paste(c(prefix, paste(names(row), row, sep = " : ")), collapse = " ; ")
})



p <- sf::st_as_sf(
  p,
  coords = c("LONGITUDE", "LATITUDE"),
  crs = st_crs(db$p),
  remove = FALSE
)
####
p$name2 <- sprintf("GSOD_%s (%s - %s)",p$STNID,p$NAME,p$ISO2C)
p$provider_id <- db$provider$ID[db$provider$name=="gsod"]
p$ID <- nrow(db0$p)+1:nrow(p)

db$p <- rbind(db0$p,p[,names(db0$p)])

plet(vect(db$p),cex=20)




## ----echo=TRUE, results='asis'------------------------------------------------
library(dplyr)

v <- weather_ts_data
v$timestamptz <- as.Date(v$YEARMODA) |> as.POSIXct(tz="GMT")
v$name2 <- sprintf("GSOD_%s (%s - %s)",v$STNID,v$NAME,v$ISO2C)
vp <- db$p |> as.data.table()  |> dplyr::mutate(p_id=ID) |> dplyr::select(p_id,name2)

v <- left_join(v,vp)
ids <- c("timestamptz","p_id")

vv <- v  |> dplyr::select(which(names(v) %in% c(ids,db$variable$name))) |> melt(id.vars=ids)
vv <- db$variable |> mutate(variable_id=ID,variable=name) |> select(variable_id,variable) |> right_join(vv) |> select(-variable)



va <- v  |> dplyr::select(which(names(v) %in% c(ids,db$attribute$name))) |> melt(id.vars=ids) |> mutate(attribute_name=variable,attrvalue=value) |> select(-variable,-value)

##
va <- db$attribute |> mutate(attribute_id=ID,attribute_name=name) |> select(attribute_id,attribute_name,variable_name) |> right_join(va) ##|> select(-variable)
##
va <- db$variable |> mutate(variable_id=ID,variable_name=name) |> select(variable_id,variable_name) |> right_join(va) |> select(-variable_name)
##
###
vv <- full_join(vv,va)
vv$action_id <- action_id
vv$ID <- nrow(db0$ts)+1:nrow(vv)
nts <- names(db$ts)
vv <- vv[,..nts]
###
db$ts <- rbind(db$ts,vv)

tail(db$ts,n=20)


