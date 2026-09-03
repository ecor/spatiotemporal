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


## ----echo=TRUE, results='asis'------------------------------------------------

library(readxl)
library(openxlsx)

loc_xlsx <- system.file("ems_template/upload-test-3D_loc.xlsx", package = "spatiotemporal")
ts_xlsx  <- system.file("ems_template/upload-test-3D_ts.xlsx",  package = "spatiotemporal")
###
vv <- get_ts(db,action_id=action_id)

####
loc_ems <- read_excel(loc_xlsx)
ts_ems  <- read_excel(ts_xlsx)


ts_ems_edit  <- ts_ems

####################
####################
####################


vvts <- get_ts(db,get_action_args=list(description=action_description))
###
vvts <- vvts |> dplyr::filter(year(vvts$timestamptz)==2023,month(vvts$timestamptz) %in% c(6,7,8))
###

vvp <- get_p(db,ID=unique(vvts$p_id))
names(vvp)[names(vvp)=="ID"] <- "p_id"
names(vvp)[names(vvp)=="name2"] <- "p_name2"
vvv <- get_variable(db,ID=unique(vvts$variable_id))
names(vvv)[names(vvv)=="ID"] <- "variable_id"
names(vvv)[names(vvv)=="name"] <- "variable_name"
names(vvv)[names(vvv)=="description"] <- "variable_description"
####
#### Measuremt Unit (Manual Settings)
vvv$variable_unit <- as.character(NA)
vvv$variable_unit[vvv$variable_name %in% c("TEMP","MIN","MAX","DEWP")] <- "deg C"
vvv$variable_unit[vvv$variable_name %in% c("PRCP","SNDP")] <- "mm/day"
vvv$variable_unit[vvv$variable_name %in% c("STP","SLP")] <- "hPa"
vvv$variable_unit[vvv$variable_name %in% c("VISIB")] <- "km"
vvv$variable_unit[vvv$variable_name %in% c("GUST","WDSP","MXSPD")] <- "m/s"
vvv$variable_unit[vvv$variable_name %in% c("I_RAIN_DRIZZLE","I_FOG","I_SNOW_ICE","I_HAIL","I_THUNDER","I_TORNADO_FUNNEL")] <- "dimensionless"

vvv$variable_unit[vvv$variable_name %in% c("ES","EA")] <- "KPa"
vvv$variable_unit[vvv$variable_name %in% c("RH")] <- "%"

vva <- get_attribute(db,ID=unique(vvts$attribute_id))
names(vva)[names(vva)=="name"] <- "attribute_name"
names(vva)[names(vva)=="description"] <- "attribute_description"

names(vva)[names(vva)=="ID"] <- "attribute_id"
prov <- get_providers(db,ID=unique(vvv$provider_id))
names(prov)[names(prov)=="ID"] <- "provider_id"
names(prov)[names(prov)=="name"] <- "provider_name"

if (nrow(prov)>1) stop("Only one prrovider is here expected!")
####
loc_ems_edit <- loc_ems[rep(1,nrow(vvp)),]
loc_ems_edit[,c("X","Y")] <- st_coordinates(vvp)[,c("X","Y")]
loc_ems_edit$Code <- vvp$p_name2
loc_ems_edit$Srid <- st_crs(vvp)$epsg
loc_ems_edit$Provider <- prov$provider_name
loc_ems_edit$Note <- "GSOD Weather Station"

##########
vvr <- vvts |> right_join(x=vvp)  |> left_join(y=vvv) |> left_join(y=vva)

ts_ems_edit <- ts_ems[rep(1,nrow(vvr)),] 
ts_ems_edit[,c("X","Y")] <- st_coordinates(vvr)[,c("X","Y")]
ts_ems_edit$Srid <- st_crs(vvr)$epsg
ts_ems_edit$Provider <- prov$provider_name
ts_ems_edit$Unit <- vvr$variable_unit
ts_ems_edit$Parameter <- vvr$variable_name
ts_ems_edit$Date <- vvr$timestamptz
ts_ems_edit$Zref <- NA
ts_ems_edit$Zdelta <- NA
ts_ems_edit$Measure <- vvr$value
ts_ems_edit$Media <- "Air"
ts_ems_edit$Note <- "variable: %s ;attribute %s (%s) attrvalue: %s " |> sprintf(vvr$variable_description,vvr$attribute_description,vvr$attribute_name,as.character(vvr$attrvalue))

##########
wpathx <- "/home/ecor/local/rpackages/jrc/spatiotemporal/inst/ems_template_new"
if (!dir.exists(wpathx)) dir.create(wpathx, recursive = TRUE)

loc_xlsx_new <- file.path(wpathx, basename(loc_xlsx))
ts_xlsx_new  <- file.path(wpathx, basename(ts_xlsx))

## loc: carica il template originale e sovrascrive solo i dati sul primo foglio
wb_loc <- loadWorkbook(loc_xlsx)
sheet_loc <- names(wb_loc)[1]
writeData(wb_loc, sheet = sheet_loc, x = loc_ems_edit, startRow = 1, colNames = TRUE)
saveWorkbook(wb_loc, loc_xlsx_new, overwrite = TRUE)

## ts: idem
wb_ts <- loadWorkbook(ts_xlsx)
sheet_ts <- names(wb_ts)[1]
writeData(wb_ts, sheet = sheet_ts, x = ts_ems_edit, startRow = 1, colNames = TRUE)
saveWorkbook(wb_ts, ts_xlsx_new, overwrite = TRUE)

