# 
# App Shiny - geo8 Template : Dynamic Geographic Map with Leaflet and Time Series 
# Author: Emanuele Cordano
# Date: October 2023
# License: GPL-3
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
#

library(leaflet)
library(dygraphs)

library(data.table)
library(dplyr)

library(nomnoml)
library(lubridate)
library(stringr)
library(sf)
#library(xml2)
#library(GSODR)
library(spatiotemporal)











data(db)
dbdata_file <- "ext/db_icpac_v2.rds"
dbdata_file <- "/home/ecor/local/rpackages/jrc/RGENERATEgeo/data/db.rds"
db <- dbdata_file |> readRDS()



db <- db

####

plotted_ts <- function(p_id0,variable_id0,db=db) {
  
  ####  db$ts %>% dplyr::filter(p_id==p_id0,variable_id==variable_id0) %>% 
  db %>% get_ts(p_id=p_id0,variable_id=variable_id0) %>%  
    dplyr::select(timestamptz,value) %>% arrange(timestamptz)}


main_ts <- function(p_id0,variable_id0,db=db) {"%s at %s" %>% 
    sprintf(db$variable$name[variable_id0],
            db$p$name2[p_id0])}











####
var_ids <- db$variable$ID[db$variable$name %in% c("MIN","MAX","PRCP")]


visualized_vars <- db$variable[data.table(ID=var_ids),on="ID"]

tab_names <- visualized_vars$name



####
gauge_station_providers <- db$provider$ID
names(gauge_station_providers) <- db$provider$name
gauge_station_providers_default <- gauge_station_providers[3] ##[c(2,3)]

####



































