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

##dbdata_file <- "/home/ecor/local/rpackages/jrc/RGENERATEgeo/data/db.rds"
##db <- dbdata_file |> readRDS()
data(db)



db <- db

####
var_ids <- db$variable$ID[db$variable$name %in% c("MIN","MAX","PRCP")]


visualized_vars <- db$variable[data.table(ID=var_ids),on="ID"]

tab_names <- visualized_vars$name







































