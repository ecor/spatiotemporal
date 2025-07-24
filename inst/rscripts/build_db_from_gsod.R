# 
# Build DB from GSOD  Dynamic Geographic Map with Leaflet and Time Series 
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
library(xml2)
library(GSODR)
library(readr)
#### VARIABLES
###vxml <- "/home/ecor/local/rpackages/rendena100/rendena10x/inst/ext_data/gsod/description/gsod_variables.xml" %>% 


vxml <- "/home/ecor/local/rpackages/jrc/spatiotemporal/inst/shiny/gsodapp/ext/gsod_description/gsod_variables.xml" %>% 
    readLines(encoding="UTF-8") %>% paste(collapse="/n") %>% read_xml()




# Extract variables
variables <- xml_find_all(vxml, "//Variable")
# Function to extract details from each variable node
extract_variable_details <- function(variable_node) {
  name <- xml_attr(variable_node, "name")
  description <- xml_text(xml_find_first(variable_node, "Description"))
  attributes <- xml_find_all(variable_node, "Attributes/Attribute")
  attr_list <- lapply(attributes, function(attr) {
    attr_name <- xml_attr(attr, "name")
    attr_value <- xml_text(attr)
    return(c(attr_name, attr_value))
  })
  return(list(name = name, description = description, attributes = attr_list))
}

####

# Apply the function to all variable nodes
variable_details <- lapply(variables, extract_variable_details)
names(variable_details) <- sapply(variable_details,FUN=function(x){x$name})
# Print the extracted details
print(variable_details)
variables_data_table <- data.table(name=sapply(variable_details,FUN=function(x){x$name}),
                                   description=sapply(variable_details,FUN=function(x){x$description}))

variable_attribute_list <- lapply(variable_details,FUN=function(x){x$attributes})
l <- which(sapply(variable_attribute_list,FUN=length)>0)
variable_attribute_list <- variable_attribute_list[l]
###
###
attributes_data_table <- variable_attribute_list |> lapply(FUN=function(x){x[[1]]}) |> lapply(FUN=matrix,nrow=1,byrow=TRUE) |>
  lapply(as.data.table) |> rbindlist()

names(attributes_data_table) <- c("name","description")
attributes_data_table$variable_name <- names(variable_attribute_list)


#   Create a data frame with points of GSOD stations 
#  INFO: https://cran.r-project.org/web/packages/GSODR/vignettes/GSODR.html#appendix-1-gsodr-final-data-format-contents-and-units
## Lake Tanganiyka: https://en.wikipedia.org/wiki/Lake_Tanganyika
lat <- -6.1
lon <- 29.5
n <- nearest_stations(LAT = lat, LON = lon, distance = 1000)


#p <- "/home/ecor/local/data/climate/gsod/tanganyika/gsod_tanganyika_stdns_v02.shp" %>% st_read()
p <- st_as_sf(n,coords=c("LON","LAT"),crs=4326)

p <- p[!duplicated(p$STNID),]
###
names(p)[names(p)=="LATITUD"] <- "LATITUDE"
names(p)[names(p)=="LONGITU"] <- "LONGITUDE"
names(p)[names(p)=="ELEVATI"] <- "ELEVATION"

names(p)[names(p)=="LATITUD"] <- "LATITUDE"
names(p)[names(p)=="LONGITU"] <- "LONGITUDE"
names(p)[names(p)=="ELEV(M)"] <- "ELEVATION"
###
nnp <- names(p)
nnp <- nnp[nnp!="geometry"] 
p$description <- "GSOD station: data through GSODR R package"
nnpstring <- paste(nnp,"%s",sep=": ")
p$name2 <- sprintf("GSOD_%s (%s - %s)",p$STNID,p$NAME,p$CTRY)
for (np in nnp) {
  
  ptemp <- sprintf("%s , %s : %s ",p$description,np,as.character(p[[np]]))
  p$description <- ptemp
}


p$ID <- 1:nrow(p)
p$provider_id <- 2
p <- p[,c("ID","name2","description","provider_id")]

### Example of values and vatriables (GSOD)
##v <- "/home/ecor/local/data/climate/gsod/tanganyika/" |> list.files(pattern=".rds",full.name=TRUE) |> sort() |> lapply(readRDS) |> rbindlist()
overwrite=FALSE

v <- "/home/ecor/local/rpackages/jrc/spatiotemporal/inst/shiny/gsodapp/ext/" |> list.files(pattern=".rds",full.name=TRUE)
v <- "/home/ecor/local/rpackages/jrc/spatiotemporal/inst/shiny/gsodapp/ext/" |> list.files(pattern=".rds",full.name=TRUE) |> sort() |> lapply(readRDS) |> rbindlist()

ids <- p$name2 |> str_replace_all("_"," ") |> str_split(" ") |> sapply(FUN=function(x){x[2]})
years=1979:2025
gsod_files="/home/ecor/local/data/climate/gsod/tanganyika_v2/station_gsod_%s_%s.csv"
gsod <- list()
for (id in ids) {
  gsod[[id]] <- list()
  for (year in years) {
    print(year)
    print(id)
    nn <- sprintf("Y%s",as.character(year))
    filename <- gsod_files |> sprintf(id,nn)
    cond <- file.exists(filename)
    download_cond <- FALSE
    if (cond) {
      gsod[[id]][[nn]] <- filename |> read_csv() ###read.table(filename,header=TRUE,sep=",",quote=FALSE)
    } else if (download_cond) {  
      gsod[[id]][[nn]] <- get_GSOD(station=id,year=year) |> try(silent=TRUE)
      if (is(gsod[[id]][[nn]],"try-error")) {
        gsod[[id]][[nn]] <- NULL
      } else if (is.data.table(gsod[[id]][[nn]])) {
        write.table(gsod[[id]][[nn]],file=filename,quote=FALSE,sep=",",col.names=TRUE,row.names = FALSE)
      }
    }
  }
}


#id="638010-99999"
#nn="Y2024"
#gsod[[id]][[nn]]
#filename <- gsod_files |> sprintf(id,nn)
#read.table(filename,header=TRUE,sep=",",quote=FALSE)
stop("HERE")


##v <- "/home/ecor/local/rpackages/rendena100/rendena10x/inst/ext_data/gsod/gsod_tanganyika_2020.rds" %>% readRDS()
v$name2 <- sprintf("GSOD_%s (%s - %s)",v$STNID,v$NAME,v$CTRY)
nnv <- names(v)[!(names(v) %in% nnp)]
nnv <- nnv[nnv!="COUNTRY_NAME"]
nnv <- nnv[nnv!="STATE"]
nnv <- nnv[nnv!="LONGITUDE"]
nnv <- nnv[nnv!="LATITUDE"]

## 
nnv <- nnv[!(nnv %in% c("YEAR","MONTH","DAY","YDAY"))]  

## cleaning 

v <- v[, ..nnv] %>% left_join(as.data.table(p),by="name2") 

nnv2 <- c(nnv,"ID")
v <- v[,..nnv2]
names(v)[names(v)=="ID"] <- "p_id"



nv <- names(v)
id.vars <- c("YEARMODA","name2","p_id")
nnv <- variables_data_table$name


#nv <- nv[!(nv %in% id.vars)]
#nattr <- nv[str_detect(nv,"ATTRIBUTES")]
#nv <- nv[!(nv %in% nattr)]
nattr <- attributes_data_table$name
v2 <- melt(v,id.vars=id.vars) %>% select(-name2)


v3 <- v2 %>% filter(variable %in% nnv)
attr1 <- v2 %>% filter(variable %in% nattr)
attr1$attribute <- attr1$variable
attr1$variable <- str_replace_all(attr1$variable,"_ATTRIBUTES","")
names(attr1)[names(attr1)=="value"] <- "attrvalue"
v4  <- full_join(v3,attr1)




#### VARIABLE TABLE 

variable_table <- variables_data_table
variable_table$ID <- 1:nrow(variable_table)
variable_table$provider_id <- 2
### ATTRRIBUTE TABLE 

attribute_table <- attributes_data_table
attribute_table$ID <- 1:nrow(attribute_table)

v5 <- data.table(variable_id=variable_table$ID,variable=variable_table$name) %>% right_join(v4)

v6 <- data.table(attribute_id=attribute_table$ID,attribute=attribute_table$name) %>% right_join(v5)

v7 <- v6 %>% select(-variable,-attribute)  ## remove variable and attribte name 


#####
v7$action_id <- 1

v7$YEARMODA <- as.POSIXct(v7$YEARMODA,tz="GMT")
names(v7)[names(v7)=="YEARMODA"] <- "timestamptz"
v7$value <- as.numeric(v7$value)
#####


# stop("HERE")
# 
# 
# p$note <- " text"
# p$project_id <- 1  
# p$ID <- 1:nrow(p)

db <- list()

db$operator <- data.table(ID=1:2, name=c("ecor", "jellow"), firstname=c("Emanuele", "John"), lastname=c("Cordano", "Field"),email=c("emanuele.cordano@gmail,com",NA))
db$project <- data.table(ID=1:3, name=c("rendena101", "rendena102", "rendena103"))
db$provider <- data.table(ID=1:2, name=c("dwd", "gsod"))
db$action <- data.table(ID=as.integer(1:3), description=c("download", "delivery")[c(1, 2, 1)], timestamptz=Sys.time(), operator_id=as.integer(1),project_id=1)
db$p <- p

db$variable <- variable_table ## see above
db$ts <- v7 ## see above
db$attribute <- attribute_table ## see above

## CHECK PRIMARY IDS OF THE TALES 

for (it in names(db)) {
  
  if (!("ID" %in% names(db[[it]])))  db[[it]]$ID <- 1:nrow(db[[it]])
  
  nn <- names(db[[it]])
  nn <- nn[nn!="ID"]
  nn <- c("ID",nn)
  
  db[[it]] <- db[[it]] %>% select(all_of(nn))
  
  
  
  
}
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

####
db_file <- "/home/ecor/local/rpackages/jrc/spatiotemporal/data/db.rda"
save(db,file=db_file,compress="xz")

####

var_ids <- db$variable$ID[db$variable$name %in% c("MIN","MAX","PRCP")]


visualized_vars <- db$variable[data.table(ID=var_ids),on="ID"]

tab_names <- visualized_vars$name



### ADDDED ON 2025 03 12

library(readr)
library(terra)
stations_selection <- read_csv("additional/stations_selection.csv") |> 
  as.data.table()

####
ssts2 <-  "GSOD_%s" |> sprintf(stations_selection$STATION_ID) 


####
all_ssts2 <- sapply(str_split(db$p$name2," "),FUN=function(x){x[1]}) |> 
  str_replace("-","")
  


iss <- which(ssts2 %in% all_ssts2)
piselect <- which(all_ssts2 %in% ssts2)
##########################################################################################


north <- -1.25
south <- -9.85
west <- 28.45
east <- 34.15

ee_tgk <- rast(xmin=west,xmax=east,ymin=south,ymax=north,crs="EPSG:4326")

pcrop <- db$p |> st_crop(ee_tgk)
pcrop <- db$p |> st_crop(ee_tgk)



stop("HERE")

##########################################################################################
### PRECIPITATION
iprcp <- db$variable |> dplyr::filter(name=="PRCP") |> select(ID) |>
  as.integer()


attrs  <- db$ts |> dplyr::filter(variable_id==iprcp & !is.na(variable_id)) |> 
  select(attribute_id,attrvalue)


 
attrs_descr <- db$attribute |> dplyr::filter(ID %in% attrs$attribute_id) 



attrs_descr$description[1] |> str_split("/n") |> unlist()|> writeLines()



#####



















