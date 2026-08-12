# --- Pacchetti ---
rm(list = ls())

library(sf)
library(sp)
library(gstat)
library(dplyr)
library(lubridate)
library(terra)
library(data.table)
library(spatiotemporal)
library(magrittr)

########### PRECIPITATION DATASET
db_rds <- "/home/ecor/local/data/climate/jrc/icpac/shiny/gsodicpacapp/ext/db_icpac_v0.rds"
db <- readRDS(db_rds)
providers <- get_providers(db)
## 
variable_name=c("PRCP")
variables <- get_variable(db,name=variable_name)

##
provider_name="icpac_rwanda"
provider_id=providers |> dplyr::filter(name %in% provider_name) |> select(ID) |> as.numeric()
stations_latlon <- get_p(db,provider_id=provider_id)

prcpts0 <- get_ts(db,p_id=stations_latlon$ID,variable_id=variables$ID)
stations0 <- stations_latlon |> as.data.table() |> select(ID,name2)

prcpts <-  prcpts0 |> select(timestamptz,p_id,value) |> left_join(stations0,by=join_by(p_id==ID))
prcptsw <- prcpts |> select(timestamptz,value,name2) |> dcast(timestamptz ~ name2)
rain_long <- prcpts |> transmute(station_id=name2,date=as.Date(timestamptz),rain_mm=value)  
# Rwanda lies mostly in UTM Zone 36 South. The most commonly used CRS for this area is:
#   
#   EPSG:32736 – WGS 84 / UTM zone 36S
# 
# Datum: WGS 84
# Units: meters
# Suitable for Rwanda and other regions in the same zone south of the equator.
# 
# hj
# 
# Alternatively, if you need a local projected CRS for Rwanda (for higher accuracy in national projects), you can use:
#   
#   EPSG:3346 – Rwanda 2005 / UTM zone 36S
# 
# Datum: Rwanda 2005 (a local geodetic datum)
# Units: meters
# Recommended for official national mapping.
##

crs_metric <- 32736
stations <- stations_latlon |> st_transform(crs=crs_metric)

# 
# dates <- seq.Date(as.Date("1985-01-01"), as.Date("2024-12-31"), by = "day")
# set.seed(42)
# rain_long <- expand.grid(station_id = stations$ID, date = dates) |>
#   as_tibble() |>
#   mutate(rain_mm = pmax(0, rnorm(n(), mean = 3, sd = 10)))
# stop("here")
# --- Geometrie ---
#st_sf <- st_as_sf(stations, coords = c("lon","lat"), crs = 4326)
st_utm <- stations |> transmute(station_id=name2)##st_transform(st_sf, 32632)

# --- Griglia di interpolazione ---
bb <- st_bbox(st_utm)
res <- 1000000
xseq <- seq(bb["xmin"], bb["xmax"], by = res)
yseq <- seq(bb["ymin"], bb["ymax"], by = res)
grid_sf <- st_as_sf(expand.grid(x = xseq, y = yseq), coords = c("x","y"), crs = crs_metric)

# --- Funzione di kriging con gstat ---
krige_one_day <- function(day, st_utm, rain_long_day, grid_sf) {
  day_df <<- rain_long_day |> filter(date == day)
  st_day <<- st_utm |> left_join(day_df, by = "station_id")
  
  if (sum(!is.na(st_day$rain_mm)) < 3) return(NULL)
  
  # Converti in Spatial
  st_sp <<- as_Spatial(st_day)
  grid_sp <<- as_Spatial(grid_sf)
  
  # Definisci modello variogramma
  vgm_model <<- variogram(rain_mm ~ 1, st_sp)
  fit_vgm <<- fit.variogram(vgm_model, model = vgm("Sph"))
  print("ktige")
  # Kriging
  kr <<- krige(rain_mm ~ 1, st_sp, grid_sp, model = fit_vgm)
  
  # Converti in sf e aggiungi data
  kr_sf <- st_as_sf(kr)
  kr_df <<- kr_sf |> as.data.table() |> select(-geometry)
  kr_coords <<- st_coordinates(kr_sf) |> as.data.table()
  kr_rst <<- cbind(kr_coords,kr_df) |> as.data.frame() |> rast(crs=crs(kr_sf))

  ####
  kr_sf$date <- day
  return(kr_sf)
}

# --- Loop su alcune date ---
all_dates <- sort(unique(rain_long$date))[1:30]
kr_list <- lapply(all_dates[1:5], krige_one_day, st_utm = st_utm,
                  rain_long_day = rain_long, grid_sf = grid_sf)
kr_list <- kr_list[!sapply(kr_list, is.null)]
kr_all <- do.call(rbind, kr_list)

# --- Conversione in raster terra ---
# Crea raster stack con una banda per ogni giorno
rast_list <- lapply(split(kr_all, kr_all$date), function(df) {
  r <- rast(df, type = "xyz", crs = "EPSG:32632")
  names(r) <- paste0("rain_", unique(df$date))
  r
})

rain_stack <- rast(rast_list)

#### # Salva su disco
####writeRaster(rain_stack, "kriging_rainfall.tif", overwrite = TRUE)


# --- Salvataggio ---
#st_write(kr_all, "kriging_daily_automap.gpkg", delete_dsn = TRUE)
