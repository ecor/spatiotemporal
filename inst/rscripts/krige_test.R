# --- Pacchetti ---
rm(list = ls())

library(sf)
library(sp)
library(gstat)
library(dplyr)
library(lubridate)
library(terra)
library(data.table)

lon0 = c(8.60, 8.75, 8.80, 8.55)
lat0 = c(45.80, 45.78, 45.85, 45.70)
n=18
# --- Dati di esempio ---
stations <- tibble::tibble(
  station_id = sprintf("P%02d",1:n),
  lon = runif(n,min=min(lon0),max=max(lon0)),
  lat = runif(n,min=min(lat0),max=max(lat0))
)

dates <- seq.Date(as.Date("1985-01-01"), as.Date("2024-12-31"), by = "day")
set.seed(42)
rain_long <- expand.grid(station_id = stations$station_id, date = dates) |>
  as_tibble() |>
  mutate(rain_mm = pmax(0, rnorm(n(), mean = 3, sd = 10)))

# --- Geometrie ---
st_sf <- st_as_sf(stations, coords = c("lon","lat"), crs = 4326)
st_utm <- st_transform(st_sf, 32632)

# --- Griglia di interpolazione ---
bb <- st_bbox(st_utm)
res <- 1000
xseq <- seq(bb["xmin"], bb["xmax"], by = res)
yseq <- seq(bb["ymin"], bb["ymax"], by = res)
grid_sf <- st_as_sf(expand.grid(x = xseq, y = yseq), coords = c("x","y"), crs = 32632)

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
