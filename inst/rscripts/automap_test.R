# --- Pacchetti ---
rm(list=ls())

library(sf)
library(sp)
library(automap)
library(dplyr)
library(lubridate)
library(terra)

# --- Dati di esempio (sostituisci con i tuoi) ---
stations <- tibble::tibble(
  station_id = c("A","B","C","D"),
  lon = c(8.60, 8.75, 8.80, 8.55),
  lat = c(45.80, 45.78, 45.85, 45.70)
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



# --- Funzione di kriging con automap ---
krige_one_day <- function(day, st_utm, rain_long_day, grid_sf) {
  day_df <- rain_long_day |> dplyr::filter(date == day)
  st_day <- st_utm |> left_join(day_df, by = "station_id")
  if (sum(!is.na(st_day$rain_mm)) < 3) return(NULL)
  print(st_day)
  st_sp <- as(st_day, "Spatial")
  grid_sp <- as(grid_sf, "Spatial")
  
  # automap: costruisce variogramma e kriging automaticamente
  print(st_sp)
  print(grid_sp)
  kr <- automap::autoKrige(rain_mm ~ 1, input_data = st_sp, new_data = grid_sp)
  kr0 <<- krh
  kr_sf <- st_as_sf(kr$krige_output)
  #kr_sf$date <- day
  kr_sf <<- kr_sf
  kr_sf
}



all_dates <- sort(unique(rain_long$date))[1:30]
# out <- krige_one_day(all_dates[1], st_utm = st_utm,
#                            rain_long_day = rain_long, grid_sf = grid_sf)
# 
# stop("qui")
# --- Loop su tutte le date (qui solo 30 per test) ---

kr_list <- lapply(all_dates[6:7], krige_one_day, st_utm = st_utm,
                  rain_long_day = rain_long, grid_sf = grid_sf)
kr_list <- kr_list[!sapply(kr_list, is.null)]
kr_all <- do.call(rbind, kr_list)



# --- Salvataggio ---
#st_write(kr_all, "kriging_daily_automap.gpkg", delete_dsn = TRUE)
