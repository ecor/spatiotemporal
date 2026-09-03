# =================================================================
# Download SSODv2 (Synoptic Summary of the Day) station data for
# Morocco, years 2024-2026
#
# IMPORTANT: SSOD does not have a query-style REST API. NOAA NCEI's
# own README describes "bulk download" as plain HTTPS access to
# static CSV files organized by year or by station, e.g.:
#
#   by-year:    https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/v2/access/by-year/<YYYY>/ssod_<STATION>_<YYYY>.csv
#   by-station: https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/v2/access/by-station/SSOD_<STATION>.csv
#
# Source docs:
#   https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/v2/doc/ssodv2_DOCUMENTATION.pdf
#   https://www.ncei.noaa.gov/products/global-historical-climatology-network-hourly
#
# This script:
#   1. Downloads NOAA's GHCNh station list (SSOD is derived from the
#      GHCNh network, so SSOD stations are a subset of it) and keeps
#      only stations whose GHCN identifier starts with "MO" - the
#      FIPS 10-4 country code for Morocco (confirmed against NOAA's
#      ghcnd-countries.txt reference list).
#   2. For each Moroccan station x each requested year, tries to
#      download the "by-year" CSV. Missing station/year combinations
#      (station didn't report that year, etc.) are skipped, not
#      treated as fatal errors.
#   3. Combines everything actually found into one CSV.
#
# NOTE ON CASING: the README's URL *pattern* capitalizes "SSOD_...",
# but the README's own worked example uses lowercase "ssod_...".
# NOAA's object store is case-sensitive, so this script tries both
# and keeps whichever one works.
# =================================================================

pkgs <- c("dplyr", "readr", "purrr", "stringr", "tidyr")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

options(HTTPUserAgent = "R (SSOD Morocco download script)")

# ---- Config -------------------------------------------------------

years        <- 2024:2026
country_code <- "MO"                 # FIPS 10-4 country code for Morocco
out_dir      <- "/home/ecor/local/rpackages/jrc/spatiotemporal/inst/ext_data/ssod/ssod_morocco"
raw_dir      <- file.path(out_dir, "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

station_list_url <- "https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/doc/ghcnh-station-list.txt"
by_year_base_url <- "https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/v2/access/by-year"

# ---- 1. Station list: identify Moroccan stations -------------------

station_list_file <- file.path(out_dir, "ghcnh-station-list.txt")
if (!file.exists(station_list_file)) {
  download.file(station_list_url, station_list_file, mode = "wb", quiet = TRUE)
}

# The published file is comma-delimited; fall back to the classic
# fixed-width GHCN station-list layout if that assumption is wrong.
stations <- tryCatch({
  df <- read_csv(station_list_file, col_names = FALSE, show_col_types = FALSE)
  if (ncol(df) < 6) stop("file does not look comma-delimited")
  names(df)[1:6] <- c("GHCN_ID", "LATITUDE", "LONGITUDE", "ELEVATION", "STATE", "NAME")
  df
}, error = function(e) {
  message("Comma-delimited parse failed (", conditionMessage(e),
          "); falling back to fixed-width parser...")
  read_fwf(
    station_list_file,
    fwf_widths(
      c(11, 9, 10, 7, 3, 31, 4, 4, 6, 6),
      col_names = c("GHCN_ID", "LATITUDE", "LONGITUDE", "ELEVATION",
                    "STATE", "NAME", "GSN", "HCN_CRN", "WMO_ID", "ICAO")
    ),
    show_col_types = FALSE
  )
})

morocco_stations <- stations %>%
  filter(str_starts(GHCN_ID, country_code)) %>%
  distinct(GHCN_ID, .keep_all = TRUE)

if (nrow(morocco_stations) == 0) {
  stop("No Moroccan stations found - check that ", station_list_url,
       " downloaded correctly and that its column layout matches what ",
       "this script assumes.")
}

cat(sprintf("Found %d candidate Moroccan station(s):\n", nrow(morocco_stations)))
print(morocco_stations %>% select(GHCN_ID, NAME, LATITUDE, LONGITUDE))
write_csv(morocco_stations, file.path(out_dir, "morocco_stations.csv"))

# ---- 2. Download the by-year SSOD CSV for each station x year -----

try_download <- function(station, year) {
  for (prefix in c("ssod", "SSOD")) {
    fname <- sprintf("%s_%s_%d.csv", prefix, station, year)
    url   <- sprintf("%s/%d/%s", by_year_base_url, year, fname)
    dest  <- file.path(raw_dir, fname)

    ok <- tryCatch({
      suppressWarnings(download.file(url, dest, mode = "wb", quiet = TRUE))
      file.exists(dest) && file.size(dest) > 0
    }, error = function(e) FALSE)

    if (isTRUE(ok)) return(dest)
    if (file.exists(dest)) unlink(dest)
  }
  NA_character_
}

combos <- expand_grid(station = morocco_stations$GHCN_ID, year = years)

cat(sprintf("\nAttempting %d station-year downloads...\n", nrow(combos)))
combos$file <- map2_chr(combos$station, combos$year, function(s, y) {
  f <- try_download(s, y)
  Sys.sleep(0.2)  # be polite to NOAA's server
  f
})

found <- combos %>% filter(!is.na(file))
cat(sprintf("Downloaded %d of %d station-year files.\n", nrow(found), nrow(combos)))

# ---- 3. Combine everything found into one CSV ----------------------

if (nrow(found) > 0) {
  all_data <- found$file %>%
    map(~ read_csv(.x, show_col_types = FALSE, col_types = cols(.default = "c"))) %>%
    bind_rows() %>%
    type_convert()

  out_file <- file.path(out_dir, "ssod_morocco_2024_2026.csv")
  write_csv(all_data, out_file)
  cat(sprintf("\nCombined data written to %s (%d rows, %d stations).\n",
              out_file, nrow(all_data), n_distinct(all_data$STATION)))
} else {
  message(
    "\nNo SSOD files were successfully downloaded for Morocco in ",
    paste(years, collapse = ", "), ".\n",
    "This can mean either (a) no Moroccan station reported in this ",
    "window, or (b) the URL pattern/casing has changed. Sanity-check ",
    "by browsing:\n",
    "  https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/v2/access/by-station/\n",
    "for a filename like SSOD_<one of the station IDs above>.csv, and ",
    "adjust try_download() accordingly."
  )
}
