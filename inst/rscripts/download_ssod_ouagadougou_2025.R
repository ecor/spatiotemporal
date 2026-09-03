## ---------------------------------------------------------------------------
## R script: download SSOD v2 station data using link #1 ("by-year" access
## point)
##
##   https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/v2/access/by-year/
##
## Target station: Ouagadougou airport (Thomas Sankara Intl, ICAO "DFFD"),
##                 Burkina Faso
## Target year:    2025
##
## The "by-year" access point is actually organized as one file per
## station per year, under:
##   v2/access/by-year/<year>/csv/SSOD_<station_id>_<year>.csv
##
## Station IDs are an 11-character code: 2-letter FIPS country code +
## 1-letter network flag + 8-character local identifier. For "network I"
## (ICAO-identified) stations, the local identifier is "0000" followed by
## the 4-letter ICAO airport code.
##
##   Burkina Faso FIPS country code : "UV"
##   Ouagadougou airport ICAO code  : "DFFD"
##   => station id                 : "UVI0000DFFD"
##
## This was confirmed against the live bucket listing (S3-compatible
## ListObjectsV2 endpoint), which contains exactly:
##   v2/access/by-year/2025/csv/SSOD_UVI0000DFFD_2025.csv
## ---------------------------------------------------------------------------

pkgs <- c("httr")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(httr)

# --- parameters: change these to fetch a different station/year ------------
station_id <- "UVI0000DFFD"   # Ouagadougou airport (ICAO DFFD), Burkina Faso
year       <- 2022:2026

# --- build the download URL -------------------------------------------------
base_url  <- "https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/v2/access/by-year/"
file_name <- sprintf("SSOD_%s_%d.csv", station_id, year)
file_url  <- paste0(base_url, year, "/csv/", file_name)

cat("Downloading:", file_url, "\n")

# --- download the file, failing loudly on HTTP errors -----------------------
resp <- GET(file_url)
stop_for_status(resp, task = paste("download", file_name))

dir.create("ssod_data", showWarnings = FALSE)
dest_file <- file.path("ssod_data", file_name)
writeBin(content(resp, as = "raw"), dest_file)

cat("Saved to:", dest_file, "\n")

# --- load and preview the downloaded data -----------------------------------
station_data <- read.csv(dest_file, stringsAsFactors = FALSE)

cat("\nRows downloaded:", nrow(station_data), "\n")
cat("Columns:", paste(names(station_data), collapse = ", "), "\n")
print(head(station_data))
