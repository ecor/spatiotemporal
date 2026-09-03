## ---------------------------------------------------------------------------
## R script: download multiple years of SSOD v2 station data using link #1
## ("by-year" access point)
##
##   https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/v2/access/by-year/
##
## Target station: Ouagadougou airport (Thomas Sankara Intl, ICAO "DFFD"),
##                 Burkina Faso
## Target years:   2020 - 2026 (inclusive)
##
## The "by-year" access point is organized as one file per station per
## year, under:
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
## Each year file's existence was confirmed against the live bucket
## listing (S3-compatible ListObjectsV2 endpoint) before writing this
## script; note that 2026 is a partial year (data collection is ongoing).
## ---------------------------------------------------------------------------

pkgs <- c("httr")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(httr)

# --- parameters: change these to fetch a different station/year range ------
station_id <- "UVI0000DFFD"   # Ouagadougou airport (ICAO DFFD), Burkina Faso
years      <- 2020:2026

base_url <- "https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/v2/access/by-year/"
dir.create("ssod_data", showWarnings = FALSE)

#' Download one year's CSV file for the given station
#'
#' @param year   4-digit year
#' @return       a data.frame with the year's data, or NULL if the
#'               download failed (e.g. the station has no data for
#'               that year)
download_ssod_year <- function(year) {
  file_name <- sprintf("SSOD_%s_%d.csv", station_id, year)
  file_url  <- paste0(base_url, year, "/csv/", file_name)
  dest_file <- file.path("ssod_data", file_name)

  cat("Downloading:", file_url, "\n")

  resp <- tryCatch(GET(file_url), error = function(e) NULL)

  if (is.null(resp) || http_error(resp)) {
    warning(sprintf("Could not download data for year %d (station %s) - skipping",
                     year, station_id))
    return(NULL)
  }

  writeBin(content(resp, as = "raw"), dest_file)
  cat("  Saved to:", dest_file, "\n")

  read.csv(dest_file, stringsAsFactors = FALSE)
}

# --- download every year and collect the results ----------------------------
yearly_data <- lapply(years, download_ssod_year)
names(yearly_data) <- years

# drop years that failed to download
yearly_data <- Filter(Negate(is.null), yearly_data)

# --- combine all years into a single data.frame ------------------------------
all_data <- do.call(rbind, yearly_data)
rownames(all_data) <- NULL

cat("\nYears successfully downloaded:", paste(names(yearly_data), collapse = ", "), "\n")
cat("Total rows across all years:", nrow(all_data), "\n")
cat("Columns:", paste(names(all_data), collapse = ", "), "\n")
print(head(all_data))

# --- save the combined dataset to a single CSV -------------------------------
combined_file <- sprintf("ssod_data/SSOD_%s_%d-%d_combined.csv",
                          station_id, min(years), max(years))
write.csv(all_data, combined_file, row.names = FALSE)
cat("\nCombined dataset saved to:", combined_file, "\n")
