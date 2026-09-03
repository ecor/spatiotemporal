## ---------------------------------------------------------------------------
## R script: download SSOD v2 data for ALL available Morocco weather
## stations, years 2000-2026, using link #1 ("by-year" access point)
##
##   https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/v2/access/by-year/
##
## Morocco's FIPS country code in this dataset is "MO" (confirmed against
## Moroccan airport ICAO codes present in the bucket, e.g. "GMMN" =
## Mohammed V Intl, Casablanca).
##
## The list of Moroccan stations is NOT hardcoded: for every year, the
## script queries the underlying S3-compatible ListObjectsV2 endpoint
## with prefix "v2/access/by-year/<year>/csv/SSOD_MO" and downloads
## whatever station files come back. This automatically adapts to
## stations being added or dropped over time (verified: 25 Moroccan
## stations in 2000 vs. 31 in 2020 - the station list does change).
##
## NOTE: this script makes one listing request + several dozen file
## downloads PER YEAR, so downloading the full 2000-2026 range can take
## a while and will create several hundred small CSV files locally.
## ---------------------------------------------------------------------------

pkgs <- c("httr", "xml2")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(httr)
library(xml2)

# --- parameters: change these to target a different country/year range ----
country_prefix <- "MO"        # Morocco FIPS country code
years          <- 2000:2026
pause_seconds  <- 0.1         # small delay between downloads, be nice to the server

base_url <- "https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/"
dir.create("ssod_data", showWarnings = FALSE)

#' List every SSOD "by-year" file key for a given year and country prefix
#' (handles pagination, in case a country ever has more than 1000 stations)
list_country_files_for_year <- function(year, country_prefix) {
  prefix <- sprintf("v2/access/by-year/%d/csv/SSOD_%s", year, country_prefix)
  keys <- character()
  token <- NULL

  repeat {
    query <- list(`list-type` = "2", prefix = prefix)
    if (!is.null(token)) query$`continuation-token` <- token

    resp <- GET(base_url, query = query)
    stop_for_status(resp)

    doc <- read_xml(content(resp, as = "text", encoding = "UTF-8"))
    xml_ns_strip(doc)   # strip the S3 namespace to simplify XPath queries

    found <- xml_text(xml_find_all(doc, ".//Contents/Key"))
    keys <- c(keys, found)

    is_truncated <- xml_text(xml_find_first(doc, ".//IsTruncated"))
    if (identical(is_truncated, "true")) {
      token <- xml_text(xml_find_first(doc, ".//NextContinuationToken"))
    } else {
      break
    }
  }

  keys
}

#' Download one file (S3 key) into ssod_data/<year>/<filename>.csv and
#' read it into a data.frame; returns NULL (with a warning) on failure
download_one <- function(key, year) {
  file_url <- paste0(base_url, key)
  dest_dir <- file.path("ssod_data", year)
  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
  dest_file <- file.path(dest_dir, basename(key))

  resp <- tryCatch(GET(file_url), error = function(e) NULL)
  if (is.null(resp) || http_error(resp)) {
    warning(sprintf("Failed to download %s - skipping", key))
    return(NULL)
  }

  writeBin(content(resp, as = "raw"), dest_file)
  tryCatch(read.csv(dest_file, stringsAsFactors = FALSE),
           error = function(e) {
             warning(sprintf("Failed to parse %s - skipping", key))
             NULL
           })
}

#' rbind a list of data.frames whose columns may not perfectly match
#' across years (missing columns are filled with NA)
rbind_fill <- function(df_list) {
  df_list <- Filter(Negate(is.null), df_list)
  if (length(df_list) == 0) return(data.frame())
  all_cols <- unique(unlist(lapply(df_list, names)))
  df_list <- lapply(df_list, function(df) {
    missing_cols <- setdiff(all_cols, names(df))
    for (col in missing_cols) df[[col]] <- NA
    df[all_cols]
  })
  do.call(rbind, df_list)
}

# --- main loop: discover + download every Morocco station, every year -----
all_data        <- list()
station_summary <- data.frame(year = integer(), n_stations = integer())

for (year in years) {
  cat("\n==== Year", year, "====\n")
  keys <- list_country_files_for_year(year, country_prefix)
  cat("Found", length(keys), "Morocco station file(s)\n")

  station_summary <- rbind(station_summary,
                            data.frame(year = year, n_stations = length(keys)))

  for (key in keys) {
    cat("  Downloading:", basename(key), "\n")
    df <- download_one(key, year)
    if (!is.null(df)) {
      all_data[[length(all_data) + 1]] <- df
    }
    if (pause_seconds > 0) Sys.sleep(pause_seconds)
  }
}

# --- combine everything into one data.frame ---------------------------------
combined <- rbind_fill(all_data)

cat("\n---------------------------------------------\n")
cat("Years processed:", paste(range(years), collapse = "-"), "\n")
cat("Total rows downloaded across all stations/years:", nrow(combined), "\n")
cat("\nNumber of Morocco stations found per year:\n")
print(station_summary)
wpath <- "/home/ecor/local/data/climate/ssod"
# --- save outputs -------------------------------------------------------------
combined_file <- sprintf("%s/ssod_data/SSOD_%s_all_stations_%d-%d_combined.csv",wpath,
                          country_prefix, min(years), max(years))
write.csv(combined, combined_file, row.names = FALSE)
cat("\nCombined dataset saved to:", combined_file, "\n")

summary_file <- sprintf("ssod_data/SSOD_%s_station_count_by_year_%d-%d.csv",
                         country_prefix, min(years), max(years))
write.csv(station_summary, summary_file, row.names = FALSE)
cat("Per-year station count saved to:", summary_file, "\n")
