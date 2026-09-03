## ---------------------------------------------------------------------------
## R script: extract the file listing from link #3 (SSOD v2 archive)
##
##   https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/v2/archive/
##
## That page is not a plain HTML directory listing: it's a JS SPA
## ("NCEI Object Store Explorer") that displays the content of an
## S3-compatible object store. The actual file list can be obtained by
## querying the underlying REST S3 endpoint directly (ListObjectsV2
## protocol), by adding list-type=2, prefix and delimiter query
## parameters to the base URL.
##
## Verified: the request
##   https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/
##       ?list-type=2&prefix=v2/archive/&delimiter=/
## returns a "ListBucketResult" XML with ~214 files, e.g.:
##   v2/archive/README.txt
##   v2/archive/ssod_v2.0.0_d1900_c20260323.tar.gz
##   v2/archive/ssod_v2.0.0_d1901_c20260323.tar.gz
##   ...
##   v2/archive/ssod_v2.0.0_d2026_c20260826.tar.gz
## ---------------------------------------------------------------------------

pkgs <- c("httr", "xml2")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(httr)
library(xml2)

#' List (with pagination) the content of a "prefix" in the SSOD v2 bucket
#'
#' @param prefix     prefix to explore (e.g. "v2/archive/")
#' @param base_url   bucket base URL (without query string)
#' @param delimiter  "/" to stay within this folder (don't descend into
#'                   subfolders), "" to recursively list all files
list_bucket <- function(prefix = "v2/archive/",
                         base_url = "https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/",
                         delimiter = "/") {

  file_list <- data.frame(
    key = character(), size = numeric(),
    last_modified = character(), stringsAsFactors = FALSE
  )
  folder_list <- character()
  token <- NULL

  repeat {
    query <- list(`list-type` = "2", prefix = prefix, delimiter = delimiter)
    if (!is.null(token)) query$`continuation-token` <- token

    resp <- GET(base_url, query = query)
    stop_for_status(resp)

    doc <- read_xml(content(resp, as = "text", encoding = "UTF-8"))
    xml_ns_strip(doc)   # strip the S3 namespace to simplify XPath queries

    # --- files (Contents) ---
    contents <- xml_find_all(doc, ".//Contents")
    if (length(contents) > 0) {
      keys    <- xml_text(xml_find_all(contents, "./Key"))
      sizes   <- as.numeric(xml_text(xml_find_all(contents, "./Size")))
      lastmod <- xml_text(xml_find_all(contents, "./LastModified"))
      file_list <- rbind(
        file_list,
        data.frame(key = keys, size = sizes, last_modified = lastmod,
                   stringsAsFactors = FALSE)
      )
    }

    # --- any subfolders (CommonPrefixes) ---
    cp <- xml_find_all(doc, ".//CommonPrefixes/Prefix")
    if (length(cp) > 0) folder_list <- c(folder_list, xml_text(cp))

    # --- pagination (default max 1000 keys per request) ---
    is_truncated <- xml_text(xml_find_first(doc, ".//IsTruncated"))
    if (identical(is_truncated, "true")) {
      token <- xml_text(xml_find_first(doc, ".//NextContinuationToken"))
    } else {
      break
    }
  }

  # drop the "empty folder" row equal to the prefix itself, if present
  file_list <- file_list[file_list$key != prefix, , drop = FALSE]

  list(files = file_list, folders = folder_list)
}

## ---------------------------------------------------------------------------
## Run: extract the content of v2/archive/
## ---------------------------------------------------------------------------

base_url <- "https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/"
result <- list_bucket(prefix = "v2/archive/", base_url = base_url)

file_df <- result$files
file_df$size_MB <- round(file_df$size / 1024^2, 2)
file_df$url <- paste0(base_url, file_df$key)

cat("Files found in v2/archive/:", nrow(file_df), "\n")
print(head(file_df[, c("key", "size_MB", "last_modified")], 20))

# Save the full listing to a CSV file
write.csv(file_df, "ssod_v2_archive_listing.csv", row.names = FALSE)
cat("\nFull listing saved to: ssod_v2_archive_listing.csv\n")

## ---------------------------------------------------------------------------
## Automatically download the latest file (record) from the archive
## ---------------------------------------------------------------------------
## "Latest" = the most recent data record (.tar.gz), identified using the
## LastModified field returned by S3 (falling back to alphabetical key
## order if the date parsing fails for some reason).

record_files <- file_df[grepl("\\.tar\\.gz$", file_df$key), , drop = FALSE]

if (nrow(record_files) == 0) {
  warning("No record files (.tar.gz) found in v2/archive/")
} else {
  record_files$last_modified_dt <- as.POSIXct(
    record_files$last_modified,
    format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"
  )

  if (all(is.na(record_files$last_modified_dt))) {
    # fallback: alphabetical order of the key (filenames look like
    # ssod_v2.0.0_d<year>_c<creation_date>.tar.gz, so alphabetical order
    # matches chronological order here)
    record_files <- record_files[order(record_files$key), ]
  } else {
    record_files <- record_files[order(record_files$last_modified_dt), ]
  }

  latest <- record_files[nrow(record_files), ]

  cat("\nLatest record file available:\n")
  cat(" - key:          ", latest$key, "\n")
  cat(" - last modified:", latest$last_modified, "\n")
  cat(" - size:         ", latest$size_MB, "MB\n")

  dir.create("ssod_archive", showWarnings = FALSE)
  dest_file <- file.path("ssod_archive", basename(latest$key))

  download.file(latest$url, destfile = dest_file, mode = "wb")
  cat("\nDownloaded to:", dest_file, "\n")
}

## ---------------------------------------------------------------------------
## (Optional) download other files from the archive
## ---------------------------------------------------------------------------
# e.g.: download the README
# readme_url <- file_df$url[grepl("README", file_df$key)]
# download.file(readme_url, destfile = "README.txt", mode = "wb")

# e.g.: download all .tar.gz files into "ssod_archive/"
# for (i in seq_len(nrow(record_files))) {
#   dest <- file.path("ssod_archive", basename(record_files$key[i]))
#   download.file(record_files$url[i], destfile = dest, mode = "wb")
# }
