## ---------------------------------------------------------------------------
## Script R: estrazione dell'elenco file dal terzo link (archivio SSOD v2)
##
##   https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/v2/archive/
##
## Quella pagina non e' una semplice directory listing HTML: e' una SPA
## ("NCEI Object Store Explorer") che mostra il contenuto di uno storage
## S3-compatibile. La lista dei file si ottiene interrogando direttamente
## l'endpoint REST S3 sottostante (protocollo "ListObjectsV2"), aggiungendo
## alla URL base i parametri list-type=2, prefix e delimiter.
##
## Verificato: la richiesta
##   https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/
##       ?list-type=2&prefix=v2/archive/&delimiter=/
## restituisce un XML "ListBucketResult" con ~214 file, es.:
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

#' Elenca (con paginazione) il contenuto di un "prefix" nel bucket SSOD v2
#'
#' @param prefix     prefisso da esplorare (es. "v2/archive/")
#' @param base_url   URL base del bucket (senza query string)
#' @param delimiter  "/" per non scendere nelle sottocartelle, "" per
#'                   ottenere ricorsivamente tutti i file
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
    xml_ns_strip(doc)   # rimuove il namespace S3, semplifica le query XPath

    # --- file (Contents) ---
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

    # --- eventuali sottocartelle (CommonPrefixes) ---
    cp <- xml_find_all(doc, ".//CommonPrefixes/Prefix")
    if (length(cp) > 0) folder_list <- c(folder_list, xml_text(cp))

    # --- paginazione (default max 1000 chiavi per richiesta) ---
    is_truncated <- xml_text(xml_find_first(doc, ".//IsTruncated"))
    if (identical(is_truncated, "true")) {
      token <- xml_text(xml_find_first(doc, ".//NextContinuationToken"))
    } else {
      break
    }
  }

  # elimina l'eventuale riga "cartella vuota" uguale al prefix stesso
  file_list <- file_list[file_list$key != prefix, , drop = FALSE]

  list(files = file_list, folders = folder_list)
}

## ---------------------------------------------------------------------------
## Esecuzione: estrae il contenuto di v2/archive/
## ---------------------------------------------------------------------------

base_url <- "https://www.ncei.noaa.gov/oa/synoptic-summary-of-the-day/"
risultato <- list_bucket(prefix = "v2/archive/", base_url = base_url)

file_df <- risultato$files
file_df$size_MB <- round(file_df$size / 1024^2, 2)
file_df$url <- paste0(base_url, file_df$key)

cat("File trovati in v2/archive/:", nrow(file_df), "\n")
print(head(file_df[, c("key", "size_MB", "last_modified")], 20))

# Salva l'elenco completo in un CSV
write.csv(file_df, "ssod_v2_archive_listing.csv", row.names = FALSE)
cat("\nElenco completo salvato in: ssod_v2_archive_listing.csv\n")

## ---------------------------------------------------------------------------
## Download automatico dell'ultimo file (record) dell'archivio
## ---------------------------------------------------------------------------
## "Ultimo" = il record dati (.tar.gz) piu' recente, individuato dal campo
## LastModified restituito da S3 (non dal semplice ordine alfabetico delle
## chiavi, che pero' viene usato come fallback se il parsing della data
## dovesse fallire).

record_files <- file_df[grepl("\\.tar\\.gz$", file_df$key), , drop = FALSE]

if (nrow(record_files) == 0) {
  warning("Nessun file record (.tar.gz) trovato in v2/archive/")
} else {
  record_files$last_modified_dt <- as.POSIXct(
    record_files$last_modified,
    format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"
  )

  if (all(is.na(record_files$last_modified_dt))) {
    # fallback: ordine alfabetico della chiave (i nomi file sono del tipo
    # ssod_v2.0.0_d<anno>_c<data_creazione>.tar.gz, quindi l'ordine
    # alfabetico coincide con quello cronologico)
    record_files <- record_files[order(record_files$key), ]
  } else {
    record_files <- record_files[order(record_files$last_modified_dt), ]
  }

  ultimo <- record_files[nrow(record_files), ]

  cat("\nUltimo file (record) disponibile:\n")
  cat(" - chiave:         ", ultimo$key, "\n")
  cat(" - ultima modifica:", ultimo$last_modified, "\n")
  cat(" - dimensione:     ", ultimo$size_MB, "MB\n")

  dir.create("ssod_archive", showWarnings = FALSE)
  dest_file <- file.path("ssod_archive", basename(ultimo$key))

  download.file(ultimo$url, destfile = dest_file, mode = "wb")
  cat("\nScaricato in:", dest_file, "\n")
}

## ---------------------------------------------------------------------------
## (Opzionale) scaricare altri file dall'archivio
## ---------------------------------------------------------------------------
# es.: scarica il README
# readme_url <- file_df$url[grepl("README", file_df$key)]
# download.file(readme_url, destfile = "README.txt", mode = "wb")

# es.: scarica tutti i .tar.gz in "ssod_archive/"
# for (i in seq_len(nrow(record_files))) {
#   dest <- file.path("ssod_archive", basename(record_files$key[i]))
#   download.file(record_files$url[i], destfile = dest, mode = "wb")
# }
