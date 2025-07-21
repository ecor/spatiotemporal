NULL
#' db Dataset
#'
#'
#' @description This dataset contains the following variables:
#' \describe{
#'   \item{\code{operator}}{ table for operators }
#'   \item{\code{project}}{ table for projects }
#'   \item{\code{provider}}{ table for (data)  providers }
#'   \item{\code{action}}{ table for action (e.g data download, data privider) }
#'   \item{\code{p}}{ table for geographic/geospatial locations (e.g. stations/gauges locations), a \code{\link[sf]{sf}} object  }
#'   \item{\code{variable}}{ table for variaples (e.g. weather variales: precipitation/rainfall,temparature,...)}
#'   \item{\code{ts}}{table with time serirs (long table format)}
#'   \item{\code{attribute}}{table for attributes}
#'   
#' }
#'
#' @details This dataset stores all information about meteorological stations and instrumental timeseries.
#' The user can easily use the package with their own data after replacing the values of such variables.
#'
#' @format Data frames and vectors
#'
#' @source GSOD (through GSODR package)
#'
#' This dataset is intended for research purposes only, being distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY.
#'
#' @usage data(db)
#' @docType data
#' @name db
#' @aliases db ssdb
#' @keywords datasets
#' 
#' 
#' @examples
#' 
#' ## see below
#'
#' 
#'    library(nomnoml)
#'    data(db)
#'    str(db)
#'    schema <- lapply(db, FUN=function(t, nfks) {
#'     print(nfks)
#'     o <- sapply(as.list(t), FUN=function(x) {class(x)})
#'     o2 <- data.frame(column=names(o), type=as.character(o))
#'       o2$reference <- as.character(NA)
#'       iin <- which(o2$column %in% nfks)
#'       o2$reference[iin] <- o2$column[iin]
#'       o2$reference[iin] <- stringr::str_sub(o2$reference[iin], end=-4)
#'       return(o2)
#'     }, nfks=sprintf("%s_id", names(db)))
#' 
#'     out2 <- list()
#'     for (it in names(schema)) {
#'       print(it)
#'       content <- paste(schema[[it]]$column, schema[[it]]$type, sep = ": ", collapse = "||")
#'       out2[[it]] <- sprintf("[<table>%s| %s]", it, content)
#'       inn <- which(!is.na(schema[[it]]$reference))
#'       if (length(inn) > 0) { 
#'       refs <- sprintf("[%s] <- 1..n[%s]", schema[[it]]$reference[inn], it)
#'       out2[[it]] <- c(out2[[it]], refs)
#'     }
#'  }
#' 
#'  nomnoml(paste(unlist(out2), collapse="\n"), svg=TRUE)
#'   
#'   
#' 
#' 
#' 


NULL
