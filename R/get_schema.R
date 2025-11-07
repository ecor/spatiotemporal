NULL
#' 
#' Render/show DB schema
#'
#' @param db db
#' @param ... further arguments
#'
#' @export
#' 
#' @importFrom stringr str_sub
#'
#' @examples
#' 
#' data(db)
#' 
#' library(nomnoml)
#' 
#' 
#' 
#' out <- get_schema(db)
#' 
#' nomnoml(out,png=TRUE,width=600)
#' 







get_schema <- function(db,...) {
  schema <- lapply(db, FUN=function(t, nfks) {
    print(nfks)
    o <- sapply(as.list(t), FUN=function(x) {class(x)})
    o2 <- data.frame(column=names(o), type=as.character(o))
    o2$reference <- as.character(NA)
    iin <- which(o2$column %in% nfks)
    o2$reference[iin] <- o2$column[iin]
    o2$reference[iin] <- str_sub(o2$reference[iin], end=-4)
    return(o2)
  }, nfks=sprintf("%s_id", names(db)))
  
  out2 <- list()
  for (it in names(schema)) {
    print(it)
    content <- paste(schema[[it]]$column, schema[[it]]$type, sep = ": ", collapse = "||")
    out2[[it]] <- sprintf("[<table>%s| %s]", it, content)
    inn <- which(!is.na(schema[[it]]$reference))
    if (length(inn) > 0) { 
      refs <- sprintf("[%s] <- 1..n[%s]", schema[[it]]$reference[inn], it)
      out2[[it]] <- c(out2[[it]], refs)
    }
  }
  outq <- paste(unlist(out2), collapse="\n")
  ###nomnoml(outq, png=png,width=width,...)

  return(outq)


}


