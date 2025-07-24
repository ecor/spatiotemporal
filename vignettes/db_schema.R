## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, results='asis'-----------------------------------------------
library(spatiotemporal)

help(package="spatiotemporal")

help("get_table")

help("get_ts")

help("get_p")

help("get_variables")

help(db)


## ----echo=FALSE, results='asis',fig.show='hold',fig.height=100----------------

library(spatiotemporal)
library(nomnoml)
library(stringr)

data(db)


## DATA BASE SCHEMA DESIGN

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

nomnoml(paste(unlist(out2), collapse="\n"), png=TRUE,width=600)




