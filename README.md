
# spatiotemporal

An R package for a spatiotemporal database schema. It has been created to handle weather data from weather stations. An example from GSODR.

See  manual for further details.

## Installation from R console 

```

## Installiation from Github repo through Gituh token 

remotes::install_github("ecor/spatiotemporal")
```

## Usage from R console 

```
library(spatiotemporal)

help(package="spatiotemporal")

help("get_table")

help("get_ts")

help("get_p")

help("get_variables")

help(db)

```

## Database Schema 


It can retrievd by this code: 



```

library(spatiotemporal)
library(nomnoml)


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

nomnoml(paste(unlist(out2), collapse="\n"), svg=TRUE)



```




