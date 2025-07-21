NULL
#' Get a time series from spatio-temporal database, eg. variables (or measurement types)
#'
#'
#' @param x database or dateset, e.g  like \code{\link{db}}
#' @param ts_table_name ts variable table name
#' @param get_p_args list of possible arguments for \code{\link{get_p}} 
#' @param get_variable_args  list of possible arguments for \code{\link{get_variable}}  
#' @param p_id id number for \code{p} table
#' @param variable_id id number for \code{variable} table
#' @param timestep timstep used between two adjacent timestamps . Unit is second. It is used if \code{regular_timestamptz==TRUE}.
#' @param regular_timestamptz logical. Default is \code{FALSE} . If it is \code{TRUE} the timestamps are regularly distributed with possible \code{value} values equal to \code{NA}. 
#' @param ... further arguments
#' 
#' 
#' @importFrom dplyr full_join arrange
#' @importFrom data.table data.table
#' @importFrom rlang .data
#' @export
#' 
#' 
#' @examples
#' 
#' data(db)
#' p <- get_p(x=db)
#' px <- p[49,] |> st_buffer(dist=250*1000) 
#' out <- get_ts(x=db,get_p_args=list(p=px),get_variable_args=list(name=c("MIN","MAX","PRCP")))
#' 
#' ## Precipitation in one site
#' 
#' px <- p[49,] |> st_buffer(dist=1)  
#' prcp <- get_ts(x=db,get_p_args=list(p=px),
#'   get_variable_args=list(name=c("PRCP")),
#'   regular_timestamptz=TRUE)
#' 
#
#' ## Aggregation through summarize ## chech quality after 1981
#' library(dplyr)
#' library(ggplot2)
#' 
#' ddata <- prcp |> filter(timestamptz>as.POSIXct("1980-12-31")) |> 
#'  group_by(attrvalue) |> summarize(nvalues=length(value)) |> ungroup()
#'
#' ccols <- list(
#'  A="#a50026", #A = 1 report of 6hour precipitation amount;
#'  B="#d73027", # B = Summation of 2 reports of 6-hour precipitation amount;
#'  E="#f46d43", #E = 1 report of 12-hour precipitation amount;
#'  D ="green", #D= Summation of 4 reports of 6-hour precipitation amount;
#'  H="#fdae61",
#'  # H = Station reported 0 as the amount for the day (e.g. from 6 hour reports), 
#'  # but also reported at least one occurrence of precipitation in hourly observations
#'  # this could indicate a trace occurred, but should be considered as incomplete data for the day;
#'  I="#fee08b",
#'  # I = Station did not report any precipitation data for the day and did not report any occurrences of precipitation in its hourly observations-it's still possible that precipitation occurred but was not reported;
#'  C="#d9ef8b",# C = Summation of 3 reports of 6-hour precipitation amount;
#'  F="#a6d96a",# F = Summation of 2 reports of 12-hour precipitation amount;
#'  G="#66bd63"  # G = 1 report of 24-hour precipitation amount;
#' 
#' ) |> unlist()
#' 
#' title <- sprintf("Precipitation Atrributes since %s",px$name2[1])
#' 
#'  gg <- ggplot(ddata, aes(x = "", y = nvalues, fill = attrvalue)) +
#'   geom_bar(width = 1, stat = "identity") +
#'   coord_polar("y", start = 0) +
#'   scale_fill_manual(values = ccols) +  # Aggiungi i colori personalizzati
#'   theme_void() +
#'   labs(title = title)
#' 
#' gg
#' 
#' 

get_ts <- function(x,ts_table_name="ts",get_p_args=list(),
                   get_variable_args=list(),p_id=NA,variable_id=NA,
                   timestep=24*3600,regular_timestamptz=FALSE,
                   ...) {
  
  
  if (all(is.na(p_id))) p_id <- NULL
  if (all(is.na(variable_id))) variable_id <- NULL
  
  if (length(get_p_args)>0){
    get_p_args$x <- x 
    get_p_formals <- formals(get_p) |> as.list()
    get_p_formals <- get_p_formals[names(get_p_formals)!="..."]
    get_p_formals <- get_p_formals[!(names(get_p_formals) %in% names(get_p_args))]
    if (length(get_p_formals)>0) {
      
      get_p_args <- c(get_p_formals,get_p_args)
      
      
      
      
    }
    
    ## p 
    p <- do.call(what="get_p",args=get_p_args)
    
    p_table_name <- get_p_args$p_table_name
    
   ## p_ids <- unique(p$ID)
    
    if (is.null(p_id)) p_id <- unique(p$ID)
    ## get p_id where to get ts 
    
    ## TO DO 
  }
  ## VARIABLES 
  if (length(get_variable_args)>0){
    get_variable_args$x <- x 
    get_variable_formals <- formals(get_variable) |> as.list()
    get_variable_formals <- get_variable_formals[names(get_variable_formals)!="..."]
    get_variable_formals <- get_variable_formals[!(names(get_variable_formals) %in% names(get_variable_args))]
    if (length(get_variable_formals)>0) {
      
      get_variable_args <- c(get_variable_formals,get_variable_args)
      
      
      
      
    }
    
    ## p 
    variable <- do.call(what="get_variable",args=get_variable_args)
    
    ##variable_table_name <- get_variable_args$p_table_name
    
   ## p_ids <- unique(p$ID)
    
    if (is.null(variable_id)) variable_id <- unique(variable$ID)
    ## get p_id where to get ts 
    
    ## TO DO 
  }
  
    
  
  
  out <- get_table(x,table_name=ts_table_name,p_id=p_id,variable_id=variable_id,...)
  if (regular_timestamptz==TRUE) {
    rr <- range(out$timestamptz)
    out <- out |> dplyr::full_join(data.table::data.table(timestamptz=seq(from=rr[1],to=rr[2],by=timestep))) |> 
      dplyr::arrange(.data$timestamptz)
  }
  ###
  #unique <- length(unique(out$p_id))==1
  #unique <- length(unique(out$variable_id))==1
  
  
  
  
  return(out)
  
}