NULL
#' Get a teble, eg. variables (or measurement types)
#'
#'
#' @param x database or dateset, e.g  like \code{\link{db}}
#' @param table_name variable table name 
#' @param vfilter (possible) character string containg the filtering condition. See \code{dplyr::\link{filter}}.
#' @param data.table logical option. If \code{TUE} returns a \code{data.table} object
#' @param sf logical option.  If \code{TRUE} returns a \code{\link{sf}} object. 

#' @param ... further arguments
#'
#'
#' @importFrom sf st_as_sf
#' @importFrom data.table as.data.table
#' @importFrom rlang parse_expr
#' @importFrom dplyr filter
#' 
#' @export
#' @examples
#' 
#' data(db)
#' table <- get_table(x=db)
#' table <- get_table(x=db,table_name="ts",vfilter="p_id==4")
#' table2 <- get_table(x=db,table_name="ts",p_id=4)
#' table4 <- get_table(x=db,table_name="ts",p_id=c(3,4,21))
#' 


get_table <- function(x,table_name="ts",vfilter=NULL,data.table=TRUE,sf=FALSE,...) {
  
  
  
  
  

  
  out <- x[[table_name]]
  if (length(vfilter)>0) for (i in 1:length(vfilter)) {
    condition <- vfilter[[i]]
    out <- out |> dplyr::filter(!!rlang::parse_expr(condition))
    
    
  }
  further_args <- list(...)
  ### 
  lfargs <- further_args |> sapply(length)
  further_args <- further_args[which(lfargs>0)]
  
  ###
  
  if (length(further_args)>0) for (it in names(further_args)) {
    
    values <- further_args[[it]] |> as.numeric() 
   
    if (any(is.na(values) & !is.na(further_args[[it]]))) {
      
      values <- further_args[[it]]
      
    }
   
   
    verb <- "%in%"
   
    if (is.character(values))  {
      values <- paste0("'",values,"'")
    
    }
    values <- values |> paste(collapse=",")
    condition <- sprintf("%s %s c(%s)",it,verb,values) 
    
    out <- out |> dplyr::filter(!!rlang::parse_expr(condition))
    
  }
    
  if (data.table) {
    out <- as.data.table(out)
  } else if (sf) {
    out <- st_as_sf(out)
  }
  
  return(out)
  
}