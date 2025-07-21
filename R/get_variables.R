NULL
#' Get variables (or measurement types)
#'
#'
#' @param x database or dateset, e.g  like \code{\link{db}}
#' @param variable_table_name variable table name 
#' @param ... further arguments
#'
#' @export
#' @examples
#' 
#' data(db)
#' variables <- get_variable(x=db)
#' variables <- get_variable(x=db,name="PRCP")
#' 


get_variable <- function(x,variable_table_name="variable",...) {
  
  
  out <- get_table(x,table_name=variable_table_name,...)
  
  return(out)
  
}