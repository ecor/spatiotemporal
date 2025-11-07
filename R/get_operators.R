NULL
#' Get operators
#'
#'
#' @param x database or dateset, e.g  like \code{\link{db}}
#' @param operator_table_name operator table name 
#' @param ... further arguments
#'
#' @export
#' @examples
#' 
#' data(db)
#' operators <- get_operators(x=db)
#' 
#' 


get_operators <- function(x,operator_table_name="operator",...) {
  
  
  out <- get_table(x,table_name=operator_table_name,...)
  
  return(out)
  
}