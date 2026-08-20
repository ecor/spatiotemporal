NULL
#' Get attributes
#'
#'
#' @param x database or dateset, e.g  like \code{\link{db}}
#' @param attribute_table_name attribute table name 
#' @param ... further arguments
#'
#' @export
#' @examples
#' 
#' data(db)
#' attributes <- get_attribute(x=db)
#' attributes_prcp <- get_attribute(x=db,name="PRCP_ATTRIBUTES")
#' 


get_attribute <- function(x,attribute_table_name="attribute",...) {
  
  
  out <- get_table(x,table_name=attribute_table_name,...)
  
  return(out)
  
}