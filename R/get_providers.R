NULL
#' Get providers
#'
#'
#' @param x database or dateset, e.g  like \code{\link{db}}
#' @param provider_table_name provider table name 
#' @param ... further arguments
#'
#' @export
#' @examples
#' 
#' data(db)
#' providers <- get_providers(x=db)
#' 
#' 


get_providers <- function(x,provider_table_name="provider",...) {
  
  
  out <- get_table(x,table_name=provider_table_name,...)
  
  return(out)
  
}