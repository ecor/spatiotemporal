NULL
#' Get actions
#'
#'
#' @param x database or dateset, e.g  like \code{\link{db}}
#' @param action_table_name action table name 
#' @param ... further arguments
#'
#' @export
#' @examples
#' 
#' data(db)
#' actions <- get_action(x=db)
#' 
#' 


get_action <- function(x,action_table_name="action",...) {
  
  
  out <- get_table(x,table_name=action_table_name,...)
  
  return(out)
  
}