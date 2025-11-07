NULL
#' Get projects
#'
#'
#' @param x database or dateset, e.g  like \code{\link{db}}
#' @param project_table_name project table name 
#' @param ... further arguments
#'
#' @export
#' @examples
#' 
#' data(db)
#' projects <- get_projects(x=db)
#' 
#' 


get_projects <- function(x,project_table_name="project",...) {
  
  
  out <- get_table(x,table_name=project_table_name,...)
  
  return(out)
  
}