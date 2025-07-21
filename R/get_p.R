NULL
#' Get station (or measurement points)
#'
#'
#' @param x database or dateset, e.g  like \code{\link{db}}
#' @param p spatial (e.g. \code{sf}) object in which extract the observation station. 
#' @param p_table_name p table name 
#' @param data.table,sf,... further arguments \code{\link{get_table}}
#'
#' @export
#' 
#' @importFrom sf st_union st_geometry st_intersection st_crop
#' 
#' 
#' @examples
#' 
#' data(db)
#' p <- get_p(x=db)
#' 
#' library(terra)
#' library(leaflet)
#' 
#' plet(vect(p),cex=10)
#' 
#' ### IN an area 
#' 
#' px <- p[49,] |> st_buffer(dist=250*1000) 
#' p1 <- get_p(x=db,p=px)
#' 
#' plet(vect(p),cex=10)
#' 
#' px <- p[49,]
#' p1 <- get_p(x=db,p=px)
#' 
#' plet(vect(p),cex=10)
#' 
#'
#' north <- -2.25
#' south <- -8.85
#' west <- 29.45
#' east <- 32.15
#' pee <- rast(xmin=west,xmax=east,ymin=south,ymax=north,crs="EPSG:4326")
#' p2 <- get_p(x=db,p=pee)
#'
#'

get_p <- function(x,p=NULL,p_table_name="p",data.table=FALSE,sf=TRUE,...) {
  
  
  out <- get_table(x,table_name=p_table_name,data.table=data.table,sf=sf,...)
  
  if (inherits(p,"sf")) {
    p <- p |> st_as_sf() |> st_geometry() |> st_union()
    out <- st_intersection(out,p)
   ## st_insersect()
  } else if (!is.null(p)) {
    
    out <- st_crop(out,p)
  }
  
  return(out)
  
}