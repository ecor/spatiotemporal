NULL
#' Put a table, e.g. variables (or measurement types)
#'
#' @param x object to put in the \code{db}
#' @param db database or dateset, e.g  like \code{\link{db}}
#' @param table_name variable table name 
#' @param vfilter (possible) character string containg the filtering condition. See \code{dplyr::\link{filter}}.
#' @param data.table logical option. If \code{TUE} returns a \code{data.table} object
#' @param sf logical option.  If \code{TRUE} returns a \code{\link{sf}} object. 

#' @param ... further arguments
#'
#'
#' @importFrom sf st_as_sf st_transform st_crs 
#' @importFrom data.table as.data.table
#' @importFrom rlang parse_expr
#' @importFrom dplyr filter left_join
#' @importFrom stringr str_detect str_split str_length
#' 
#' @export
#' @examples
#' 
#' data(db)
#' table_name="provider"
#' new_providers <- data.table(name=c("asdf","qwerty"))
#' 
#' db <- put_in_table(new_providers,db=db,table_name=table_name)
#' 
#' ###
#' table_name="variable"
#' new_variable_name <- "rainfall"
#' new_variables <- data.table(name=new_variable_name,provider_name="asdf")
#' 
#' db <- put_in_table(new_variables,db=db,table_name=table_name)
#' 
#' 
#' 
#' 
#' 
#' 


put_in_table <- function(x,db,table_name="provider",vfilter=NULL,data.table=(table_name!="p"),sf=(table_name=="p"),...) {
  
  
  new_table <- db[[table_name]]
  ntables <- names(db)
  external_ids <- paste0(names(db),"_id")
  names(ntables) <- external_ids
  print(external_ids)
  iids <- which(names(new_table) %in% external_ids)
  # print(names(new_table))
  # print(iids)
  if (!sf) x <- as.data.frame(x)
  
  ####
  if (length(iids)>0) {
    external_ids1 <- names(new_table)[iids]
    print(external_ids1)
    external_names1 <- ntables[external_ids1]
    print(external_names1)
    print(names(x))
    iids2 <- which(!(external_ids1 %in% names(x)))
    print(iids2)
    external_namesz <- external_names1[iids2]
    if (length(external_namesz)>0) {
      # print(external_namesz)
      # external_namesz0 <<- external_namesz
      for (namez in external_namesz) {
        # print(table_name)
        # print(namez)
        # namez1 <<- namez
        # xx <<- x
        ntablez <- paste0(namez,"_")
        ll <- str_length(ntablez)
        ## DO SOMETHING
       
        iuu <- which(str_sub(names(x),end=ll)==ntablez)
        aargs <- as.data.frame(x)[,iuu,drop=FALSE]
        ot <- db[[namez]] ##get_table(x=db,name=namez)
        names(ot)[names(ot)=="ID"] <- "id"
        names(ot) <- paste(namez,names(ot),sep="_")
        ot <- as.data.frame(ot)
        ot <- ot[,names(ot) %in% c(names(x),paste0(namez,"_id"))]
        
        ot2 <- left_join(x,ot)
        x <- ot2
        
        
        # print(table_name)
        # print(namez)
        # namez1 <<- namez
        # xx <<- x
        # iuu1 <<- iuu 
        # aargs1 <<- aargs
        # names(aargs) <- str_split(names(aargs),"_",n=2) |> sapply(FUN=function(x){x[[2]]}) 
        # aargs <- aargs |> as.list()
        # aargs$table_name <- namez
        # aargs$x <- db
        # what <- get_table
        # aargs <<- aargs
        # ot <- do.call(what=what,args=aargs)
        # ##print(ot)
        # x[,paste0(namez,"_id")] <- ot$ID
      }
      
      
      
    }
    
    
    
  }
  ####
  
  

  x$ID <- 1:nrow(x)+max(new_table$ID)
  ####
  iinx <- which(!(names(new_table) %in% names(x)))
  if (length(iinx)>0) {
    
    nnx <- names(new_table)[iinx]
    x[,nnx] <- as.numeric(NA)
    
    
  } 
  
  
  ####
  x <- x[,names(new_table)]
  ### CHECK primary and foreing IDS not be NA
  inid <- which(str_sub(names(x),start=-2)=="id")
  ####
  if (length(inid)>0) if (any(is.na(x[,inid]))) stop("foreing and/or primary keys (id) cannot be NA!")
  ####
  #print(data.table)
  #print(table_name)
  ####
  if (data.table) {
    x <- as.data.table(x)
    db[[table_name]] <- rbind(new_table,x)
  } else if (sf) {
    #x <- st_as_sf(x)
    x <- st_transform(x,crs=st_crs(new_table))
  

    db[[table_name]] <- rbind(new_table,x)
  }
  
  #db[[table_name]] <- rbind(new_table,x)
  
  
  return(db)
  
}