# 
# App Shiny - geo8 Template : Dynamic Geographic Map with Leaflet and Time Series 
# Author: Emanuele Cordano
# Date: October 2023
# License: GPL-3
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
#
library(leaflet)
library(shiny)

source('./global.R')


plotted_ts <- function(p_id0,variable_id0,db=db) {
 
####  db$ts %>% dplyr::filter(p_id==p_id0,variable_id==variable_id0) %>% 
  db %>% get_ts(p_id=p_id0,variable_id=variable_id0) %>%  
  dplyr::select(timestamptz,value) %>% arrange(timestamptz)}


main_ts <- function(p_id0,variable_id0,db=db) {"%s at %s" %>% 
  sprintf(db$variable$name[variable_id0],
          db$p$name2[p_id0])}





function(input, output, session) {
  output$map <- renderLeaflet({
    ll <- leaflet() %>% addScaleBar(position="bottomleft") %>%
      addProviderTiles(providers[[input$basemap]]) %>%
      addMarkers(data = db$p, popup = ~paste("Name:", name2))
    
    if (!is.null(input$map_bounds)) {
      north <- input$map_bounds$north
      south <- input$map_bounds$south
      east <- input$map_bounds$east
      west <- input$map_bounds$west
      ll <- ll %>% fitBounds(west, south, east, north)
    }
    ll
  })
  
  observeEvent(input$map_marker_click, {
    clickp <- input$map_marker_click
  
    lng <- clickp$lng
    lat <- clickp$lat
    pcoords <- st_coordinates(db$p)
    tole <- 10^-5
    ip <- which(abs(pcoords[,1]-lng)<=tole & abs(pcoords[,2]-lat)<=tole)
   
    # 
    # 
    # # point lick
    # click_point <- st_sfc(st_point(c(lng, lat)), crs = 4326) %>% 
    #   st_transform(st_crs(db$p))
    # ip <- which(st_is_within_distance(x=click_point,y=db$p,dist=1000,sparse=FALSE)[,1])
    ip <- ip[1]
    
    
    
   
    ##selected <- db$p[data$lat == click$lat & data$lng == click$lng, ]
    
    p_id0 <- db$p$ID[ip]
    output$dygraph1 <- renderDygraph({
      ####
      iref <- 1
      
      ####
      print("here")
      print(p_id0)
      ## here is the time series!!!
      variable_id0 <- visualized_vars$ID[iref]

      dygraph(plotted_ts(p_id0=p_id0,variable_id0=variable_id0,db=db),main=main_ts(p_id0=p_id0,variable_id0=variable_id0,db=db)) %>% 
        dyRangeSelector()
    })
    
    output$dygraph2 <- renderDygraph({
      ####
      iref <- 2
      
      ####
      
      
      ## here is the time series!!!
      ## here is the time series!!!
      variable_id0 <- visualized_vars$ID[iref]
      dygraph(plotted_ts(p_id0=p_id0,variable_id0=variable_id0,db=db),main=main_ts(p_id0=p_id0,variable_id0=variable_id0,db=db)) %>% 
        dyRangeSelector()
    })
    
    output$dygraph3 <- renderDygraph({
      ####
      iref <- 3
      
      ####
      
      
      ## here is the time series!!!
      ## here is the time series!!!
      variable_id0 <- visualized_vars$ID[iref]
      
      dygraph(plotted_ts(p_id0=p_id0,variable_id0=variable_id0,db=db),main=main_ts(p_id0=p_id0,variable_id0=variable_id0,db=db)) %>% 
        dyRangeSelector()
    })
    
    
  })
}