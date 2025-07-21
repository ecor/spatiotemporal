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
library(shiny)

source('./global.R')


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width=3,
      radioButtons("basemap", "Select basemap:", 
                   choices = c("Esri World Imagery" = "Esri.WorldImagery", 
                               "OpenTopoMap" = "OpenTopoMap",
                               "OpenStreetMap" = "OpenStreetMap"))
    ),
    mainPanel(
      leafletOutput("map",height =1000),
      tabsetPanel(
        tabPanel(tab_names[1], dygraphOutput("dygraph1")),
        tabPanel(tab_names[2], dygraphOutput("dygraph2")),
        tabPanel(tab_names[3], dygraphOutput("dygraph3")),
      )
    )
  )
)