library(DiagrammeR)
library(shiny)
library(shinyAce)
library(leaflet)   # devtools::install_github("booysej/leaflet-shiny")
library(shinyTree) # devtools::install_github("booysej/shinyTree")
library(shinyBS)
library(shinythemes)
library(DT)
library(rCharts)
library(rpivotTable);

shinyUI(fluidPage( theme = shinytheme("spacelab"),
      
  tags$head(
    #includeCSS("style.css"),
    #includeScript("busy.js"),
    includeCSS("styles.css"),        
    includeScript("gomap.js"),        
    tags$style(type="text/css", "select.pvtAggregator {width: 55px;}"),
    #tags$style(type="text/css", "select.pvtRenderer {width: 120px;}"),  
    tags$style(type="text/css", "select.pvtAttrDropdown {width: 100px;}"),  
    tags$style(type="text/css", "table#locktable { border-collapse:separate; border-spacing:0 5px;}") 
  ),    
  navbarPage("CRIDF", id="nav",collapsible=T,
             
             tabPanel("Dashboard 1",  
                                          
                      bsCollapse(id="story",  open="Map View",multiple=T,                                          
                          bsCollapsePanel("Map",
                              
                               conditionalPanel(
                                 condition = "output.d1m1==null ",                                    
                                 div(class = "busy",
                                     p("Loading Map ..."),
                                     img(src="ajaxloaderq.gif")
                                 )
                               ),
                               leafletOutput("d1m1", width="100%", height="280px"),                               
                               tags$div(style=" position: absolute;left: 70px;top: 280px;",
                                 sliderInput("d1year", " Tx Flows Year", 2010, 2030, 2015,1,animate=list(loop=TRUE))
                               )
                          ),
                          
                          bsCollapsePanel("Capacity Expansion Plan",
                            showOutput("d1t1", "highcharts") 
                          ),   
                          bsCollapsePanel("Graphs",     
                               
                                 showOutput("d1t2a", "highcharts"),
                                 showOutput("d1t2b", "highcharts"),
                                 showOutput("d1t2c", "highcharts"),
                                 showOutput("d1t2d", "highcharts")
                                                                  
                          ),
                          bsCollapsePanel("Settings",
                                          fluidRow(
                                            column(4,sliderInput("d1water", "Water Availability", 
                                                                 min(as.numeric(gsub("%","",as.character(unique(tdataseries$Water.Availability))))), 
                                                                 max(as.numeric(gsub("%","",as.character(unique(tdataseries$Water.Availability))))), 
                                                                 min(as.numeric(gsub("%","",as.character(unique(tdataseries$Water.Availability))))),
                                                                 20,
                                                                 animate=FALSE)),
                                            column(4,sliderInput("d1uclf", "Coal UCLF", 
                                                                 min(as.numeric(gsub("%","",as.character(unique(tdataseries$Coal.UCLF))))), 
                                                                 max(as.numeric(gsub("%","",as.character(unique(tdataseries$Coal.UCLF))))), 
                                                                 min(as.numeric(gsub("%","",as.character(unique(tdataseries$Coal.UCLF))))),
                                                                 20,
                                                                 animate=FALSE)),
                                            column(4,selectInput("d1policy", "Choose Policy:",
                                                                 as.character(unique(tdataseries$policy))
                                                                 ,multiple=F,selected="unconstraint"))
                                          )                                          
                          )                          
                      )    
             ),
             tabPanel("Dashboard 2",  
                      fluidRow(
                        column(6,
                               conditionalPanel(
                                 condition = "output.d2m1==null ",                                    
                                 div(class = "busy",
                                     p("Loading Map ..."),
                                     img(src="ajaxloaderq.gif")
                                 )
                               ),
                               leafletOutput("d2m1", width="100%", height="500px")
                        ),
                        column(6,
                               showOutput("d2t1", "highcharts")                               
                        )
                      ),
                      fluidRow(
                        column(6,
                               showOutput("d2t2", "highcharts")
                        ),
                        column(6,
                               showOutput("d2t3", "highcharts")
                        )
                      )
             )
             
           
  ) # navbar
  
  
  
))
