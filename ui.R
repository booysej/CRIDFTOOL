###### Author: Jacques Booysen #####
###### booysenjacques@gmail.com ####
#library(DiagrammeR)
library(shiny)
#library(shinyAce)
library(leaflet)   # devtools::install_github("booysej/leaflet-shiny")
library(shinyTree) # devtools::install_github("booysej/shinyTree")
library(shinyBS)
library(shinythemes)
library(RSQLite)
library(DT)
library(rCharts)
library(rpivotTable);
library(shinyGridster)
library(shinydashboard)
library(shinyIncubator);
dir.exists = file.exists

shinyUI(dashboardPage( skin = "blue",
 dashboardHeader(titleWidth=400, title = "CRIDF SAPP Scenario Planning Tool:"
                 ),
 dashboardSidebar(
   tags$head(
     includeCSS("styles.css"),
     includeCSS("www/ui/jquery-ui.css"),        
     includeScript("gomap.js"),
     
     tags$style(type="text/css", "select.pvtAggregator {width: 55px;}"),
     tags$style(type="text/css", "select.pvtAttrDropdown {width: 100px;}"),
     tags$style(type="text/css", ".selectize-control {width: 200px;}"),  
     tags$style(type="text/css", "table#locktable { border-collapse:separate; border-spacing:0 5px;}") ,
     tags$style(type="text/css", ".navbar {margin-bottom: 0px;}"),
     tags$style(type="text/css", "label {font-size: 90%;}"),
     tags$style(type="text/css", ".item {font-size: 90%;}"),
     tags$style(type="text/css", ".google-visualization-charteditor-dialog {width: 70%; z-index: 999;}"),
     tags$style(type="text/css", "li.ui-state-default.ui-state-hidden[role=tab]:not(.ui-tabs-active) {display: none;}"),

     tags$style(type="text/css", ".sortable {
       width: 100%;
       font-size: 0;
       padding: 1px;
       border-radius: 0px;
     }"),
     
     tags$style(type="text/css", ".portlet {
       font: 12px/1.3 sans-serif;
       margin: 3px;
       padding: 1px;
       display: inline-block;
       vertical-align: top;
       height: auto;
     }"),
     
     tags$style(type="text/css", ".portlet.span-1 { width: 32%; }"),
     tags$style(type="text/css", ".portlet.span-2 { width: 65%; }"),
     tags$style(type="text/css", ".portlet.span-3 { width: 97%; }"),
     
     tags$style(type="text/css", ".portlet-header {
       margin: 3px;
       padding: 1px 0 2px 3px;
     }"),
     
     tags$style(type="text/css", ".portlet-header .ui-icon {
       float: right;
     }"),
     
     tags$style(type="text/css", ".portlet-content {
       padding: 4px;
     }"),
     
     tags$style(type="text/css", ".portlet-minimized {
       height: auto;
     }"),
     
     tags$style(type="text/css", ".portlet-minimized .portlet-content {
       display: none;
     }"),
     
     tags$style(type="text/css", ".ui-sortable-placeholder {
       border: 1px dotted black;
       visibility: visible !important;
     }")
     
   ),
   
             
   selectInput("dboard", "Dashboard View:",
               availabledashboards
   ),
                  sidebarMenu(id="step",
                    menuItem("Step 0 (Overview)",tabName = "STEP0",icon=icon("glyphicon glyphicon-info-sign",lib = "glyphicon")),
                    menuItem("DashBoard", tabName = "STEP1",icon=icon("desktop")),
                    #menuItem("Step 2 (Lock Policies)", tabName = "STEP2",icon=icon("desktop")),
                    #menuItem("Step 3 (Sensitivities)", tabName = "STEP3",icon=icon("desktop")),
                    menuItem("Input Data Explorer", tabName = "IDE",icon=icon("table")),
                    menuItem("Help", tabName = "HELP",icon=icon("glyphicon glyphicon-question-sign",lib = "glyphicon")),
                    sliderInput("daterange", label = "Filter Date Range:", min = 2011, 
                                max = 2050, value = c(2015, 2025),ticks=FALSE,width="600px")
                  ),
                  
   
   
        conditionalPanel("input.step=='STEP1'",
                         tags$table(style="border-collapse: separate;border-spacing: 10px 5px;",
                           tags$tr(tags$td(align="center",
                             actionButton("mandashboards1","Manage Dashboard",icon=icon("table"))
                           )),
                           tags$tr(tags$td(align="center",
                            actionButton("savedashboards1","Save Screen Layout",icon=icon("save"))
                           )),
                           tags$tr(tags$td(align="center",
                                    bsAlert("savedashboardalert")
                           ))
                         ))
                   
                

   ),  
 dashboardBody(
   bsModal("managedash", "Manage Dashboard Items", "mandashboards1", size = "large",
           conditionalPanel(
             condition = "output.mandash==null ",                                    
             div(class = "busy",
                 p("Loading Dashboard Items ..."),
                 img(src="ajaxloaderq.gif")
             )
           ),
           DT::dataTableOutput('mandash'),
           actionButton("adddashboards1","Add Dashboard Item",icon=icon("plus") ), 
           actionButton("remdashboards1","Remove Dashboard Item",icon=icon("minus") )
   ),
   bsModal("addcomp", "Select a new Dashboard Item to Add(+)", "adddashboards1", size = "large",
           DT::dataTableOutput('addcompdash'),
           #uiOutput("compprop"),
           actionButton("addcomponent","Add Dashboard Item",icon=icon("plus") ) 
   ),
   tabItems(
     tabItem(tabName = "STEP0",
             tabsetPanel(id="overview",     
                         tabPanel("Photo",
                                  fluidRow(
                                    column(2,
                                           tags$h4("STEP 0 - Overview")),                                                                                              
                                    column(2,bsButton("s0s1","Next (Step1) >>",style="primary")) #,verbatimTextOutput("queryText"))
                                  ),
                                  tags$img(src="overview.gif")
                         ),
                         tabPanel("Text",
                                  fluidRow(
                                    column(6,tags$h4("STEP 0 - Overview")),                                                                                              
                                    column(6,bsButton("s0s1","Next (Step1) >>",style="primary"))
                                  ),
                                  tags$h1("Executive Summary"),
                                  tags$p("The Climate Resilient Infrastructure Development Facility (CRIDF) has identified the need to deliver a 
                                                 user friendly, graphically driven, SADC regionalised long-term energy planning and scenario 
                                                 tool to determine climatic and economic impacts in the region over the coming 30 years (2015-2045).
                                                 Until now, individual country, as well as regionally co-ordinated integrated resource and master 
                                                 planning studies have been undertaken, e.g. IRENA SPLAT, or may be currently in progress.  It is not 
                                                 the intention of this project to duplicate any of these projects but rather to build and enhance on the 
                                                 outputs of these studies. The proposed model will be unique in that it will include the effects of 
                                                 sustainable water utilisation, long term temperature changes as well as economic feedback (e.g. lower 
                                                 energy prices will stimulate economic growth), into the future projections of energy and demand."),
                                  tags$p("Enerweb-EOH is proposing a three phase approach to delivering a visually interactive optimisation 
                                                 tool, allowing economic and climatic scenarios to be investigated. In the first phase, the tool will use 
                                                 the already established local and regional generation and transmission expansion plans as input data.  
                                                 After the first phase of the project, a demonstration solution will be shown at the 45th SAPP 
                                                 Conference, after which a dynamic optimiser, which includes climate and economic feedback 
                                                 modules, will be delivered as a second phase. During the third phase, it is proposed to deliver two 
                                                 detailed case studies, to ensure transfer, training and embedding of the tool in the SAPP participants."),
                                  tags$p("The successful delivery of this project will see a scenario analysis type of tool being made available to 
                                                 country and regional electricity planners, financiers, developers, government and non-government 
                                                 bodies.  It will specifically enable and facilitate a visually interactive experience, containing a GIS 
                                                 graphical type interface, and require minimal training. The user will be able to gain valuable insights 
                                                 from comparisons and differences between different future scenarios around climatic and economic 
                                                 influences. This will empower decision makers and financiers to make better informed decisions which 
                                                 include the influences of climatic sustainability and economic dynamics.")
                         )
             ) 
             
     ), # TabItem STEP0
     
     tabItem(tabName = "STEP1",

             uiOutput("dashboard"),
             conditionalPanel(
               condition = "output.dashboard==null ",                                    
               div(class = "busy",
                   p("   Loading ..."),
                   img(src="ajaxloaderq.gif")
               )
             )
             
     ), # TabItem STEP1
     
     tabItem(tabName = "STEP2",
             fluidRow(
               column(6,tags$h5("STEP 2 - SELECT/LOCK POLICY"),                                                                 
                      bsButton("s2s1","<< Back (Step1)",style="primary"),
                      bsButton("s2s3","Next (Step3) >>",style="primary")                               
               ),                                                                                                                      
               column(6,
                      tags$h5("")
               )                        
             ),
             fluidRow(
               column(5,
                      fluidRow(
                        column(8,
                               uiOutput("availlistui")
                        ),
                        column(4,
                               uiOutput("s2info")
                        )
                      )),
               column(4,tags$span(tags$br(),
                                  bsButton("punlockbase", "<<UNLOCK",style="danger"),
                                  bsButton("plockbase", "LOCK as Baseline>>",style="warning"),
                                  tags$br(),tags$br(),
                                  bsButton("punlockscen", "<<UNLOCK",style="danger"),
                                  bsButton("plockscen", "LOCK as Scenario>>",style="warning")
               )),
               column(3, uiOutput("lockedlistui"))
             ),
             bsAlert("s2alert"),
             conditionalPanel(
               #condition = "(input.lockedbaseline=='NONE') || (input.lockedscenario=='NONE') ",  
               condition = "(1==1)",  
               #################################                                        
               tabsetPanel(id="story2",  selected="New Capacity (Constrained) for Selected",    type="pills",                      
                           
                           tabPanel("New Capacity (Constrained) for Selected", 
                                    fluidRow(                                            
                                      column(12,     
                                             div(class='wrapper',tags$style(".highcharts{height: 100px, width: 300px}"),
                                                 showOutput("d2t1", "highcharts"))                                                              
                                      )
                                    )
                           ),
                           tabPanel("Pivot Table for Selected (Policy)", 
                                    
                                  tags$div(style="overflow-y: scroll;overflow-x: scroll;",
                                    fluidRow(
                                      column(4,
                                             selectInput("d2pivotts", "Filter Time Series:",
                                                         as.character(unique(series1$series))
                                                         ,multiple=T,selected="New Capacity")     ),                            
                                      column(4,
                                             ""),                          
                                      column(4,
                                             "")
                                    ),                                                  
                                    fluidRow(
                                      column(12,     
                                             conditionalPanel(
                                               condition = "output.d2pivot==null ",                                    
                                               div(class = "busy",
                                                   p("Loading Pivot ..."),
                                                   img(src="ajaxloaderq.gif")
                                               )
                                             ),
                                             rpivotTableOutput("d2pivot",width="50%")  
                                             
                                      ) 
                                    )
                                )
                           )
                           
               ) 
               #####
             ) # Conditional
     ),
     tabItem(tabName = "STEP3",
             fluidRow(
               column(2,
                      tags$h5("STEP 3 - CHECK SENSITIVITIES"),
                      bsButton("s3s2","<< Back (Step2)",style="primary"),
                      bsButton("s3s4","Help",style="primary")
               ),
               #column(1, "Fixed Year: 2020 (Keep Centralized Generation Fixed until this year)"),
               column(2,sliderInput("d3water", "Water Availability % (Assumption After 2020)", 
                                    min(unique(runMasterdata[runMasterdata$policy_id==15,]$water.availability))*100, 
                                    max(unique(runMasterdata[runMasterdata$policy_id==15,]$water.availability))*100, 
                                    mean(unique(runMasterdata[runMasterdata$policy_id==15,]$water.availability))*100,
                                    10,
                                    animate=FALSE,ticks=FALSE,width = "100%")),
               column(2,sliderInput("d3uclf", "Coal UCLF % (Assumption After 2020)", 
                                    min(unique(runMasterdata[runMasterdata$policy_id==15,]$coal.uclf))*100, 
                                    max(unique(runMasterdata[runMasterdata$policy_id==15,]$coal.uclf))*100, 
                                    max(unique(runMasterdata[runMasterdata$policy_id==15,]$coal.uclf))*100,
                                    10,
                                    animate=FALSE,ticks=FALSE)),
               column(2,
                      sliderInput("d3uclf2", "Transmission UCLF % (Assumption After 2020)", 
                                  min(unique(runMasterdata[runMasterdata$policy_id==15,]$transmission.uclf))*100, 
                                  max(unique(runMasterdata[runMasterdata$policy_id==15,]$transmission.uclf))*100, 
                                  max(unique(runMasterdata[runMasterdata$policy_id==15,]$transmission.uclf))*100,
                                  10,
                                  animate=FALSE,ticks=FALSE) 
               ),
               column(2,checkboxInput("d3withoutGrandInga", "Without Grand Inga (After 2020)", FALSE)), 
               column(2,
                      sliderInput("d3cons", "Consumption % Adjustment (After 2020)", 
                                  min(unique(runMasterdata[runMasterdata$policy_id==15,]$consumption.adjustment))*100, 
                                  max(unique(runMasterdata[runMasterdata$policy_id==15,]$consumption.adjustment))*100, 
                                  mean(unique(runMasterdata[runMasterdata$policy_id==15,]$consumption.adjustment))*100,
                                  10,
                                  animate=FALSE,ticks=FALSE)
               )
               
               
               
             ),
             tags$span("Test Sensitivities on Baseline and Scenario, if we keep centralized generation CONSTRAINT using the Original selected expansion plans (Step 1 and 2) up until the year 2020, 
                       whereafter the model runs again. "),downloadButton('s3downloadpolicy', 'Download (for Assumption)'),
             bsCollapse(id="story3",  open=c("CHECK: Map View and Tx Energy Flows - click on country to select"),
                        #"CHECK: Sensitivity"),
                        multiple=T,                                 
                        bsCollapsePanel("CHECK: Map View and Tx Energy Flows - click on country to select",style="info",                                                              
                                        fluidRow(
                                          column(12,                                                        
                                                 conditionalPanel(
                                                   condition = "output.d3m1==null ",                                    
                                                   div(class = "busy",
                                                       p("Loading Map ..."),
                                                       img(src="ajaxloaderq.gif")
                                                   )
                                                 ),
                                                 leafletOutput("d3m1", width="80%",height=600),                               
                                                 tags$div(style=" position: absolute;left: 70px;top: 350px;",
                                                          sliderInput("d3year", " Tx Flows Year", 2020, 2031, 2015,1,animate=list(loop=TRUE),ticks=FALSE),
                                                          uiOutput("d3m1type")
                                                          
                                                 )
                                          )                                          
                                        )
                                        
                        ),   
                        
                        bsCollapsePanel("CHECK: Sensitivity",style="info",  
                                        tabsetPanel(id="sensnav",  type="pills", 
                                                    
                                                    tabPanel("Average Price",   
                                                             tags$div(style="overflow-y: scroll;overflow-x: scroll;",
                                                             conditionalPanel(
                                                               condition = "output.d3pivot5==null ",                                    
                                                               div(class = "busy",
                                                                   p("Loading Pivot ..."),
                                                                   img(src="ajaxloaderq.gif")
                                                               )
                                                             ),
                                                             rpivotTableOutput("d3pivot5",width="50%")
                                                             )
                                                    ),        
                                                    
                                                    tabPanel("Total Cost to variability in Actual Water",     
                                                             tags$div(style="overflow-y: scroll;overflow-x: scroll;",
                                                             conditionalPanel(
                                                               condition = "output.d3pivot1==null ",                                    
                                                               div(class = "busy",
                                                                   p("Loading Pivot ..."),
                                                                   img(src="ajaxloaderq.gif")
                                                               )
                                                             ),
                                                             rpivotTableOutput("d3pivot1",width="50%")
                                                             )
                                                    ),
                                                    
                                                    tabPanel("Fuel Cost per Fuel Type in Actual Water",
                                                             tags$div(style="overflow-y: scroll;overflow-x: scroll;",
                                                             conditionalPanel(
                                                               condition = "output.d3pivot2==null ",                                    
                                                               div(class = "busy",
                                                                   p("Loading Pivot ..."),
                                                                   img(src="ajaxloaderq.gif")
                                                               )
                                                             ),
                                                             rpivotTableOutput("d3pivot2",width="50%")
                                                             )
                                                    ),
                                                    tabPanel("Total Cost to variability in Actual Coal UCLF",
                                                             tags$div(style="overflow-y: scroll;overflow-x: scroll;",
                                                             conditionalPanel(
                                                               condition = "output.d3pivot3==null ",                                    
                                                               div(class = "busy",
                                                                   p("Loading Pivot ..."),
                                                                   img(src="ajaxloaderq.gif")
                                                               )
                                                             ),
                                                             rpivotTableOutput("d3pivot3",width="50%")
                                                             )
                                                    ),
                                                    tabPanel("Compare New Capacity",
                                                             tags$div(style="overflow-y: scroll;overflow-x: scroll;",
                                                             conditionalPanel(
                                                               condition = "output.d3pivot4==null ",                                    
                                                               div(class = "busy",
                                                                   p("Loading Pivot ..."),
                                                                   img(src="ajaxloaderq.gif")
                                                               )
                                                             ),
                                                             rpivotTableOutput("d3pivot4",width="50%")
                                                             )
                                                    )
                                                    
                                        )
                        )
                        
                        
                        
             )  
     ),
     
     tabItem(tabName = "IDE", 
              sidebarPanel(
                fluidRow(
                  column(12,
                         conditionalPanel(
                           condition = "output.tree==null ",                                    
                           div(class = "busy",
                               p("Drawing Hierarchy..."),
                               img(src="ajaxloaderq.gif")
                           )
                         ),      
                         verbatimTextOutput("treesel"),
                         shinyTree("tree",search=T)
                  )
                ),
                fluidRow(
                  column(6,"Drill down and select a technology.")                                                                          
                )                                
                ,width=4
              ),
              mainPanel(
                fluidRow(
                  column(12,  
                         DT::dataTableOutput('x1')
                  )
                ),
                #fluidRow(
                #   column(12,
                #          showOutput("timeseries", "highcharts")
                #   )                                                     
                # ),
                fluidRow(
                  column(8,
                         showOutput("timeseries", "highcharts")
                  ),
                  column(4,
                         DT::dataTableOutput('x5')
                  )                                                     
                )
                ,width=8)
              
     ),
     tabItem(tabName="HELP",                        
              fluidRow(
                column(12,
                       
                       tabsetPanel(id="nav",  type="pills",   selected="STEP 1 - HELP",                        
                                   ########################### STEP0 ##########################
                                   tabPanel("STEP 1 - HELP",
                                            img(src="images/STEP1.png",width="100%"),
                                            img(src="images/STEP1-2.png",width="100%")
                                   ),
                                   tabPanel("STEP 2 - HELP",
                                            img(src="images/STEP2.png",width="100%")
                                   ),
                                   tabPanel("STEP 3 - HELP",
                                            img(src="images/STEP3.png",width="100%"),
                                            img(src="images/STEP3-2.png",width="100%")
                                   ),
                                   tabPanel("Input Data Explorer - HELP",
                                            img(src="images/IDE.png",width="100%")
                                   )
                       )
                )
              )
     )
     
     
     
     
   ) # Tab Items
             
 
   
   
 )
))