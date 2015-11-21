###### Author: Jacques Booysen #####
###### booysenjacques@gmail.com ####
#library(DiagrammeR) #devtools::install_github("booysej/DiagrammeR")
library(shiny)
library(xts)

#library(shinyAce)   #devtools::install_github("shinyAce", "trestletech")
library(shinyBS)
library(R.cache)
library(leaflet)    #devtools::install_github("booysej/leaflet")
library(shinyTree)  #devtools::install_github("booysej/shinyTree")
library(jsonlite)
library(DT);
library(data.table)
library(rCharts)
library(shinyGridster)
library(shinyIncubator);  #devtools::install_github("rstudio/shiny-incubator")
library(digest)
library(R.utils)
dir.exists = file.exists

library(rpivotTable);  # devtools::install_github("booysej/rpivotTable")  #install.packages("/home/jacques/shiny-server/apps/src/rpivotTable",repos=NULL,type="source")
tryCatch(library(cridfdata), error = function(e) e, finally = print(""))
#library(cridfdata); #devtools::install_github("booysej/cridfdata")
library(googleVis) #devtools::install_github("booysej/googleVis")
library(shinydashboard)

#       library(httr)
#       set_config(
#         use_proxy(url="proxy.enerweb.co.za", port=3128, username="booysej",password="rofdeer")
#       )
#       
#       options(RCurlOptions = list(proxy="proxy.enerweb.co.za:3128", proxyuserpwd="booysej:rofdeer"))
# DEBUG
# install.packages("/home/jacques/shiny-server/apps/src/googleVis",repos=NULL,type="source")
# install.packages("/home/jacques/shiny-server/apps/src/leaflet",repos=NULL,type="source")
# install.packages("/home/jacques/shiny-server/apps/src/rpivotTable",repos=NULL,type="source")
# install.packages("/home/jacques/shiny-server/apps/src/shinyTree",repos=NULL,type="source")
# install.packages("../src/DiagrammeR",repos=NULL,type="source")
# fileData <- reactiveFileReader(1000, session, 'data.csv', read.csv)
#addResourcePath('tiles', system.file('legacy/www/tiles', package='leaflet'))    



observerpool <- list();

shinyServer(function(input, output, session) {
  #addResourcePath('jquery', system.file('legacy/www/tiles', package='leaflet'))    
  
  step1components = list(MAP1="Map (GIS) - Transmission Flows, (Unconstraint) - click on country to filter",
                         #DV="Google Visualization - Data per Country (Map)", # BUGS
                         #DT="Google Visualization Timeseries by EnergyType", # BUGS
                         NEWCAPWA="New Capacity (2 Water Availability Scenarios)" ,
                         NEWCAPWACUM="Cumulative - New Capacity (2 Water Availability Scenarios)" ,
                         AVGPRICEDIFF="Average Price Difference",
                         FUELCONS="Fuel Cost vs Consumption (Contraint until 2020 with default design)",
                         COSTSENS="Cost vs Sensitivity (Contraint until 2020 with default design)",
                         AVGPRICEWA="Average Price vs Water Availability (Contraint until 2020 with default design)",
                         GENTS="Miscellaneous Timeseries",
                         CREATEPOL="Create New Policy Tool",
                         PIVOT="Pivot Table",
                         DEMO="Plot Demo",
                         DATAEXPORT="Export Data",
                         CO2VOLUME="CO2 volume chart"
  )
  
  #updateTabItems(session, "step", selected = "STEP1")  
  ############################
  if(file.exists("dashboardconfig.rdata")) {
    load("dashboardconfig.rdata");
  } else {
    dashboardconfig <- list(planner.R=list(),environmentalist.R = list(),econimist.R = list()); 
  }
  ############################
  
  ## New Id
  # max(unlist(lapply(dashboardconfig,function(x) {lapply(x,function(y) {y$id} )})))
  
  values <- reactiveValues(
                           dashboard="planner.R", 
                           user="default",
                           dashactions=NULL,
                           dashresume=NULL, 
                           olddashactions=date(), 
                           mapview="capacity",
                           startyear=2015,endyear=2025,selectedtech="",map1suspended=TRUE,
                           map2suspended=FALSE,selectavail="NONE",country="All",geojson=geojson, 
                           markercountry="All",selectedfeat=NULL,abehave="showonclick",lockedbasepolicy="NONE",lockedscenpolicy="NONE",
                           createdpolicy=list(list(name="NONE",thewater=0,thecoaluclf=0,
                                                   thetxuclf=0,varyload=TRUE,load=0,withoutinga=FALSE),
                                              list(name="Assume Low Water Availability",
                                                   thewater=100,thecoaluclf=100,thetxuclf=100,varyload=TRUE,load=100,withoutinga=FALSE),
                                              list(name="Assume High Water Availability",
                                                   thewater=120,thecoaluclf=100,thetxuclf=100,varyload=TRUE,load=100,withoutinga=FALSE)
                                              ),
                           availablepolicy=list(list(name="NONE",thewater=0,thecoaluclf=0,thetxuclf=0,varyload=TRUE,load=0,withoutinga=FALSE))
  )  
  
  observe({
    if(!is.null(input$dboard)) {
      if(input$dboard!="") {
        values$dashboard <- as.character(input$dboard);
      }
    }
  })
  
  
  ############################# DASHBOARD Magic #####################
  output$dashboard <- renderUI({
    #values$map1suspended = TRUE;
    #values$THREAD_DASHBOARD.id
    source(values$dashboard,local=TRUE)
    isolate({
      print("SETTING")
      values$dashactions <- date();
    });
    return(dashboard);
  })
  
  output$dashboarditems <- renderUI({
    thedash = as.character(values$dashboard)
    devent = values$dashactions
    print("Render Items")

    thev = dashboardconfig[[thedash]]
    
    # Kill all old Dashboard Observers
    
    try({lapply(observerpool,function(comp) { lapply(comp,function(x) { print("Kill"); x$suspend(); x$destroy(); })  })});
    
    try({unlink(paste("www/linksd1m1*.csv",sep=""))})
    
    observerpool <<- list();

    ############## Portlet View ######################
    portlet <- function(header,content, width=400, height=400,top=10,left=10,uuid="",id="") {
        tags$div(id=uuid,class=paste("portlet rescontainer",sep=""), "data-id"=id, 
                 # http://www.javascriptkit.com/dhtmltutors/customattributes.shtml
                 style=paste("font: 12px/1.3 sans-serif;
                          margin: 3px;
                          padding: 1px;
                          display: inline-block;
                          vertical-align: top;
                          height: ",height,"px;
                          width: ",width,"px;",sep=""),
                 tags$div(class="portlet-header",header),
                 tags$div(class="portlet-content",content)
               )
    }
    
    
      thec = lapply(
        thev,function(x) {
          component <<- x;
          componentfields <<- names(x);
          print(componentfields)
          ui = " ";
          if(file.exists(paste("components/",x$type,".R",sep=""))) {
            source(paste("component.R",sep=""),local=TRUE);
          }
          if(!is.na(x$width)) {width = x$width}
          if(!is.na(x$height)) {height = x$height}
          #if(!exists("height")) {height=400}
          #if(!exists("width")) {width=400}
          b = portlet(x$name,ui,width, height,x$top,x$left,uuid,x$id) 
          attr(b,"component") <- x;
          b
        })
      
      
      if(length(thec)>0) {
        dashboard = tags$div(class="sortable", thec,includeScript("portlets.js"))
      } else {
        dashboard = "Click on 'Manage Dashboard' on the left to Add Items."
      }    
   
     isolate({
       # Resume after render
       values$dashresume <- date();
     });
    
    return(dashboard);
  })

  # Resume after Render
   observe({
     dres = values$dashresume;
     isolate({ 
       threads = names(observerpool);
         lapply(threads,function(t) {
           obs = observerpool[[as.character(t)]]
           if(!is.null(obs)) {
             lapply(obs,function(x) { if(!is.null(x)) { x$resume(); }  })
           } else {
             print("Observer is NULL")
           }
       })
     
     });
   })
  
   observe({
     if(input$savedashboards1>0) {
       save(dashboardconfig,file="dashboardconfig.rdata")
       createAlert(session, "savedashboardalert", "savedash", title = "",
                   content = paste("Saved New Screen Layout!",sep=""), append = FALSE)
       Sys.sleep(1)
       closeAlert(session, "savedash")
     }
   })
  # Manage Dashboard
  output$mandash <-  DT::renderDataTable({
               input$dashboard_order;
               devent = values$dashactions;
               #thev = eval(parse(text=paste("dashboardconfig$",as.character(values$dashboard),sep="") ))
               thev = dashboardconfig[[as.character(values$dashboard)]]
               if(length(thev)>0) {
               indx <- sapply(thev, length)
               res <- as.data.frame(do.call(rbind,lapply(thev, `length<-`,max(indx))))
               colnames(res) <- names(thev[[which.max(indx)]])
               #save(res,file="/tmp/res.rdata")
               res = res[,c("name","width")]
               #res$open = ifelse(res$open,"Yes","No")
               names(res) = c("Existing Dashboard Item","Screen Width")
              
                     datatable(res,
                               rownames=FALSE,
                               selection="single",
                               options = list(pageLength = 5,class="cell-border")
                     );
               } else {
                 datatable(data.frame(Items="Empty"),
                           rownames=FALSE,
                           selection="single",
                           options = list(pageLength = 5)
                 );
               }
                     
  },server=FALSE)
  
  # Available Components
  output$addcompdash <- DT::renderDataTable({
      comp = as.data.frame(t(as.data.frame(step1components)))
    
      comp$ROW = rownames(comp)
      comp = comp[order(comp$V1),]
      comp$ROW =NULL
      
      names(comp) = c("New Dashboard Item")
      datatable(comp,
                rownames=FALSE,
                selection="single",
                options = list(pageLength = 10)
      );
  },server=FALSE);
    
  # Remove Dashboard Item
  observe({
    if(!is.null(input$remdashboards1)) {
      if(input$remdashboards1>0) {
        
        isolate({
        #thev = eval(parse(text=paste("dashboardconfig$",as.character(values$dashboard),sep="") ))
        thev = dashboardconfig[[as.character(values$dashboard)]]
        
        indx <- sapply(thev, length)
        res <- as.data.frame(do.call(rbind,lapply(thev, `length<-`,max(indx))))
        colnames(res) <- names(thev[[which.max(indx)]])
        
        if(!is.null(input$mandash_rows_selected)) {
           newres = res[-input$mandash_rows_selected,];
           newlist = apply(newres,1,as.list)
           names(newlist) = NULL;
           
           #eval(parse(text=paste("dashboardconfig$",as.character(values$dashboard)," <<- newlist;",sep="") ))
           dashboardconfig[[as.character(values$dashboard)]] <<- newlist;
           #toggleModal(session, "managedash", toggle = "close")
           values$dashactions <- date();
        }
        
      }) 
    }   
    }
    
  })
  
  output$compprop <- renderUI({
    selectInput("compwidth", "New Dashboard Item Screen Width %",
                c("33% (1/3 of Screen)" = 1,
                  "66% (2/3 of Screen)" = 2,
                  "100% (3/3 of Screen)" = 3)
    )
  })
  
  # Add Dashboard Item
  observe({
    if(!is.null(input$addcomponent)) {
      if(input$addcomponent>0) {
        print("Add Comp")
        isolate({
          
          #thev = eval(parse(text=paste("dashboardconfig$",as.character(values$dashboard),sep="") ))
          thev = dashboardconfig[[as.character(values$dashboard)]]
          if(length(thev)>0) {
            indx <- sapply(thev, length)
            res <- as.data.frame(do.call(rbind,lapply(thev, `length<-`,max(indx))))
            colnames(res) <- names(thev[[which.max(indx)]])
          } else {
            res = data.frame()
          }
          
          comp = as.data.frame(t(as.data.frame(step1components)))
          comp$ROW = rownames(comp)
          comp = comp[order(comp$V1),]
          comp$ROW =NULL
          
          comp$type = rownames(comp)
          names(comp) = c("name","type")
          if(do.call(sum,lapply(dashboardconfig,length))>0) {
            comp$id = max( as.numeric(as.vector(unlist(lapply(dashboardconfig,function(x) {lapply(x,function(y) {y$id} )})))) ) + 1
          } else {
            comp$id = 1;
          }
          comp$open = TRUE;
          comp$boardfile = as.character(values$dashboard)
          comp$chart = "Custom"
          comp$series = "Custom"
          comp$width = NA;
          comp$height = NA;
          comp$top = 0;
          comp$left = 0;
          
          if(!is.null(input$addcompdash_rows_selected)) {
            
            newrow = comp[input$addcompdash_rows_selected,]
            newres = rbind(res,newrow)
            newlist = apply(newres,1,as.list)
            names(newlist) = NULL;
            dashboardconfig[[as.character(values$dashboard)]] <<- newlist;
            
            #eval(parse(text=paste("dashboardconfig$",as.character(values$dashboard)," <<- newlist;",sep="") ))
            toggleModal(session, "addcomp", toggle = "close")
            #toggleModal(session, "managedash", toggle = "close")
            values$dashactions <- date();
          }
          
        }) 
      }   
    }
    
  })
  
  
  
  ####################################################################
  
  
  # Setup each Dashboard (customize open tabs etc.)
  observe({
    thetime = values$dashactions;
    isolate({
      if(!is.null(thetime)) {
        if(values$dashboard=="planner.R") {
          
        } else if(values$dashboard=="econimist.R") {
          
        } else if(values$dashboard=="environmentalist.R") {
          
          if(!is.null(input$mapview)) {
            #updateSelectInput(session, "mapview", selected = "price")
          }
        } else if(values$dashboard=="demo.R") {
          
        } else if(values$dashboard=="experimental.R") {
          
        }
      }
        
    })
  })
  
  
  ######## DASH Boards #############

  # Initial Map Step 3 setup
  observe({
    if(!is.null(input$story3)) {
      isolate({
        values$map2suspended = TRUE;
      });
      if("CHECK: Map View and Tx Energy Flows - click on country to select" %in% input$story3) {
  
      output$d3m1 <- renderLeaflet({
          leaflet(height=600) %>%
            addGeoJSON(geojson,layerId="main") %>%
            #addProviderTiles("OpenMapSurfer.Roads",options=tileOptions(minZoom = 4, maxZoom = 6))  %>% 
            addTiles(options=tileOptions(minZoom = 4, maxZoom = 6),attribution="Enerweb EOH")   %>% 
            setView(22.8731,-22.9992,5) %>% hideArrows(mapname="d3m1",behaviour="hideall");
        })  
      outputOptions(output, "d3m1", priority = 500)
  
  
      isolate({
        values$map2suspended = FALSE;
      });
  
      }}
  });
 
  # click on sea map ( show all)
  observe({
    if(!is.null(input$d3m1_click)) { 
      isolate({
      values$geojson$features <- isolate(lapply(values$geojson$features, function(feat) {              
        feat$properties$style <- list(fillOpacity = 0,color="green")
        values$selectedfeat = NULL;
        feat
      }))    
      proxy = leafletProxy("d3m1") # %>% removeGeoJSON(geojson[[2]]$properties$name)             
      proxy %>% addGeoJSON(isolate(values$geojson),layerId="main") 
      values$country <- 'All';
      });
    }
  },priority=1000)
  # click on country s3
  observe({
    theclick = NULL;        
    if(!is.null(input$d3m1_geojson_click)) {     
      theclick = input$d3m1_geojson_click      
    }
    
    isolate({
    if(!is.null(theclick)) {                 
      tcountry <- names(gistranslate[gistranslate==theclick$properties$COUNTRY])              
      if(!is.null(tcountry)) {   
        isolate({
          if(nchar(tcountry[1])>0) {
            gist = as.character(unlist(gistranslate[gsub(" $","",strsplit(tcountry,"\\(")[[1]][1])]))        
            if( (length(gist)>0) && (nchar(gist)>0) ) {    
              
              values$geojson$features <- isolate(lapply(values$geojson$features, function(feat) {              
                if (feat$properties$COUNTRY==gist) {
                  #print(gist)
                  feat$properties$style <- list(fillOpacity = 0.4,color="green")   
                  values$selectedfeat = feat;
                } else {
                  feat$properties$style <- list(fillOpacity = 0,color="green")
                }       
                feat
              }))              
            }
          }
        });      
      }      
      proxy = leafletProxy("d3m1") # %>% removeGeoJSON(geojson[[2]]$properties$name)             
      proxy %>% addGeoJSON(isolate(values$geojson),layerId="main")  # %>% addTiles( "tiles2/{z}/{x}/{y}.png")                   
      
      values$country <- tcountry
    }
    });
  },priority=1000)  

  
  
  showarrowsunconstraint <-function(thewater,thecoaluclf,thetxuclf,thecountry="All", theyear=2015,theseries="TransmissionOutput",
                        proxy=TRUE, mapname="", exclGI=FALSE,adjcons=FALSE,cons=100,mapview="capacity") {
    
    found = FALSE;
    td = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,cons/100)
    if(length(td[,1])==0) {return(NULL);}
    tfinal = subset(td, series == theseries)  
    units = as.character(tfinal$unit[1])
    if (thecountry!="All") {
      #tfinal = subset(tfinal, country.name == thecountry)          
    }
    
    tfinal = merge(tfinal,dtfrom,by="producing.country")
    tfinal= merge(tfinal,dtto,by="consuming.country") 
    
    txoutputfinal = subset(tfinal, time == theyear)          
    t = txoutputfinal[,c("source","target","value"),with=FALSE]
    t$text=paste("%flow ",units,sep="")
    setnames(t,names(t),c("source","target","flow","text"))
    t$flow = round(as.numeric(as.character(t$flow)))
    t = t[t$flow>0,]      
    t = t[, lapply(.SD, sum), by = c("source","target","text")]
    write.csv(t,file=paste("www/links",mapname,".csv",sep=""),row.names = FALSE)
    abe = "showall";
    if(proxy) {
     
      if(mapview=="capacity") {
        found = TRUE;
        td = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,TRUE,cons/100)
        
        perval = lapply(seq_along(dt$ID),function(i) {
          tfinal = subset(td, country.name == as.character(dt@data[i,]$COUNTRY))
          tfinal = subset(tfinal, series == "New Capacity")
          tfinal2 = tfinal[, c("time","value","energy.source"),with=F]
          tfinal3 = tfinal2[, lapply(.SD, sum), by = c("time","energy.source")]     
          tfinal3 = subset(tfinal3, time == theyear)
          
          tdat = as.data.frame(t(reshape(tfinal3,idvar=c("energy.source"),direction="wide")),stringsAsFactors=F)
          colnames(tdat) = as.character(unlist(tdat[1,]))
          tdat = tdat[-1,]
          rownames(tdat) = gsub("value\\.","",rownames(tdat))      
          
          x = as.data.frame(apply(tdat,2,as.numeric),stringsAsFactors=F)
          #rownames(x) = rownames(tdat)
          if (length(a)>0) {
            nn <<- rownames(x)
          } else {
            nn <<- NULL;
          }
          #x = x[order(rownames(x)),]
          
          ## HERE
          a = as.list(round(x[,1]))
          if (length(a)==0) {
            a = as.list(rep(0,9))
          }
          names(a)=nn
          return(a)
          #list(0,0,0,0,0,0,0)
          #as.character(dt@data[i,]$ID)
        })
        chart1Files = plotIcons2(perval,dt@data$ID,year=theyear, 70, 70,type="stacked")     
        
      } else if (mapview=="price") {
        found = TRUE;
        td =  getunconstraint(110/100, thecoaluclf/100,thetxuclf/100, exclGI,TRUE,100/100)
        td1 = getunconstraint(100/100, thecoaluclf/100,thetxuclf/100, exclGI,TRUE,100/100)
        td2 = getunconstraint(120/100, thecoaluclf/100,thetxuclf/100, exclGI,TRUE,100/100)
        
        perval = lapply(as.character(dt@data$COUNTRY),function(thec) {
          seriesname = "Avg Price"
          tfinal = subset(td, series == seriesname)
          tfinal1 = subset(td1, series == seriesname)  
          tfinal2 = subset(td2, series == seriesname)  
          units = "Difference in Average Price"
          
          tfinal = subset(tfinal, country.name == thec)          
          tfinal1 = subset(tfinal1, country.name == thec)
          tfinal2 = subset(tfinal2, country.name == thec)
          
          tfinal = subset(tfinal, time %in% (seq(values$startyear,values$endyear,1)))          
          tfinal1 = subset(tfinal1, time %in% (seq(values$startyear,values$endyear,1)))          
          tfinal2 = subset(tfinal2, time %in% (seq(values$startyear,values$endyear,1)))          
          
          if(nrow(tfinal)>0) {
            tfinala = tfinal[, c("time","value","country.name"),with=F]
            tfinalb = tfinala[, lapply(.SD, mean), by = c("country.name")]     # Mean of AVG Price
            tfinalb = tfinalb[(tfinalb$time>2010) & (tfinalb$time<2050) ,c("country.name","value"),with=F]
            
            tfinal1a = tfinal1[, c("time","value","country.name"),with=F]
            tfinal1b = tfinal1a[, lapply(.SD, mean), by = c("country.name")]     # Mean of AVG Price
            tfinal1b = tfinal1b[(tfinal1b$time>2010) & (tfinal1b$time<2050) ,c("country.name","value"),with=F]
            
            tfinal2a = tfinal2[, c("time","value","country.name"),with=F]
            tfinal2b = tfinal2a[, lapply(.SD, mean), by = c("country.name")]     # Mean of AVG Price
            tfinal2b = tfinal2b[(tfinal2b$time>2010) & (tfinal2b$time<2050) ,c("country.name","value"),with=F]
            
            a = list(list(name="10% less water (100%) compared to baseline (110%)", data=(tfinal1b$value - tfinalb$value)),
                     list(name="10% more water (120%) compared to baseline (110%)",data=(tfinal2b$value - tfinalb$value)) )
          } else {
            a =  list(list(name="10% less water than Baseline",data=NULL ),
                      list(name="10% more water than Baseline",data=NULL ))
          }
          return(a);
        });
        chart1Files = plotIcons2(perval,dt@data$ID,year=theyear, 100, 70,type="sidebar")             
      }

      if(found) {
        proxy2 = leafletProxy(mapname);     
        proxy2  %>% showArrows(mapname=mapname,
                             jsonfile="thedata.geojson",
                             linkfile=paste("links",mapname,".csv",sep=""),    
                             behaviour=abe,    
                             showid="")  %>% hideArrows(mapname=mapname,behaviour="hideall") %>% 
                  clearMarkers() %>%
                  addMarkers(options = markerOptions(clickable = TRUE, draggable = FALSE, 
                                    keyboard = TRUE, title = "Click for Detail", alt = "", 
                                    zIndexOffset = 0, opacity = 0.8, 
                                    riseOnHover = TRUE, riseOffset = 250),
                                    data = dt,
                                    layerId = dt@data$ID,
                                    icon = ~ icons(
                                              iconUrl = chart1Files[ID],
                                              iconAnchorX = 45, iconAnchorY = 55,
                                              popupAnchorX = 0, popupAnchorY = 0
                                    )
                            );
      }  
      
    }
  }  
  
  showarrowsconstraint <-function(designwater,designcoaluclf,designtxuclf,designexclGI,designcons,thewater,thecoaluclf,thetxuclf,exclGI,cons,
                                  thecountry="All", 
                                  theyear=2015,theseries="TransmissionOutput",
                                  proxy=TRUE, mapname="") {
     
    td = getconstraint(designwater/100,designcoaluclf/100,designtxuclf/100,designexclGI,designcons/100, 
                              thewater/100, thecoaluclf/100,thetxuclf/100,exclGI,cons/100)
    
    if(length(td)==0) {
        createAlert(session, "globalalert", "ga", title = "",
                    content = "No data found for selected combination", append = FALSE)
      return(NULL);
    } else {closeAlert(session, "ga")}
    
    
    tfinal = subset(td, series == theseries)  
    units = as.character(tfinal$unit[1])
    if (thecountry!="All") {
      #tfinal = subset(tfinal, country.name == thecountry)          
    }
    
    tfinal = merge(tfinal,dtfrom,by="producing.country")
    tfinal= merge(tfinal,dtto,by="consuming.country") 
    
    txoutputfinal = subset(tfinal, time == theyear)          
    t = txoutputfinal[,c("source","target","value"),with=FALSE]
    t$text=paste("%flow ",units,sep="")
    setnames(t,names(t),c("source","target","flow","text"))
    t$flow = round(as.numeric(as.character(t$flow)))
    t = t[t$flow>0,]      
    t = t[, lapply(.SD, sum), by = c("source","target","text")]
    write.csv(t,file=paste("www/links",mapname,".csv",sep=""),row.names = FALSE)
    abe = "showall";
    if(proxy) {
      proxy2 = leafletProxy(mapname);     
      proxy2  %>% showArrows(mapname=mapname,
                             jsonfile="thedata.geojson",
                             linkfile=paste("links",mapname,".csv",sep=""),    
                             behaviour=abe,    
                             showid="")  %>% hideArrows(mapname=mapname,behaviour="hideall");
    }
  }  
  

  

    
  # Update Arrows Step3
  observe({
    observe({    
    if( !is.null(values$lockedbasepolicy) && !is.null(values$lockedscenpolicy)) {    
      if( (values$lockedbasepolicy!='NONE') && (values$lockedscenpolicy!='NONE')) {    
        
        thewater = input$d3water    
        theuclf = input$d3uclf
        theuclf2 = input$d3uclf2
        thecountry = values$country
        theyear = input$d3year
        fixedyear = 2020 #input$d3fixedyear
        exclGI = input$d3withoutGrandInga
        cons = input$d3cons
        flowtype = input$d3txflowtype
        #thetime = values$dashactions;
        
        isolate({
          if(is.null(flowtype)) {
            flowtype = "baseline";
          }
          if(is.null(fixedyear)) {
            fixedyear = 2020
          }
          
          abe = "showall";
          if (!is.null(thewater) && !is.null(theuclf) && !is.null(thecountry) && !is.null(theyear) && !is.null(flowtype) ) {
            thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
            if(length(thepolicies)>1) {
              
              if(flowtype=="baseline") {
                r = values$availablepolicy[thepolicies==isolate(values$lockedbasepolicy)][[1]]
              } else if(flowtype=="scenario") {
                r = values$availablepolicy[thepolicies==isolate(values$lockedscenpolicy)][[1]]
              }
              designwater = unlist(r[2])
              designcoaluclf = unlist(r[3])
              designtxuclf = unlist(r[4])
              designexclGI = unlist(r[7])
              varyload=TRUE
              designcons = unlist(r[6])
              
             # print("HERE")
              
              showarrowsconstraint(designwater,designcoaluclf,designtxuclf,
                                   designexclGI,designcons,
                                   thewater,
                                   theuclf,
                                   theuclf2,exclGI,cons,
                                              thecountry, 
                                              theyear,"TransmissionOutput",
                                              TRUE, "d3m1")
              
              #showarrowsconstraint(designwater,designcoaluclf,designtxuclf,fixedyear,
              #                     thewater,theuclf,100,thecountry, theyear,"TransmissionOutput",TRUE,"d3m1",exclGI,varyload,load,100);
            }
          } 
          
        });

        
      }
    }    
  },suspended=values$map2suspended)
  });
  
  observe({
    values$startyear <- as.numeric(strsplit(as.character(input$daterange[1]),"-")[[1]][1])
    values$endyear <- as.numeric(strsplit(as.character(input$daterange[2]),"-")[[1]][1])
  });
  
  #output$queryText <- renderText({
  
  # URL demo param ?demo=1 etc.
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query$demo)) {
      if(query$demo==1) {
        updateTabsetPanel(session,"nav","STEP 1")
        values$country="Namibia"
        updateSliderInput(session, "daterange",value=c(2011,2050))
        
        updateCollapse(session,id="story",open="1.1) New Capacity (2 Water Availability Scenarios)")
        updateSliderInput(session, "d1water", value = 100)
        updateSliderInput(session, "d1uclf", value = 100)
        updateSliderInput(session, "d1uclf2", value = 100)
        updateCheckboxInput(session, "withoutGrandInga", value = FALSE)
        updateSliderInput(session, "d1cons", value = 100)
        
        return(query$demo)
      }
      if(query$demo==2) {
        updateTabsetPanel(session,"nav","STEP 1")
        values$country="All"
        updateCollapse(session,id="story",open="2.1) Average Price Difference")
        updateSliderInput(session, "d1water", value = 100)
        updateSliderInput(session, "d1uclf", value = 100)
        updateSliderInput(session, "d1uclf2", value = 100)
        updateCheckboxInput(session, "withoutGrandInga", value = FALSE)
        updateSliderInput(session, "d1cons", value = 100)
        
        return(query$demo)
      }
      if(query$demo==3) {
        
        updateTabsetPanel(session,"nav","STEP 1")
        values$country="All"
        updateCollapse(session,id="story",open="4.1) Fuel Cost vs Consumption (Contraint until 2020 with default design)")
        updateSliderInput(session, "d1water", value = 100)
        updateSliderInput(session, "d1uclf", value = 100)
        updateSliderInput(session, "d1uclf2", value = 100)
        updateCheckboxInput(session, "withoutGrandInga", value = FALSE)
        updateSliderInput(session, "d1cons", value = 100)
        
        return(query$demo)
      }
      if(query$demo==4) {
        
        updateTabsetPanel(session,"nav","STEP 1")
        values$country="All"
        updateCollapse(session,id="story",open="4.2) Cost vs Sensitivity (Contraint until 2020 with default design)")
        updateSliderInput(session, "d1water", value = 100)
        updateSliderInput(session, "d1uclf", value = 100)
        updateSliderInput(session, "d1uclf2", value = 100)
        updateCheckboxInput(session, "withoutGrandInga", value = FALSE)
        updateSliderInput(session, "d1cons", value = 100)
        
        return(query$demo)
      }
      if(query$demo==5) {
        
        updateTabsetPanel(session,"nav","STEP 1")
        values$country="All"
        updateCollapse(session,id="story",open="5.1) Average Price vs Water Availability (Contraint until 2020 with default design)")
        updateSliderInput(session, "d1water", value = 100)
        updateSliderInput(session, "d1uclf", value = 100)
        updateSliderInput(session, "d1uclf2", value = 100)
        updateCheckboxInput(session, "withoutGrandInga", value = FALSE)
        updateSliderInput(session, "d1cons", value = 100)
        
        return(query$demo)
      }
      if(query$demo==6) {
        values$lockedbasepolicy="Assume High Water Availability";
        values$lockedscenpolicy="Assume Low Water Availability";
        updateTabsetPanel(session,"nav","STEP 3")
        values$country="All"
        updateCollapse(session,id="story3",open="CHECK: Sensitivity",close="CHECK: Map View and Tx Energy Flows - click on country to select")
        updateSliderInput(session, "d1water", value = 100)
        updateSliderInput(session, "d1uclf", value = 100)
        updateSliderInput(session, "d1uclf2", value = 100)
        updateCheckboxInput(session, "withoutGrandInga", value = FALSE)
        updateSliderInput(session, "d1cons", value = 100)
        
        return(query$demo)
      }
      if(query$demo==7) {
        
        updateTabsetPanel(session,"nav","Input Data Explorer")
        return(query$demo)
      }
    } else {
      #updateTabsetPanel(session,"nav","STEP 1")
    #  updateCollapse(session,id="story",open="EVALUATE: Flows, Map View (Unconstraint) - click on country to filter")
    }
  })
  
  observe({
    print(input$d1t1LegendItemClick$name);
    
  })
  
  # EVALUATE: New Capacity (Unconstrained)
  output$d1t1 <- renderChart({      
    thewater = input$d1water    
    theuclf = input$d1uclf
    theuclf2 = input$d1uclf2
    thecountry = values$country
    thepolicy = "unconstraint"    
    exclGI = input$withoutGrandInga
    load = input$d1cons
    varyload=TRUE
    
      if (!is.null(thewater) & !is.null(theuclf) & !is.null(theuclf2) & !is.null(thecountry)   ) {
        return(barunconstraint(thewater,theuclf,theuclf2,thecountry, thedom="d1t1","Unconstraint","All",
                               values$startyear,values$endyear,exclGI,varyload,load));                
      }
  });
  

  infounconstraint <- function(thewater,thecoaluclf,thetxuclf,thecountry,theseries="Demand", thedom="d1t2a", exclGI=FALSE,adjcons=FALSE,cons=0) {
    
    if (!is.null(thewater) && !is.null(thecoaluclf) && !is.null(thetxuclf) ) {    
      td = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,cons/100)  
      
      tfinal = subset(td, series %in% theseries)  
      units = as.character(tfinal$unit[1])
      if (thecountry!="All") {
        tfinal = subset(tfinal, country.name == thecountry)          
      }
      
      #if(nrow(tfinal)>0) {        
      tfinal2 = tfinal[, c("time","value"),with=F]
      tfinal3 = tfinal2[, lapply(.SD, sum), by = c("time")]             
      tfinal3 = tfinal3[(tfinal3$time>2010) & (tfinal3$time<2050) ,]
      x = tfinal3;
      #}
      
      
      
      h1 <- rCharts:::Highcharts$new()
      h1$chart(type = "spline",marginLeft=80,height=300)
      h1$title(text = paste(paste(theseries,collapse=",") ," (",thecountry[1],")",sep="") )
      
      #if(nrow(tfinal)>0) {
      h1$xAxis(categories = x$time,labels=list(enabled=TRUE))
      h1$yAxis(title = list(text = units),min=0)        
      h1$series( data = x$value, type="spline", name=paste(theseries,sep="")  )
      
      #}
      
      h1$legend(symbolWidth = 80,enabled=FALSE)
      h1$set(dom = thedom)
      h1$plotOptions(animation=FALSE,spline=list(animation=FALSE))
      h1$exporting(enabled = F)          
      #print(h1)
      return(h1)         
      
    }        
  }
  
 
  
  output$s2info <- renderUI({
    thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
    if(length(thepolicies)>1) {
      
      
      if(!is.null(input$availablepolicies)) {
      r = values$availablepolicy[thepolicies==input$availablepolicies][[1]]
      
      tags$table(border=1,spacing=1,
                 tags$tr(
                   tags$th("Assumption"),
                   tags$th("Value")
                 ),
                 tags$tr(
                   tags$td("Water Availability"),
                   tags$td(r[2])
                 ),
                 tags$tr(
                   tags$td("Coal UCLF"),
                   tags$td(r[3])
                 ),  
                 tags$tr(
                   tags$td("Transmission UCLF"),
                   tags$td(r[4])
                 ),  
                 tags$tr(
                   tags$td("Include Grand Inga"),
                   tags$td( ifelse(r[7],"No","Yes") )
                 ),  
                 tags$tr(
                   tags$td("Adjust Consumption"),
                   tags$td( ifelse(r[5],"Yes","No") )
                 ),  
                 tags$tr(
                   tags$td("Consumption"),
                   tags$td( r[6] )
                 )
      )
      }
    }
  })
  
  
  
  output$availlistui <- renderUI({
    thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
    if(length(thepolicies)>1) {
      thepolicies = thepolicies[thepolicies!="NONE"]  
    }
    
    selectInput("availablepolicies","AVAILABLE Policy Assumptions: (SELECT)",as.list(thepolicies),
                size=5,selectize=FALSE)#selected=values$selectavail)
  })    
  output$lockedlistui <- renderUI({
    tags$span(
      selectInput("lockedbaseline","LOCKED Baseline Policy:",as.list(values$lockedbasepolicy),size=1,selectize=FALSE),
      selectInput("lockedscenario","LOCKED Scenario Policy:",as.list(values$lockedscenpolicy),size=1,selectize=FALSE)
    )
  })
  
  output$d3m1type <- renderUI({
    
    a = list("Baseline" = "baseline",
             "Scenario" = "scenario")    
    names(a) = c(paste("Baseline Design: ",values$lockedbasepolicy,sep=""), paste("Scenario Design: ",values$lockedscenpolicy,sep=""))
    
    radioButtons("d3txflowtype", "Scenario vs Baseline",a)    
  })
  
  output$d2t1 <- renderChart({
    if(!is.null(input$availablepolicies)) {
      if(!is.null(values$availablepolicy)) {
        if(length(values$availablepolicy)>0) {  
        
      if(input$availablepolicies!="NONE") {
      
    thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
    if(length(thepolicies)>1) {
      print("There")
      r = values$availablepolicy[thepolicies==input$availablepolicies][[1]]
      
      thewater = unlist(r[2])
      theuclf = unlist(r[3])
      theuclf2 = unlist(r[4])
      thecountry = values$country
      exclGI = unlist(r[7])
      varyload=TRUE
      load = unlist(r[6])
      if (!is.null(thewater) && !is.null(theuclf) && !is.null(theuclf2)  ) {
        return(barunconstraint(thewater,theuclf,theuclf2,thecountry, thedom="d2t1",r[1],"All",
                             values$startyear,values$endyear,exclGI,varyload,load));                
      }
    }
    } else {
     
      h1 <- rCharts:::Highcharts$new()
       h1$chart(type = "column",marginLeft=50,height=300)
       h1$title(text = paste("New Capacity",sep=""))
       h1$subtitle(text = paste("",sep=""))
     
       h1$legend(symbolWidth = 10)
       h1$set(dom = "d2t1")
       h1$plotOptions(animation=FALSE,column=list(stacking= 'normal',animation=FALSE))
       h1$exporting(enabled = T)    
      
      return(h1)  
    }
        }
      }
    } else {
      h1 <- rCharts:::Highcharts$new()
      h1$chart(type = "column",marginLeft=50,height=300)
      h1$title(text = paste("New Capacity",sep=""))
      h1$subtitle(text = paste("",sep=""))
      
      h1$legend(symbolWidth = 10)
      h1$set(dom = "d2t1")
      h1$plotOptions(animation=FALSE,column=list(stacking= 'normal',animation=FALSE))
      h1$exporting(enabled = T)    
      
      return(h1)  
    }
  });
    

  
  datasetInputUC <- reactive({
    thewater = input$d1water    
    thecoaluclf = input$d1uclf
    thetxuclf = input$d1uclf2
    exclGI = input$withoutGrandInga
    cons = input$d1cons
    td = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,TRUE,cons/100)
    td
  })
  
  datasetInputC <- reactive({
    
    if( !is.null(values$lockedbasepolicy) && !is.null(values$lockedscenpolicy)) {    
      if( (values$lockedbasepolicy!='NONE') && (values$lockedscenpolicy!='NONE')) {    
        
        thewater = input$d3water    
        theuclf = input$d3uclf
        theuclf2 = input$d3uclf2
        thecountry = values$country
        theyear = input$d3year
        fixedyear = 2020 
        exclGI = input$d3withoutGrandInga
        cons = input$d3cons
        flowtype = input$d3txflowtype
        
        
        isolate({
          if(is.null(flowtype)) {
            flowtype = "baseline";
          }
          if(is.null(fixedyear)) {
            fixedyear = 2020
          }
          
          if (!is.null(thewater) && !is.null(theuclf) && !is.null(thecountry) && !is.null(theyear) && !is.null(flowtype) ) {
            thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
            if(length(thepolicies)>1) {
              
              
              r = values$availablepolicy[thepolicies==isolate(values$lockedbasepolicy)][[1]]
              designwater = unlist(r[2])
              designcoaluclf = unlist(r[3])
              designtxuclf = unlist(r[4])
              designexclGI = unlist(r[7])
              varyload=TRUE
              designcons = unlist(r[6])
              td = getconstraint(designwater/100,designcoaluclf/100,designtxuclf/100,designexclGI,designcons/100, 
                       thewater/100, theuclf/100,theuclf2/100,exclGI,cons/100)
              td$ref = "BASELINE";
              td$refname = values$lockedbasepolicy;
              
              r = values$availablepolicy[thepolicies==isolate(values$lockedscenpolicy)][[1]]
              designwater = unlist(r[2])
              designcoaluclf = unlist(r[3])
              designtxuclf = unlist(r[4])
              designexclGI = unlist(r[7])
              varyload=TRUE
              designcons = unlist(r[6])
              td2 = getconstraint(designwater/100,designcoaluclf/100,designtxuclf/100,designexclGI,designcons/100, 
                                 thewater/100, theuclf/100,theuclf2/100,exclGI,cons/100)
              td2$ref = "SCENARIO"
              td2$refname = values$lockedscenpolicy;
              return(as.data.table(rbind(td,td2)))
              
            }
          }
           
        });
        
      }
    }
        
  })
  
  output$s1downloadpolicy <- downloadHandler(
    filename = function() {
      paste("UnconstraintSample", "csv", sep = ".")
    },
    content = function(file) {
          write.table(datasetInputUC(), file, sep = ",",row.names = FALSE)
    }
  )
  
  output$s3downloadpolicy <- downloadHandler(
    filename = function() {
      paste("ConstraintSample", "csv", sep = ".")
    },
    content = function(file) {
      write.table(datasetInputC(), file, sep = ",",row.names = FALSE)
    }
  )
  
  
  
  # LOCK base
  observe({
    if(!is.null(input$plockbase)) {
    if(input$plockbase>0) {
      if (!is.null(isolate(input$availablepolicies))) {
        if(isolate(input$lockedbaseline=='NONE')) {
          isolate({
           closeAlert(session, "s2a")
            if(isolate(values$lockedscenpolicy)!=isolate(input$availablepolicies)) {
                values$lockedbasepolicy <- isolate(input$availablepolicies);
            }
            thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
            if(length(thepolicies)>=1) {
              #values$availablepolicy  = values$availablepolicy[thepolicies!=as.character(input$availablepolicies)]  
              #values$selectavail = values$availablepolicy[thepolicies!=as.character(input$availablepolicies)][[1]]$name
            }
          });
          
        }
      }
    }      
    }
  });
  # LOCK scen
  observe({
    if(!is.null(input$plockscen)) {
    if(input$plockscen>0) {
      if (!is.null(isolate(input$availablepolicies))) {
        if(isolate(input$lockedscenario=='NONE')) {
          isolate({
            closeAlert(session, "s2a")
            if(isolate(values$lockedbasepolicy)!=isolate(input$availablepolicies)) {
              values$lockedscenpolicy <- isolate(input$availablepolicies);
            }
             thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
             if(length(thepolicies)>=1) {
               #values$availablepolicy  = values$availablepolicy[thepolicies!=as.character(input$availablepolicies)]  
               #values$selectavail = values$availablepolicy[thepolicies!=as.character(input$availablepolicies)][[1]]$name
               #values$selectavail = values$availablepolicy[thepolicies!=as.character(input$availablepolicies)][1]
             }
          });
          
        }
      }
    }  }    
  });  
  # UNLOCK base
  observe({
    if(!is.null(input$punlockbase)) {      
    if(input$punlockbase>0) {      
      if(isolate(input$lockedbaseline!='NONE')) {
        isolate({
        closeAlert(session, "s2a")
        values$lockedbasepolicy <- 'NONE';
        values$availablepolicy <- isolate(values$createdpolicy) 
        thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
        #values$availablepolicy  = values$availablepolicy[thepolicies!=as.character(input$lockedscenario)]  
        });
      }
    }}      
  });
  # UNLOCK scen
  observe({
    if(!is.null(input$punlockscen)) {      
    if(input$punlockscen>0) {      
      if(isolate(input$lockedscenario!='NONE')) {
        isolate({
        closeAlert(session, "s2a")
        values$lockedscenpolicy <- 'NONE';
        values$availablepolicy <- isolate(values$createdpolicy)
        thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
        #values$availablepolicy  = values$availablepolicy[thepolicies!=as.character(input$lockedbaseline)]    
        });
      }
    }}      
  });  

  output$d2pivot <-renderRpivotTable({  
    
    thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
    if(length(thepolicies)>1) {
      r = values$availablepolicy[thepolicies==input$availablepolicies][[1]]
      
      thewater = unlist(r[2])
      theuclf = unlist(r[3])
      theuclf2 = unlist(r[4])
      thecountry = values$country
      exclGI = unlist(r[7])
      varyload=TRUE
      load = unlist(r[6])
      
      td = getunconstraint(thewater/100, theuclf/100,theuclf2/100, exclGI,varyload,load/100)  
      tfinal = subset(td, series == input$d2pivotts)  
      units = as.character(tfinal$unit[1])
      if (thecountry!="All") {
        tfinal = subset(tfinal, country.name == thecountry)          
      }
      fs = subset(tfinal, time %in% c(values$startyear:values$endyear))     
      if(nrow(fs)>0) {
        rpivotTable(data=fs,rows=c("series","energy\\.source"),cols=c("time"),aggregatorName="Sum",rendererName="Stacked Bar Chart",vals="value"
                    ,hiddenAttributes=c("consuming\\.country","producing\\.country","policy_name","policy_id","run_id",
                                        "design\\.coal\\.uclf",
                                        "design\\.fix\\.year",
                                        "design\\.consumption\\.adjustment",
                                        "design\\.grand\\.inga\\.out",
                                        "design\\.transmission\\.uclf",
                                        "design\\.water\\.availability",
                                        "coal\\.uclf",
                                        "fix\\.year",
                                        "consumption\\.adjustment",
                                        "grand\\.inga\\.out",
                                        "transmission\\.uclf",
                                        "water\\.availability"
                                        ))    
      }
    }

  })

  observe({    
    if( !is.null(input$lockedbaseline) || !is.null(input$lockedbaseline)) {    
      if( (input$lockedbaseline=='NONE') || (input$lockedbaseline=='NONE')) {
        createAlert(session, "s3alert", "s3a", title = "Step2: Error",
                    content = "Please Lock>> both a BASELINE and SCENARIO policy before proceeding to Step3!", append = FALSE)
      } else {
        closeAlert(session, "s3a")
      }    
    }
  })
  
  GetBaseScen <- function(seriesnames,
                          water, coaluclf,txuclf,exclGI,cons,
                          fixwater=TRUE,fixcoaluclf=TRUE,fixtxuclf=TRUE,fixexclGI=TRUE,fixcons=TRUE) {
    
    thepolicies = do.call(c,lapply(values$availablepolicy,function(x) {x$name}))
    if(length(thepolicies)>1) {
      
      thecountry = values$country

     
      ## Get BASELINE
      r = values$availablepolicy[thepolicies==isolate(values$lockedbasepolicy)][[1]]
      designwater = unlist(r[2])
      designcoaluclf = unlist(r[3])
      designtxuclf = unlist(r[4])
      designexclGI = unlist(r[7])
      varyload=TRUE
      designcons = unlist(r[6])
      
      
      td = getconstraint(designwater/100,designcoaluclf/100,designtxuclf/100,designexclGI,designcons/100, 
                                water/100, coaluclf/100,txuclf/100,exclGI,cons/100,
                                fixwater,fixcoaluclf,fixtxuclf,fixexclGI,fixcons)
      
      #td = getconstraint(designwater/100,designcoaluclf/100,designtxuclf/100,fixyear,thewater/100, thecoaluclf/100,thetxuclf/100, 
      #                   exclGI,varyload,load/100,cons/100,fixwater,fixcoaluclf,fixtxuclf,fixcons)
      
      if(length(td)==0) {
        createAlert(session, "globalalert", "ga", title = "",
                    content = "No data found for selected combination", append = FALSE)
        return(NULL);
      } else {closeAlert(session, "ga")}
      
      #td = getunconstraint(thewater/100, theuclf/100,theuclf2/100, exclGI,varyload,load/100)  
      tfinal = subset(td, series %in% seriesnames)  
      units = as.character(tfinal$unit[1])
      if (thecountry!="All") {
        tfinal = subset(tfinal, country.name == thecountry)          
      }
      fs = subset(tfinal, time %in% c(values$startyear:values$endyear))
      fs$resulttype = "Baseline" 
      fs$lockedname = values$lockedbasepolicy
      
      # Get SCENARIO
      r = values$availablepolicy[thepolicies==isolate(values$lockedscenpolicy)][[1]]
      designwater = unlist(r[2])
      designcoaluclf = unlist(r[3])
      designtxuclf = unlist(r[4])
      designexclGI = unlist(r[7])
      varyload=TRUE
      designcons = unlist(r[6])

      
      td = getconstraint(designwater/100,designcoaluclf/100,designtxuclf/100,designexclGI,designcons/100, 
                         water/100, coaluclf/100,txuclf/100,exclGI,cons/100,
                         fixwater,fixcoaluclf,fixtxuclf,fixexclGI,fixcons)
      
      #td = getconstraint(designwater/100,designcoaluclf/100,designtxuclf/100,fixyear,thewater/100, thecoaluclf/100,thetxuclf/100, 
      #                   exclGI,varyload,load/100,cons/100,fixwater,fixcoaluclf,fixtxuclf,fixcons)
      
      if(length(td)==0) {
        createAlert(session, "globalalert", "ga", title = "",
                    content = "No data found for selected combination", append = FALSE)
        return(NULL);
      } else {closeAlert(session, "ga")}
      
      #td = getunconstraint(thewater/100, theuclf/100,theuclf2/100, exclGI,varyload,load/100)  
      tfinal = subset(td, series %in% seriesnames)  
      units = as.character(tfinal$unit[1])
      if (thecountry!="All") {
        tfinal = subset(tfinal, country.name == thecountry)          
      }
      fs2 = subset(tfinal, time %in% c(values$startyear:values$endyear))
      fs2$resulttype = "Scenario"
      fs2$lockedname = values$lockedscenpolicy
      
      fs3 = rbind(fs,fs2,fill=TRUE)
      return(fs3)
    }
  }
 
  
  output$d3pivot1 <-renderRpivotTable({ 
      
      water = input$d3water    
      coaluclf = input$d3uclf
      txuclf = input$d3uclf2
      thecountry = values$country
      fixedyear = 2020 
      exclGI = input$d3withoutGrandInga
      cons = input$d3cons
      
      if (!is.null(water) && !is.null(coaluclf) && !is.null(txuclf) && !is.null(thecountry) && !is.null(cons) && !is.null(exclGI) ) {
      
        withProgress(message = 'Loading Pivot',
                     detail = '', value = 20, {
                       
              
      
      fs3 = GetBaseScen(c("Export revenue","Import cost","TransmissionInvestment","Investment",
                          "Annual Investment","Fuel Cost","O&M Costs"),
                              water, coaluclf,txuclf,exclGI,cons,
                              fixwater=FALSE,fixcoaluclf=TRUE,fixtxuclf=TRUE,fixexclGI=TRUE,fixcons=TRUE);
      
  
      if(nrow(fs3)>0) {
        rpivotTable(data=fs3,rows=c("resulttype","coal\\.uclf"),cols=c("water\\.availability"),aggregatorName="Sum",rendererName="Line Chart",vals="value"
                    ,hiddenAttributes=c("consuming\\.country","producing\\.country","policy_name","policy_id","run_id",
                                        "design\\.coal\\.uclf",
                                        "design\\.fix\\.year",
                                        "design\\.consumption\\.adjustment",
                                        "design\\.grand\\.inga\\.out",
                                        "design\\.transmission\\.uclf",
                                        "design\\.water\\.availability",
                                        #"coal\\.uclf",
                                        "fix\\.year"
                                        #"consumption\\.adjustment"
                                        #"grand\\.inga\\.out",
                                        #"transmission\\.uclf",
                                        #"water\\.availability"
                    ))    
      
      }
      
                     })
      }
  })

  output$d3pivot2 <-renderRpivotTable({ 
    
    water = input$d3water    
    coaluclf = input$d3uclf
    txuclf = input$d3uclf2
    thecountry = values$country
    fixedyear = 2020 
    exclGI = input$d3withoutGrandInga
    cons = input$d3cons
    
    if (!is.null(water) && !is.null(coaluclf) && !is.null(txuclf) && !is.null(thecountry) && !is.null(cons) && !is.null(exclGI) ) {
    
    withProgress(message = 'Loading Pivot',
                 detail = '', value = 20, {
    
    
      fs3 = GetBaseScen(c("Fuel Cost"),
                              water, coaluclf,txuclf,exclGI,cons,
                              fixwater=FALSE,fixcoaluclf=TRUE,fixtxuclf=TRUE,fixexclGI=TRUE,fixcons=TRUE);
    
    if(nrow(fs3)>0) {
      rpivotTable(data=fs3,rows=c("coal\\.uclf","energy\\.source"),cols=c("resulttype","water\\.availability"),aggregatorName="Sum",rendererName="Table Barchart",vals="value"
                  ,hiddenAttributes=c("consuming\\.country","producing\\.country","policy_name","policy_id","run_id",
                                      "design\\.coal\\.uclf",
                                      "design\\.fix\\.year",
                                      "design\\.consumption\\.adjustment",
                                      "design\\.grand\\.inga\\.out",
                                      "design\\.transmission\\.uclf",
                                      "design\\.water\\.availability",
                                      #"coal\\.uclf",
                                      "fix\\.year"
                                      #"consumption\\.adjustment"
                                      #"grand\\.inga\\.out",
                                      #"transmission\\.uclf",
                                      #"water\\.availability"
                  ))    
      
    }
                 });
    }
  })
  
  output$d3pivot3 <-renderRpivotTable({ 
    
    water = input$d3water    
    coaluclf = input$d3uclf
    txuclf = input$d3uclf2
    thecountry = values$country
    fixedyear = 2020 
    exclGI = input$d3withoutGrandInga
    cons = input$d3cons
    
    if (!is.null(water) && !is.null(coaluclf) && !is.null(txuclf) && !is.null(thecountry) && !is.null(cons) && !is.null(exclGI) ) {
    
    withProgress(message = 'Loading Pivot',
                 detail = '', value = 20, {
    
    if(!is.null(water)) {
      
      
      fs3 = GetBaseScen(c("Export revenue","Import cost","TransmissionInvestment","Investment",
                          "Annual Investment","Fuel Cost","O&M Costs"),
                        water, coaluclf,txuclf,exclGI,cons,
                        fixwater=TRUE,fixcoaluclf=FALSE,fixtxuclf=TRUE,fixexclGI=TRUE,fixcons=TRUE);
      
      
      if(nrow(fs3)>0) {
        rpivotTable(data=fs3,rows=c("resulttype","water\\.availability"),cols=c("coal\\.uclf"),aggregatorName="Sum",rendererName="Line Chart",vals="value"
                    ,hiddenAttributes=c("consuming\\.country","producing\\.country","policy_name","policy_id","run_id",
                                        "design\\.coal\\.uclf",
                                        "design\\.fix\\.year",
                                        "design\\.consumption\\.adjustment",
                                        "design\\.grand\\.inga\\.out",
                                        "design\\.transmission\\.uclf",
                                        "design\\.water\\.availability",
                                        #"coal\\.uclf",
                                        "fix\\.year"
                                        #"consumption\\.adjustment"
                                        #"grand\\.inga\\.out",
                                        #"transmission\\.uclf",
                                        #"water\\.availability"
                    ))    
        
      }
    }
                 });
    }
  })

  output$d3pivot4 <-renderRpivotTable({ 
    
    water = input$d3water    
    coaluclf = input$d3uclf
    txuclf = input$d3uclf2
    thecountry = values$country
    fixedyear = 2020 
    exclGI = input$d3withoutGrandInga
    cons = input$d3cons
    
    if (!is.null(water) && !is.null(coaluclf) && !is.null(txuclf) && !is.null(thecountry) && !is.null(cons) && !is.null(exclGI) ) {
      
    
    withProgress(message = 'Loading Pivot',
                 detail = '', value = 20, {
    
    
    if(!is.null(water)) {
      
      
      fs3 = GetBaseScen(c("New Capacity"),
                        water, coaluclf,txuclf,exclGI,cons,
                        fixwater=TRUE,fixcoaluclf=TRUE,fixtxuclf=TRUE,fixexclGI=TRUE,fixcons=TRUE);
      
      
      if(nrow(fs3)>0) {
        rpivotTable(data=fs3,rows=c("energy\\.source"),cols=c("time","resulttype"),aggregatorName="Sum",rendererName="Stacked Bar Chart",vals="value"
                    ,hiddenAttributes=c("consuming\\.country","producing\\.country","policy_name","policy_id","run_id",
                                        "design\\.coal\\.uclf",
                                        "design\\.fix\\.year",
                                        "design\\.consumption\\.adjustment",
                                        "design\\.grand\\.inga\\.out",
                                        "design\\.transmission\\.uclf",
                                        "design\\.water\\.availability",
                                        #"coal\\.uclf",
                                        "fix\\.year"
                                        #"consumption\\.adjustment"
                                        #"grand\\.inga\\.out",
                                        #"transmission\\.uclf",
                                        #"water\\.availability"
                    ))    
        
      }
    }
                 });
      
    }
  })
  
  output$d3pivot5 <-renderRpivotTable({ 
    
    water = input$d3water    
    coaluclf = input$d3uclf
    txuclf = input$d3uclf2
    thecountry = values$country
    fixedyear = 2020 
    exclGI = input$d3withoutGrandInga
    cons = input$d3cons
    
    if (!is.null(water) && !is.null(coaluclf) && !is.null(txuclf) && !is.null(thecountry) && !is.null(cons) && !is.null(exclGI) ) {
      
      
      withProgress(message = 'Loading Pivot',
                   detail = '', value = 20, {
                     
                     
                     if(!is.null(water)) {
                       
                       
                       fs3 = GetBaseScen(c("Avg Price"),
                                         water, coaluclf,txuclf,exclGI,cons,
                                         fixwater=TRUE,fixcoaluclf=TRUE,fixtxuclf=TRUE,fixexclGI=TRUE,fixcons=TRUE);
                       
                       
                       if(nrow(fs3)>0) {
                         rpivotTable(data=fs3,rows=c(""),cols=c("time","resulttype"),aggregatorName="Average",rendererName="Stacked Bar Chart",vals="value"
                                     ,hiddenAttributes=c("consuming\\.country","producing\\.country","policy_name","policy_id","run_id",
                                                         "design\\.coal\\.uclf",
                                                         "design\\.fix\\.year",
                                                         "design\\.consumption\\.adjustment",
                                                         "design\\.grand\\.inga\\.out",
                                                         "design\\.transmission\\.uclf",
                                                         "design\\.water\\.availability",
                                                         #"coal\\.uclf",
                                                         "fix\\.year"
                                                         #"consumption\\.adjustment"
                                                         #"grand\\.inga\\.out",
                                                         #"transmission\\.uclf",
                                                         #"water\\.availability"
                                     ))    
                         
                       }
                     }
                   });
      
    }
  })
  
    

  

  
  # Draw: Energy chain TREE (Hierarchy)
  output$tree <- renderTree({
    
    #- Country
    #- Level
    #- Energy. Source
    #- Status
    #- Name
    
    withProgress(message = 'Loading hierarchy....',
                 detail = 'Please wait', value = 0, {
              
                   tree <- lapply(countries, function(country){
                     level.list <- lapply(level[c(4,1,3,5,2)], function(alevel){
                       energy.sources <- uniqueAndSorted(subset(nodes,country.name == country & level==alevel)$energy.source)
                       energy.sources.list <- lapply(energy.sources, function(asource){
                         status <- uniqueAndSorted(subset(nodes,country.name == country & level==alevel & energy.source == asource)$status)
                         status.list <- lapply(status, function(astatus){
                           tech <- uniqueAndSorted(subset(nodes,country.name == country & level==alevel & energy.source == asource & status == astatus)$name)        
                           #tech <- uniqueAndSorted(subset(nodes,country.name == country & level==alevel & energy.source == asource & status == astatus)$technology)        
                           lnames <- as.list(tech)
                           names(lnames) <- tech
                           return(lnames)
                         })
                         names(status.list) <- status
                         return(status.list)
                       })
                       names(energy.sources.list) <- energy.sources
                       return(energy.sources.list)
                     })
                     names(level.list) <- level[c(4,1,3,5,2)]
                     return(level.list)
                   })
                   names(tree) <- countries

                 });
    
    return(tree)
  })
  output$treesel <- renderText({})
  
  output$x1 <- DT::renderDataTable({
    if(values$selectedtech!="") {
    
    withProgress(message = 'Loading data....',
                 detail = 'Please wait...', value = 0, {
                   st = values$selectedtech
                   stid = ref_objects[ref_objects$technology==st,]$id
                   stn = ref_objects[ref_objects$technology==st,]$name
                   si = idedata[idedata$technology_id %in% stid,]$series_id
                   
                   datatable(ref_series[ref_series$id %in% si,c("id","series","applicable.level","unit")],
                             rownames=FALSE,
                             #selection = list(mode = 'single', selected = c(1)),   
                             selection="single",
                             #selection = list(mode = 'single', selected = c("1")),
                             options = list(pageLength = 5),
                             #caption=
                             #   paste("Select Properties below to view Timeseries Data for selected Technology:   ",          
                             #         paste(nv[4],nv[5],nv[6],paste(nv[1]," (",nv[2],")",sep=""),sep=" -> "),sep="")
                             # 
                             caption=
                               paste("Select Properties for: ", st," (", stn,")",sep="")
                             #
                   );
                   
                 })
    
    }
  },server=FALSE)
  
  outputOptions(output, "x1", priority = 1000)
  
  output$x5 = DT::renderDataTable({  
    if(values$selectedtech!="") {
      st = values$selectedtech
      
      stid = ref_objects[ref_objects$technology==st,]$id
      stn = ref_objects[ref_objects$technology==st,]$name
      si = unique(idedata[idedata$technology_id %in% stid,]$series_id)
    
      b = ref_series[ref_series$id %in% si,]
      
      isel = input$x1_rows_selected
      
      if(!is.null(isel)) {
       b = b[isel,]
       print(isel)
       
       withProgress(message = 'Loading data....',
                   detail = 'Please wait...', value = 0, { 
                     
                            
                     
                     a = datatable(idedata[(idedata$technology_id==stid & idedata$series_id==b$id) ,c(3:4)],
                                   rownames=FALSE,
                                   selection="single",
                                   options = list(pageLength = 6),
                                   #caption=htmltools::tags$b(paste("Data for: ",zname2,sep=""))
                                   caption= paste("Data for: ",b$series,"",sep="")
                     )    
                     
                     
                   });
       return(a);      
      }
    }
  })
  
  
  
  output$timeseries <- renderChart({
    isel = input$x1_rows_selected
    
    if(values$selectedtech!="" & !is.null(isel) ) {
      st = values$selectedtech
      stid = ref_objects[ref_objects$technology==st,]$id
      stn = ref_objects[ref_objects$technology==st,]$name
      stc = ref_objects[ref_objects$technology==st,]$country.name
      si = idedata[idedata$technology_id %in% stid,]$series_id
      
      b = ref_series[ref_series$id %in% si,]
      isel = input$x1_rows_selected
      b = b[isel,]
    
      td = idedata[(idedata$technology_id==stid & idedata$series_id==b$id) ,c(3:4)]
      
      x = td$time
      y = as.numeric(as.character(td$value))
      
      h1 <- Highcharts$new()
      #h1$chart(type = "spline",zoomType="x",height=300,width=600)
      h1$chart(type = "spline",zoomType="x")
      h1$yAxis(title = list(text="Value"),min=0)
      h1$xAxis(title = list(text="Year"), categories = x,
               tickmarkPlacement="on")
      
      h1$title(text=paste(stn ,sep=""))
      h1$subtitle(text=paste(stc ,sep=""))    
      
      h1$series( data = y, type="area", name=paste(" ",b$series ,sep="")  )
      
    
      
      
      h1$legend(symbolWidth = 80)
      h1$set(dom = 'timeseries')
      h1$plotOptions(animation=FALSE,
                     area=list(
                       stacking="normal",
                       lineColor="#666666",
                       lineWidth=1,
                       marker = list(
                         lineWidth=1,
                         lineColor="#666666"
                       )
                     ));
      h1$exporting(enabled = T)    
      return(h1)    
    } else {
      #updateCollapse(session, id = "mainpan", open = c("Map and Property View"),close=c("Time Series and Data"));
      h1 <- Highcharts$new()     
      h1$chart(type = "spline",zoomType="x")
      h1$yAxis(title = list(text="Value"),min=0)
      h1$xAxis(title = list(text="Year"), #categories = x,
               tickmarkPlacement="on")
      
      h1$title(text=paste("",sep=""))
      
      
      h1$legend(symbolWidth = 80)
      h1$set(dom = 'timeseries')
      h1$plotOptions(animation=FALSE,
                     area=list(
                       stacking="normal",
                       lineColor="#666666",
                       lineWidth=1,
                       marker = list(
                         lineWidth=1,
                         lineColor="#666666"
                       )
                     ));
      h1$exporting(enabled = T)    
      return(h1)    
    }
    
  })
  
  
  
  observe({
    tree <- input$tree
    t = get_selected(tree)
      if(length(t)>0) {
        ans = attr(t[[1]],"ancestry")
        if(length(ans)>0) {
          country = ans[1]
        } else {
          country = t[[1]]
        }
        if (length(ans)==4) {  
          lk = unlist(lapply(strsplit(ans,"\\("),function(x) { gsub(" $","",x[1]) } ))
          as.character(ref_objects[ref_objects$energy.source==lk[3] & ref_objects$level==lk[2] & ref_objects$country.name==lk[1] & ref_objects$name==t[[1]],]$technology)
          values$selectedtech = as.character(ref_objects[ref_objects$energy.source==lk[3] & ref_objects$level==lk[2] & ref_objects$country.name==lk[1] & ref_objects$name==t[[1]],]$technology)
        } else {
          values$selectedtech = ""
        }
      } else {
        values$selectedtech = ""
      }
  })
  
  
  observe({
    demo=0
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query$demo)) {
      if(query$demo==6) {
        demo=6
      }
    }
    
    if(!is.null(input$nav) && !is.null(input$s2s3) ) {
      if((input$nav=="STEP 3") && (input$s2s3<1) && (demo!=6) ) {
        updateTabsetPanel(session, "nav", selected = "STEP 2")
      }
    }    
  })
  
  observe({
    demo=0
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query$demo)) {
      if(query$demo==6) {
        demo=6
      }
    }
    
    
    if(!is.null(input$nav) && !is.null(input$s1s2) ) {
      if((input$nav=="STEP 2") && (input$s1s2<1) && (demo!=6)  ) {
        
        updateTabsetPanel(session, "nav", selected = "STEP 1")
      }
    }    
  })
  
  
  ### NEXT/Back
  observe({    
    if(!is.null(input$s0s1)) {      
    if(input$s0s1>0) {   
      updateTabItems(session, "step", selected = "STEP1")
      #updateTabsetPanel(session, "nav", selected = "STEP 1")
    }}    
  })
  observe({    
    if(!is.null(input$s1s2)) {      
    if(input$s1s2>0) {      
      isolate({
      thepolicies = do.call(c,lapply(values$createdpolicy,function(x) {x$name}))
      if(length(thepolicies)>1) {
        thepolicies = thepolicies[thepolicies!="NONE"]  
      }
      if (length(thepolicies)>=2) {
        session$sendCustomMessage('activeNavs', 'STEP 2')
        updateTabsetPanel(session, "step", selected = "STEP2")
      } else {
        createAlert(session, "s1alert", "s1a", title = "Step1: Error",
                    content = "Create at least 2 Policies!", append = FALSE);
      }
      });
    }}    
  })
  observe({    
    if(!is.null(input$s2s1)) {      
    if(input$s2s1>0) {      
      updateTabsetPanel(session, "step", selected = "STEP1")
    }}    
  })
  observe({    
    if(!is.null(input$s2s3)) {      
    if(input$s2s3>0) {      
       if( (isolate(input$lockedscenario)!='NONE') && (isolate(input$lockedbaseline)!='NONE')  ) {       
         session$sendCustomMessage('activeNavs', 'STEP 3') 
         updateTabsetPanel(session, "step", selected = "STEP3")
        } else {
          createAlert(session, "s2alert", "s2a", title = "Step2: Error",
                      content = "Please Lock>> both a BASELINE and SCENARIO policy before proceeding to Step3!", append = FALSE)
        }        
    }}
  })
  observe({    
    if(!is.null(input$s3s2)) {
     if(input$s3s2>0) {      
       updateTabsetPanel(session, "step", selected = "STEP2")
     }    
    }
  })
  observe({    
    if(!is.null(input$s3s4)) {
     if(input$s3s4>0) {      
      updateTabsetPanel(session, "step", selected = "HELP")
     }    
    }
  })
#   observe({    
#     if(input$s4s3>0) {      
#       updateTabsetPanel(session, "nav", selected = "STEP 3")
#     }    
#   })
  
})

# 
# ################### Collapse ##################
# if(input$layout=="collapse") {
#   thec = lapply(
#     thev,function(x) {
#       component <<- x;
#       componentfields <<- names(x);
#       ui = " ";
#       if(file.exists(paste("components/",x$type,".R",sep=""))) {
#         source(paste("component.R",sep=""),local=TRUE);
#       }
#       b = bsCollapsePanel(paste(as.character(x$name),sep=""),style="info",ui)
#     })
#   bsCollapse2 <- function (panels, id = NULL, multiple = FALSE, open = NULL)  {
#     if (is.null(id)) 
#       id = paste0("collapse", sprintf("%07i", as.integer(stats::runif(1, 
#                                                                       1, 1e+06))))
#     if (!multiple & length(open) > 1) {
#       open <- open[1]
#     }
#     for (i in seq(length(panels))) {
#       if (shinyBS:::getAttribs(panels[[i]])$value %in% open) {
#         panels[[i]]$children[[2]] <- shinyBS:::addClass(panels[[i]]$children[[2]], 
#                                                         "in")
#       }
#       if (!multiple) {
#         panels[[i]]$children[[1]]$children[[1]]$children[[1]] <- shinyBS:::addAttribs(panels[[i]]$children[[1]]$children[[1]]$children[[1]], 
#                                                                                       `data-parent` = paste0("#", id))
#       }
#     }
#     bsTag <- shiny::tags$div(class = "panel-group sbs-panel-group", 
#                              `data-sbs-multi` = multiple, id = id, role = "tablist", 
#                              panels)
#     htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
#   }
#   if(length(thec)>0) {
#     
#     dashboard = bsCollapse2(id="story",multiple=T,panels=thec,
#                             open = unlist(lapply(thev,function(x) {if(x$open==TRUE) { x$name} else {NULL} })) )
#   } else {
#     dashboard = "Click on 'Manage Dashboard' on the left to Add Items."
#   }
# }
# 
# ############## Box View ######################
# if(input$layout=="box") {
#   thec = lapply(
#     thev,function(x) {
#       component <<- x;
#       componentfields <<- names(x);
#       ui = " ";
#       if(file.exists(paste("components/",x$type,".R",sep=""))) {
#         source(paste("component.R",sep=""),local=TRUE);
#       }
#       b = box(ui,width=x$width,collapsible=TRUE,title=x$name) #bsCollapsePanel(paste(as.character(x$name),sep=""),style="info",ui)
#       attr(b,"component") <- x;
#       b
#     })
#   
#   if(length(thec)>0) {
#     dashboard = thec
#   } else {
#     dashboard = "Click on 'Manage Dashboard' on the left to Add Items."
#   }    
# }
# 
