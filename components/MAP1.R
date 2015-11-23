width=400
height=400
minwidth=400
minheight=400

ui= tags$div(
  bsModal(id="mapmodalDASHBOARD.uuid", title="Timeseries View",trigger=NULL, size = "large",
         showOutput("d1mapchartDASHBOARD.uuid", "highcharts") 
         ),
         conditionalPanel(
           condition = "output.d1m1DASHBOARD.uuid==null ",                                    
           div(class = "busy",
               p("Loading Map ..."),
               img(src="ajaxloaderq.gif")
           )
         ),
div(id="componentDASHBOARD.uuid", style=paste("height: ",(DASHBOARD.height-35),"px; !important; overflow:hidden;width:'100%';",sep=""),
    
    leafletOutput("d1m1DASHBOARD.uuid", width="100%",height="95%"),
    conditionalPanel(
      condition = "output.d1m1DASHBOARD.uuid!=null ",    
      
      tags$div(id="part3DASHBOARD.uuid",style="position: relative;left: 70px;top: -150px;pointer-events:none;",
               tags$table(width="100%", style="border-spacing:0; border-collapse: collapse; width:'100%'; ",
                          tags$tr(
                            tags$td(style="horizontal-align: left;pointer-events: all;",
                              sliderInput("d1yearDASHBOARD.uuid", 
                                          " Tx Flows Year", 
                                          2010, 2031, 2015,1,
                                          animate=list(loop=TRUE),ticks=FALSE,width = 200,sep="")),
                            tags$td(style="horizontal-align: right;pointer-events: all;",
                              selectInput("mapviewDASHBOARD.uuid","Map Icon Graphs:",
                                          c("Average Price Difference" = "price",
                                            "New Capacity" = "capacity",
                                            "Generation Mix" = "generation",
                                            "CO2 Difference" = "co2"
                                            #"CO2/Generation" = "co2gen"
                                          ),
                                          selected="New Capacity"
                              )
                            )
                          ),
                          tags$tr(tags$td(colspan=2,tags$div(style="pointer-events:none;",imageOutput("m1legendDASHBOARD.uuid", height = "30px")) ))
               )
      ) #div
    )))

js = ""

# Store all Observers here for Lifetime Management
observerpool[["DASHBOARD.uuid"]] <<- list();
 
values$map1suspendedDASHBOARD.uuid <- TRUE;
values$geojsonDASHBOARD.uuid = geojson; 



###########################################
observerpool[["DASHBOARD.uuid"]][[1]]  <<- observe({
  mv = input$mapviewDASHBOARD.uuid;
  isolate({
    values$mapviewDASHBOARD.uuid = mv;
  })
},autoDestroy=TRUE, suspended = TRUE)

###########################################
# Update Arrows Step1
observerpool[["DASHBOARD.uuid"]][[2]]  <<- observe({
  try({
  if (!is.null(values$map1suspendedDASHBOARD.uuid)) {
  if (!values$map1suspendedDASHBOARD.uuid) {
    thewater = input$d1water    
    theuclf = input$d1uclf
    theuclf2 = input$d1uclf2
    thecountry = values$country
    theyear = input$d1yearDASHBOARD.uuid
    exclGI = input$withoutGrandInga
    load = input$d1cons
    varyload = TRUE
    thetime = values$dashactions;
    #print(thetime)
    
    if (!is.null(thewater) & !is.null(theuclf) & !is.null(theuclf2) & !is.null(thecountry) & !is.null(theyear)  ) {
      print("SHOW")
      #isolate({
      showarrowsunconstraint(thewater,theuclf,theuclf2,thecountry, theyear,"TransmissionOutput",TRUE,"d1m1DASHBOARD.uuid",exclGI,varyload,load,values$mapviewDASHBOARD.uuid);
      #});
    }
  }
  }
  });
},autoDestroy=TRUE, suspended = TRUE);


###########################################
# Initial Map Step 1 setup
observerpool[["DASHBOARD.uuid"]][[3]]  <<- observe({
  if(!is.null(values$dashactions)) {
    values$map1suspendedDASHBOARD.uuid = FALSE;
  }
},autoDestroy=TRUE, suspended = TRUE)

output$d1m1DASHBOARD.uuid <- renderLeaflet({
  leaflet(height=600) %>%
    addGeoJSON(geojson,layerId="main") %>%
    addTiles(options=tileOptions(minZoom = 4, maxZoom = 6),attribution="Enerweb EOH")   %>% 
    setView(22.8731,-22.9992,5) %>% hideArrows(mapname="d1m1DASHBOARD.uuid",behaviour="hideall");
}) 

###########################################
output$m1legendDASHBOARD.uuid <- renderImage({
  
  if (input$mapviewDASHBOARD.uuid=="capacity") {
    list(src = "www/images/legend2.png",
         contentType = 'image/png',
         width = 600,
         height = 40,
         alt = "New Capacity Legend")
  } else if (input$mapviewDASHBOARD.uuid=="price") {
    list(src = "www/images/legend3.png",
         contentType = 'image/png',
         width = 600,
         height = 30,
         alt = "Avg Price Diff Legend")
  } else if (input$mapviewDASHBOARD.uuid=="generation") {
    list(src = "www/images/legend2.png",
         contentType = 'image/png',
         width = 600,
         height = 40,
         alt = "Generation Legend")
  } else if (input$mapviewDASHBOARD.uuid=="co2") {
    list(src = "www/images/legend3.png",
         contentType = 'image/png',
         width = 600,
         height = 30,
         alt = "CO2 Diff Legend")
  }
  
},deleteFile=FALSE)

###########################################
# Click on icon marker
observerpool[["DASHBOARD.uuid"]][[4]]  <<- observe({
  click <- input$d1m1DASHBOARD.uuid_marker_click;
  if(is.null(click))
    return()
  print(click);
  text2 <-paste(" ", as.character(dt[dt$ID==click$id,]$COUNTRY) )
  
  map = leafletProxy("d1m1DASHBOARD.uuid");  
  #map %>% addPopups( click$lng, click$lat, text2);
  
  isolate({
    values$markercountryDASHBOARD.uuid <- as.character(dt[dt$ID==click$id,]$COUNTRY)
  });
  
  toggleModal(session, "mapmodalDASHBOARD.uuid", toggle = "open")
  #map %>% addPopups( click$lng, click$lat, uiOutput("test1"));
},autoDestroy=TRUE, suspended = TRUE)

###########################################
# click on sea map ( show all)
observerpool[["DASHBOARD.uuid"]][[5]]  <<- observe({
  if(!is.null(input$d1m1DASHBOARD.uuid_click)) {   
    isolate({
      values$geojsonDASHBOARD.uuid$features <- isolate(lapply(values$geojsonDASHBOARD.uuid$features, function(feat) {              
        feat$properties$style <- list(fillOpacity = 0,color="green")
        values$selectedfeat = NULL;
        feat
      }))    
      proxy = leafletProxy("d1m1DASHBOARD.uuid") # %>% removeGeoJSON(geojson[[2]]$properties$name)             
      proxy %>% addGeoJSON(isolate(values$geojsonDASHBOARD.uuid),layerId="main") 
      values$country <- 'All';
    });
  }
},priority=1000,autoDestroy=TRUE, suspended = TRUE)

###########################################
output$aggregate_inflows <- renderText({
  paste("Aggregate_inflows: ",input$d1m1DASHBOARD.uuid_arrownode$properties$aggregate_inflows,sep="")
})

###########################################
output$aggregate_outflows <- renderText({
  paste("Aggregate_outflows: ",input$d1m1DASHBOARD.uuid_arrownode$properties$aggregate_outflows,sep="")    
})

###########################################
observerpool[["DASHBOARD.uuid"]][[6]]  <<- observe({
  if(!is.null(input$d1m1DASHBOARD.uuid_arrownode)) {
    print(input$d1m1DASHBOARD.uuid_arrownode$properties$aggregate_inflows)
    print(input$d1m1DASHBOARD.uuid_arrownode$properties$aggregate_outflows)
  }    
}, suspended = TRUE);

###########################################
# click on country s1
observerpool[["DASHBOARD.uuid"]][[7]]  <<- observe({
  theclick = NULL;    
  if(!is.null(input$d1m1DASHBOARD.uuid_geojson_click)) {     
    theclick = input$d1m1DASHBOARD.uuid_geojson_click      
  }    
  
  isolate({    
    if(!is.null(theclick)) {                 
      tcountry <- names(gistranslate[gistranslate==theclick$properties$COUNTRY])              
      if(!is.null(tcountry)) {   
        isolate({
          if(nchar(tcountry[1])>0) {
            gist = as.character(unlist(gistranslate[gsub(" $","",strsplit(tcountry,"\\(")[[1]][1])]))        
            if( (length(gist)>0) && (nchar(gist)>0) ) {    
              
              values$geojsonDASHBOARD.uuid$features <- isolate(lapply(values$geojsonDASHBOARD.uuid$features, function(feat) {              
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
      
      # session$sendInputMessage("d1m1_geojson_click",list(value=NULL));
      #updateTextInput(session, "d1m1_geojson_click",value=NULL);
      
      # print(values$geojson[[2]]$properties$name)
      
      proxy = leafletProxy("d1m1DASHBOARD.uuid") #%>% removeGeoJSON(values$geojson[[2]]$properties$name)             
      #proxy %>% removeGeoJSON("urn:ogc:def:crs:OGC:1.3:CRS84");
      proxy %>% addGeoJSON(isolate(values$geojsonDASHBOARD.uuid),layerId="main")  # %>% addTiles( "tiles2/{z}/{x}/{y}.png")                   
      
      values$country <- tcountry
    }
  });
  
},priority=1000,autoDestroy=TRUE, suspended = TRUE)

###########################################
output$d1mapchartDASHBOARD.uuid <- renderChart({      
  thewater = input$d1water    
  theuclf = input$d1uclf
  theuclf2 = input$d1uclf2
  thecountry = values$markercountryDASHBOARD.uuid
  thepolicy = "unconstraint"    
  exclGI = input$withoutGrandInga
  load = input$d1cons
  theyear = input$d1year 
  varyload=TRUE
  
  if (!is.null(thewater) & !is.null(theuclf) & !is.null(theuclf2) & !is.null(thecountry)   ) {
    if(input$mapviewDASHBOARD.uuid=="capacity") {
      return(barunconstraint(thewater,theuclf,theuclf2,thecountry, thedom="d1mapchartDASHBOARD.uuid","Unconstraint","All",
                             values$startyear,
                             values$endyear,exclGI,varyload,load,seriesname = "New Capacity"));     
    } else if(input$mapviewDASHBOARD.uuid=="price") {
      return(demo2(thewater,theuclf,theuclf2,thecountry, thedom="d1mapchartDASHBOARD.uuid",paste(values$startyear,values$endyear,sep="-"),"All",
                   values$startyear,values$endyear,exclGI,varyload,load));  
    } else if(input$mapviewDASHBOARD.uuid=="generation") {
      return(barunconstraint(thewater,theuclf,theuclf2,thecountry, thedom="d1mapchartDASHBOARD.uuid","Unconstraint","All",
                             values$startyear,
                             values$endyear,exclGI,varyload,load,seriesname = "Generation"));     
    } else if(input$mapviewDASHBOARD.uuid=="co2") {
      return(demo2(thewater,theuclf,theuclf2,thecountry, thedom="d1mapchartDASHBOARD.uuid",paste(values$startyear,values$endyear,sep="-"),"All",
                   values$startyear,values$endyear,exclGI,varyload,load,seriesname = "CO2",units = "% Difference in CO2 "));  
    }
      
      
  }
})

