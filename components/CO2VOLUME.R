width=600
height=400
minwidth=600
minheight=400
######### UI ############### DASHBOARD.??? gets replaced
ui = tags$span(id="componentDASHBOARD.uuid",
        showOutput("co2volDASHBOARD.uuid", "highcharts"),
        plotOutput("plotDASHBOARD.uuid",height = "10px")
     )

    output$plotDASHBOARD.uuid <- renderPlot({ par(mar=c(0,0,0,0)); plot(1, type="n", axes=F, xlab="", ylab="") })
    
    output$co2volDASHBOARD.uuid <- renderChart({
      thewater = input$d1water    
      theuclf = input$d1uclf
      theuclf2 = input$d1uclf2
      thecountry = values$country
      exclGI = input$withoutGrandInga
      varyload=TRUE
      load = input$d1cons
      
      r1 = co2vol(thewater,theuclf,theuclf2,thecountry,theseries="CO2", thedom="co2volDASHBOARD.uuid",exclGI,varyload,load)
      r1$set(width =  session$clientData$output_plotDASHBOARD.uuid_width )
      return(r1)
    })
    
    
    co2vol <- function(thewater,thecoaluclf,thetxuclf,thecountry,theseries="Demand", thedom="d1t2a", exclGI=FALSE,adjcons=FALSE,cons=0) {
      
      if (!is.null(thewater) && !is.null(thecoaluclf) && !is.null(thetxuclf) ) {    
        
        ## Low Water
        td = getunconstraint(100/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,cons/100)  
        tfinal = subset(td, series %in% theseries)  
        units = as.character(tfinal$unit[1])
        if (thecountry!="All") {
          tfinal = subset(tfinal, country.name == thecountry)          
        }
        tfinal2 = tfinal[, c("time","value"),with=F]
        tfinal3 = tfinal2[, lapply(.SD, sum), by = c("time")]             
        tfinal3 = tfinal3[(tfinal3$time>2010) & (tfinal3$time<2050) ,]
        x = tfinal3;
        
        ## High Water
        td = getunconstraint(120/100, thecoaluclf/100,thetxuclf/100, exclGI,adjcons,cons/100)  
        tfinal = subset(td, series %in% theseries)  
        units = as.character(tfinal$unit[1])
        if (thecountry!="All") {
          tfinal = subset(tfinal, country.name == thecountry)          
        }
        tfinal2 = tfinal[, c("time","value"),with=F]
        tfinal3 = tfinal2[, lapply(.SD, sum), by = c("time")]             
        tfinal3 = tfinal3[(tfinal3$time>2010) & (tfinal3$time<2050) ,]
        x2 = tfinal3;
        
        
        h1 <- rCharts:::Highcharts$new()
        h1$chart(type = "spline",marginLeft=80,height=300)
        h1$title(text = paste(paste(theseries,collapse=",") ," (",thecountry[1],")",sep="") )
        
        h1$xAxis(categories = x$time,labels=list(enabled=TRUE))
        h1$yAxis(title = list(text = units),min=0)        
        h1$series( data = x$value, type="spline", name=paste(theseries,"Assume 10% Lower Water from Baseline (100%)",sep=" - ")  )
        h1$series( data = x2$value, type="spline", name=paste(theseries,"Assume 10% More Water from Baseline (120%)",sep=" - ")  )
        
        h1$legend(symbolWidth = 80,enabled=TRUE)
        h1$set(dom = thedom)
        h1$plotOptions(animation=FALSE,spline=list(animation=FALSE))
        h1$exporting(enabled = F)          
        
        return(h1)         
      }        
    }