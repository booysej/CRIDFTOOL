width=600
height=1300
minwidth=400
minheight=1300


######### UI ############### DASHBOARD.??? gets replaced
ui = tags$span(id="componentDASHBOARD.uuid",
        showOutput("d1t2aDASHBOARD.uuid", "highcharts"),
        showOutput("d1t2bDASHBOARD.uuid", "highcharts"),
        showOutput("d1t2cDASHBOARD.uuid", "highcharts"),
        showOutput("d1t2dDASHBOARD.uuid", "highcharts"),
        plotOutput("plotDASHBOARD.uuid",height = "10px")
     )

    output$plotDASHBOARD.uuid <- renderPlot({ par(mar=c(0,0,0,0)); plot(1, type="n", axes=F, xlab="", ylab="") })
    
    output$d1t2aDASHBOARD.uuid <- renderChart({
      thewater = input$d1water    
      theuclf = input$d1uclf
      theuclf2 = input$d1uclf2
      thecountry = values$country
      exclGI = input$withoutGrandInga
      varyload=TRUE
      load = input$d1cons
      
      r1 = infounconstraint(thewater,theuclf,theuclf2,thecountry,theseries="Demand", thedom="d1t2aDASHBOARD.uuid",exclGI,varyload,load)
      r1$set(width =  session$clientData$output_plotDASHBOARD.uuid_width )
      return(r1)
    })
    output$d1t2bDASHBOARD.uuid <- renderChart({
      thewater = input$d1water    
      theuclf = input$d1uclf
      theuclf2 = input$d1uclf2
      thecountry = values$country
      exclGI = input$withoutGrandInga
      varyload=TRUE
      load = input$d1cons
      r1 = infounconstraint(thewater,theuclf,theuclf2,thecountry,theseries="Generation", thedom="d1t2bDASHBOARD.uuid",exclGI,varyload,load)
      r1$set(width =  session$clientData$output_plotDASHBOARD.uuid_width )
      return(r1)
    })
    output$d1t2cDASHBOARD.uuid <- renderChart({
      thewater = input$d1water    
      theuclf = input$d1uclf
      theuclf2 = input$d1uclf2
      thecountry = values$country
      exclGI = input$withoutGrandInga
      varyload = TRUE
      load = input$d1cons
      r1 = infounconstraint(thewater,theuclf,theuclf2,thecountry,
                              theseries=c("Export revenue","Import cost","TransmissionInvestment",
                                          "Investment","Annual Investment","Fuel Cost","O&M Costs"), thedom="d1t2cDASHBOARD.uuid",exclGI,varyload,load)
      r1$set(width =  session$clientData$output_plotDASHBOARD.uuid_width )
      return(r1)
    })
    output$d1t2dDASHBOARD.uuid <- renderChart({
      thewater = input$d1water    
      theuclf = input$d1uclf
      theuclf2 = input$d1uclf2
      thecountry = values$country
      exclGI = input$withoutGrandInga
      varyload=TRUE
      load = input$d1cons
      r1 = infounconstraint(thewater,theuclf,theuclf2,thecountry,theseries="Import Capacity", thedom="d1t2dDASHBOARD.uuid",exclGI,varyload,load)
      r1$set(width =  session$clientData$output_plotDASHBOARD.uuid_width )
      return(r1)
    })
    
    