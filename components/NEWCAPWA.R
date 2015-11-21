width=600
height=650
minwidth=400
minheight=650
######### UI ############### DASHBOARD.??? gets replaced
ui= tags$span(
             showOutput("demo1aDASHBOARD.uuid", "highcharts"),
             showOutput("demo1bDASHBOARD.uuid", "highcharts"),
             plotOutput("plotDASHBOARD.uuid",height = "10px")
             )

observerpool[["DASHBOARD.uuid"]] <<- list();
    
   output$plotDASHBOARD.uuid <- renderPlot({ par(mar=c(0,0,0,0)); plot(1, type="n", axes=F, xlab="", ylab="") })

    output$demo1aDASHBOARD.uuid <- renderChart({      
      thewater = input$d1water    
      theuclf = input$d1uclf
      theuclf2 = input$d1uclf2
      thecountry = values$country
      thepolicy = "unconstraint"    
      exclGI = input$withoutGrandInga
      load = input$d1cons
      varyload=TRUE
      
      if (!is.null(thewater) & !is.null(theuclf) & !is.null(theuclf2) & !is.null(thecountry)   ) {
        r1 = demo1(100,theuclf,theuclf2,thecountry, thedom="demo1aDASHBOARD.uuid","Assume 10% Lower Water from Baseline (100%)","All",
                     values$startyear,values$endyear,exclGI,varyload,load);     
        
        r1$set(width =  session$clientData$output_plotDASHBOARD.uuid_width )
        return(r1)
      }
    })
    
    output$demo1bDASHBOARD.uuid <- renderChart({      
      thewater = input$d1water    
      theuclf = input$d1uclf
      theuclf2 = input$d1uclf2
      thecountry = values$country
      thepolicy = "unconstraint"    
      exclGI = input$withoutGrandInga
      load = input$d1cons
      varyload=TRUE
      
      if (!is.null(thewater) & !is.null(theuclf) & !is.null(theuclf2) & !is.null(thecountry)   ) {
        r1 = demo1(120,theuclf,theuclf2,thecountry, thedom="demo1bDASHBOARD.uuid","Assume 10% More Water from Baseline (120%)","All",
                     values$startyear,values$endyear,exclGI,varyload,load)                
        r1$set(width =  session$clientData$output_plotDASHBOARD.uuid_width)
        return(r1)
      }
    })
    
    