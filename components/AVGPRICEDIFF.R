width=900
height=550
minwidth=900
minheight=550
######### UI ############### DASHBOARD.??? gets replaced
ui= tags$span(  
                showOutput("avgDASHBOARD.uuid", "highcharts"),
                plotOutput("plotDASHBOARD.uuid",height = "10px")
)

# Store all Observers here for Lifetime Management
observerpool[["DASHBOARD.uuid"]] <<- list();
output$plotDASHBOARD.uuid <- renderPlot({ par(mar=c(0,0,0,0)); plot(1, type="n", axes=F, xlab="", ylab="") })

output$avgDASHBOARD.uuid <- renderChart({      
      thewater = input$d1water    
      theuclf = input$d1uclf
      theuclf2 = input$d1uclf2
      thecountry = values$country
      thepolicy = "unconstraint"    
      exclGI = input$withoutGrandInga
      load = input$d1cons
      varyload=TRUE
      
      if (!is.null(thewater) & !is.null(theuclf) & !is.null(theuclf2) & !is.null(thecountry)   ) {
        r1 = demo2(thewater,theuclf,theuclf2,thecountry, thedom="avgDASHBOARD.uuid",paste(values$startyear,values$endyear,sep="-"),"All",
                   values$startyear,values$endyear,exclGI,varyload,load)
        r1$set(width =  session$clientData$output_plotDASHBOARD.uuid_width )
        return(r1);
      }
})
    