width=500 # default
height=400 # default
minwidth=200
minheight=200

library(data.table)
library(lattice)

ui=tags$span(
         plotOutput("componentDASHBOARD.uuid",height=DASHBOARD.height-40,width=DASHBOARD.width-10)
      )

# Store all Observers here for Lifetime Management
observerpool[["DASHBOARD.uuid"]] <<- list();

output$componentDASHBOARD.uuid <- renderPlot({
      thewater = input$d1water    
      thecoaluclf = input$d1uclf
      thetxuclf = input$d1uclf2
      exclGI = input$withoutGrandInga
      cons = input$d1cons
      
      td = getunconstraint(thewater/100, thecoaluclf/100,thetxuclf/100, exclGI,TRUE,cons/100)
      seriesname = "New Capacity"
      tfinal = subset(td, series == seriesname)  
      tfinal2 = tfinal[, c("time","value","energy.source"),with=F]
      tfinal3 = tfinal2[, lapply(.SD, sum), by = c("time","energy.source")]     
      tfinal3 = tfinal3[(tfinal3$time>2010) & (tfinal3$time<2050) ,]
      attach(tfinal3)
      xyplot(value~time|energy.source,type="l",title=session$user)
})
      
