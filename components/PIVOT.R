width=500 # default
height=600 # default
minwidth=500
minheight=600

library(data.table)
library(lattice)

ui=tags$div(id="componentDASHBOARD.uuid",style="overflow-y: scroll;overflow-x: scroll;",
            selectInput("d2pivottscomponentDASHBOARD.uuid", "Filter Time Series:",
                        as.character(unique(series1$series))
                        ,multiple=T,selected="New Capacity") ,
        conditionalPanel(
          condition = "output.pivotDASHBOARD.uuid==null ",                                    
          div(class = "busy",
            p("Loading Pivot ..."),
            img(src="ajaxloaderq.gif")
          )
        ),  rpivotTableOutput("pivotDASHBOARD.uuid",height=paste(DASHBOARD.height-120,"px",sep=""),width = "400px") 
         # plotOutput("componentDASHBOARD.uuid",height=DASHBOARD.height-40,width=DASHBOARD.width-10)
      )

# Store all Observers here for Lifetime Management
observerpool[["DASHBOARD.uuid"]] <<- list();

output$pivotDASHBOARD.uuid <-renderRpivotTable({  
  
  thewater = input$d1water    
  theuclf = input$d1uclf
  theuclf2 = input$d1uclf2
  exclGI = input$withoutGrandInga
  load = input$d1cons
  thecountry = values$country
  varyload=TRUE
    
    td = getunconstraint(thewater/100, theuclf/100,theuclf2/100, exclGI,varyload,load/100)  
    tfinal = subset(td, series == input$d2pivottscomponentDASHBOARD.uuid)  
    #tfinal = td
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
  
  
})
outputOptions(output, "pivotDASHBOARD.uuid", suspendWhenHidden = FALSE)