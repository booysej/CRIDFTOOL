ui = tags$div(style="height: 400px; width: 100%;",
         fluidRow(
           column(4,
                  selectInput("d1seriesgvtsDASHBOARD.uuid", "",
                              as.character(unique(series1$series)) ) # defined in global.R
           ),
           column(4,
                  ""
           )
         ), htmlOutput("d1t1gvtsDASHBOARD.uuid")
)

# Store all Observers here for Lifetime Management
observerpool[["DASHBOARD.uuid"]] <<- list();

values$d1t1gvtsDASHBOARD.uuid <- "DASHBOARD.chart";

observerpool[["DASHBOARD.uuid"]][[1]] <<- observe({
  if(!is.null(input$gviswrapperd1t1gvtsDASHBOARD.uuid)) {
    aa <- fromJSON(input$gviswrapperd1t1gvtsDASHBOARD.uuid);
    type = as.character(aa$chartType);
    values$d1t1gvtsDASHBOARD.uuid = type
  } 
  
},suspended = TRUE)

output$d1t1gvtsDASHBOARD.uuid <- renderGvis({      
  thewater = input$d1water    
  theuclf = input$d1uclf
  theuclf2 = input$d1uclf2
  thecountry = values$country
  thepolicy = "unconstraint"    
  exclGI = input$withoutGrandInga
  load = input$d1cons
  varyload=TRUE
  
  
  if (!is.null(thewater) & !is.null(theuclf) & !is.null(theuclf2) & !is.null(thecountry)   ) {
    a = barunconstraintGVTS(thewater,theuclf,theuclf2,thecountry, thedom="d1t1gvtsDASHBOARD.uuid","Unconstraint","All",
                            values$startyear,values$endyear,exclGI,varyload,load,shinyid="d1t1gvtsDASHBOARD.uuid",h=300,
                            input$d1seriesgvtsDASHBOARD.uuid,values$d1t1gvtsDASHBOARD.uuid);
    
    return(a);
  }
})


