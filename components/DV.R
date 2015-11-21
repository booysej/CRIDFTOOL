######### UI ############### DASHBOARD.??? gets replaced
ui= tags$div(style='height: 400px; width: 100%;',
         fluidRow(
           column(4,
                  selectInput('d1seriesgvDASHBOARD.uuid', '',
                              as.character(unique(series1$series)) ) # defined in global.R
           ),
           column(4,
                  sliderInput('d1yeargvDASHBOARD.uuid', '', 2010, 2031, 2015,1,animate=list(loop=TRUE),ticks=FALSE)
           ),
           column(4,''
           )
         ), htmlOutput('d1t1gvDASHBOARD.uuid')
)

observerpool[["DASHBOARD.uuid"]] <<- list();
    
values$d1t1gvDASHBOARD.uuid <- "DASHBOARD.chart";
   
observerpool[["DASHBOARD.uuid"]][[1]] <<- observe({
  if(!is.null(input$gviswrapperd1t1gvDASHBOARD.uuid)) {
    aa <- fromJSON(input$gviswrapperd1t1gvDASHBOARD.uuid);
    type = as.character(aa$chartType);
    values$d1t1gvDASHBOARD.uuid = type
  }
},suspended = TRUE)

output$d1t1gvDASHBOARD.uuid <- renderGvis({      
  thewater = input$d1water    
  theuclf = input$d1uclf
  theuclf2 = input$d1uclf2
  thecountry = values$country
  thepolicy = 'unconstraint'    
  exclGI = input$withoutGrandInga
  load = input$d1cons
  varyload=TRUE
  
  
  if (!is.null(thewater) & !is.null(theuclf) & !is.null(theuclf2) & !is.null(thecountry)   ) {
    a = barunconstraintGV(thewater,theuclf,theuclf2,thecountry, thedom='d1t1DASHBOARD.uuid','Unconstraint','All',
                          values$startyear,values$endyear,exclGI,varyload,load,shinyid='d1t1gvDASHBOARD.uuid',h=300,input$d1seriesgvDASHBOARD.uuid,
                          input$d1yeargvDASHBOARD.uuid, values$d1t1gvDASHBOARD.uuid);
    
    return(a);
  }
})