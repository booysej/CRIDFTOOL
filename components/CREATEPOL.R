width=230
height=700
minwidth=230
minheight=700


library(data.table)
ui= tags$span(
                  textInput("s1policynameDASHBOARD.uuid", "Create New Policy Name for Assuptions:"),
                  bsAlert("s1alert"),
                  bsButton("s1createpolicyDASHBOARD.uuid","Create Policy",style="primary"),
                  uiOutput("createlistuiDASHBOARD.uuid"),
                  #fluidRow(
                  #  column(12,
                           uiOutput("s1infoDASHBOARD.uuid"),
                  #         )
                  #),
                  #fluidRow(
                  #  column(12,
                         tags$h5("For Selected Policy:"), 
                         bsButton("s1deletepolicyDASHBOARD.uuid","Delete",style="warning"),
                         bsButton("s1loadpolicyDASHBOARD.uuid","< Show >",style="info"),
                         #bsButton("s1downloadpolicy","Download",style="info"),
                         bsModal("modalDeleteDASHBOARD.uuid", "Delete Policy: Are you sure?", "s1deletepolicyDASHBOARD.uuid", size = "small",
                                 bsButton("s1deletepolicyYDASHBOARD.uuid","Yes - Delete",style="warning"),
                                 bsButton("s1deletepolicyNDASHBOARD.uuid","No - Cancel",style="info")),
                   # )
                  #),
                  tags$br(),
                  tags$h5("Help: Create policies based on assuptions by changing the slider positions (Generation Expantion Plan runs UNCONSTRAINT), then when happy enter a 'Policy Name' and then click 'Create Policy'."),
                  tags$h5("Note: Create at least 2 Policies before proceeding to STEP2, Policy names must be at least 6 Characters long."),
                  "Download Data for Slider Selection",
                  downloadButton('s1downloadpolicyDASHBOARD.uuid', 'Download')
           );



# Store all Observers here for Lifetime Management - Start all with Suspended=TRUE
observerpool[["DASHBOARD.uuid"]] <<- list()


# Create new policy
observerpool[["DASHBOARD.uuid"]][[1]] <<- observe({
  if(!is.null(input$s1createpolicyDASHBOARD.uuid)) {
    if(input$s1createpolicyDASHBOARD.uuid>0) {
      isolate({
        if (!is.null(isolate(input$createdpoliciesDASHBOARD.uuid))) {
          if(!is.null(input$s1policynameDASHBOARD.uuid)) {
            if(nchar(input$s1policynameDASHBOARD.uuid)>5) {
              #values$createdpolicy <- isolate(values$createdpolicy[!grepl("NONE",values$createdpolicy)])
              
              thepolicies = do.call(c,lapply(values$createdpolicy,function(x) {x$name})) # GLOBAL values$createdpolicy
              if (!input$s1policynameDASHBOARD.uuid %in% thepolicies) {
                
                exclGI = input$withoutGrandInga # GLOBAL 
                varyload=TRUE
                load = input$d1cons # GLOBAL 
                
                values$createdpolicy[[isolate(length(values$createdpolicy))+1]] <- isolate( # GLABAL
                  list(name=input$s1policynameDASHBOARD.uuid,
                       thewater=input$d1water,
                       thecoaluclf=input$d1uclf,
                       thetxuclf=input$d1uclf2,
                       varyload=varyload,load=load,withoutinga=exclGI) # GLOBALS
                )
                values$lockedbasepolicy="NONE"; # GLOBAL
                values$lockedscenpolicy="NONE"; # GLOBAL
                
                #cp = values$createdpolicy;
                #save(cp,file = "/tmp/cp.rdata");
                #values$availablepolicy <- isolate(values$createdpolicy)
              }
              closeAlert(session, "s1a")
            } else {
              createAlert(session, "s1alert", "s1a", title = "Step1: Error",
                          content = "Policy Name need to be at least 6 characters long!", append = FALSE)
            }
          }
        }
      });
    }}
  
  values$availablepolicy <- isolate(values$createdpolicy) # GLOBAL
}, suspended = TRUE);

# Close delete confirm
observerpool[["DASHBOARD.uuid"]][[2]] <<- observe({
  if(!is.null(input$s1deletepolicyNDASHBOARD.uuid)) {
    if(input$s1deletepolicyNDASHBOARD.uuid>0) {
      toggleModal(session, "modalDeleteDASHBOARD.uuid", toggle = "close")
    }
  }
},suspended = TRUE)

output$createlistuiDASHBOARD.uuid <- renderUI({
  print("Render Policies");
  thepolicies = do.call(c,lapply(values$createdpolicy,function(x) {x$name})) # GLOBAL: values$createdpolicy
  if(length(thepolicies)>1) {
    thepolicies = thepolicies[thepolicies!="NONE"]  
  }
  selectInput("createdpoliciesDASHBOARD.uuid","Created Policies:",
              as.list(thepolicies)
              ,size=5,selectize=FALSE)
})

output$s1infoDASHBOARD.uuid <- renderUI({
  thepolicies = do.call(c,lapply(values$createdpolicy,function(x) {x$name}))
  if(length(thepolicies)>1) {
    
    
    if(!is.null(input$createdpoliciesDASHBOARD.uuid)) {
      r = values$createdpolicy[thepolicies==input$createdpoliciesDASHBOARD.uuid][[1]]
      
      tags$table(border=1,spacing=1,
                 tags$tr(
                   tags$th("Policy Assumption"),
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

# Show Policy
observerpool[["DASHBOARD.uuid"]][[3]] <<- observe({
  if(!is.null(input$s1loadpolicyDASHBOARD.uuid)) {
    if(input$s1loadpolicyDASHBOARD.uuid>0) {
      isolate({
        if (!is.null(isolate(input$createdpoliciesDASHBOARD.uuid))) {
          if(length(values$createdpolicy)>1) {
            
            if(input$createdpoliciesDASHBOARD.uuid!="NONE") {
              thepolicies = do.call(c,lapply(values$createdpolicy,function(x) {x$name}))
              if(length(thepolicies)>1) {
                sel <- isolate(values$createdpolicy[thepolicies==input$createdpoliciesDASHBOARD.uuid])
                updateSliderInput(session, "d1water", value = sel[[1]]$thewater)
                updateSliderInput(session, "d1uclf", value = sel[[1]]$thecoaluclf)
                updateSliderInput(session, "d1uclf2", value = sel[[1]]$thetxuclf)
                updateCheckboxInput(session, "withoutGrandInga", value = sel[[1]]$withoutinga)
                #updateCheckboxInput(session, "varyload", value = sel[[1]]$varyload)
                updateSliderInput(session, "d1cons", value = sel[[1]]$load)
              }
            }
            
          }
        }
      })
    }}
},suspended = TRUE)

# Delete a Policy
observerpool[["DASHBOARD.uuid"]][[4]] <<- observe({
  if(!is.null(input$s1deletepolicyYDASHBOARD.uuid)) {
    if(input$s1deletepolicyYDASHBOARD.uuid>0) {
      toggleModal(session, "modalDeleteDASHBOARD.uuid", toggle = "close")
      isolate({
        if (!is.null(isolate(input$createdpoliciesDASHBOARD.uuid))) {
          if(nchar(input$createdpoliciesDASHBOARD.uuid)>0) {
            
            if (input$createdpoliciesDASHBOARD.uuid!="NONE") {
              if(length(values$createdpolicy)>1) {
                values$createdpolicy <- isolate(values$createdpolicy[
                  do.call(c, lapply(values$createdpolicy,function(x) {x$name}))!=input$createdpoliciesDASHBOARD.uuid])
              }
            }
            
          }
        }
      });
    }
  }
  values$availablepolicy <- isolate(values$createdpolicy)
},suspended = TRUE)

output$s1downloadpolicyDASHBOARD.uuid <- downloadHandler(
  filename = function() {
    paste("UnconstraintSample", "csv", sep = ".")
  },
  content = function(file) {
    write.table(datasetInputUC(), file, sep = ",",row.names = FALSE)
  }
)
