dashboard = fluidPage(
  fluidRow(
    #column(3,tags$h5("STEP 1 - EVALUATE/CREATE POLICIES"),
    #       bsButton("s1s2","Next (Step2) >>",style="primary")
    #),
    column(3,sliderInput("d1water", "Water Availability % (Assumption)", 
                         min(unique(runMasterdata[runMasterdata$policy_id==14,]$water.availability))*100, 
                         max(unique(runMasterdata[runMasterdata$policy_id==14,]$water.availability))*100, 
                         mean(unique(runMasterdata[runMasterdata$policy_id==14,]$water.availability))*100,
                         10,
                         animate=FALSE,ticks=FALSE,width = "100%")),
    column(3,sliderInput("d1uclf", "Coal UCLF % (Assumption)", 
                         min(unique(runMasterdata[runMasterdata$policy_id==14,]$coal.uclf))*100, 
                         max(unique(runMasterdata[runMasterdata$policy_id==14,]$coal.uclf))*100, 
                         max(unique(runMasterdata[runMasterdata$policy_id==14,]$coal.uclf))*100,
                         10,
                         animate=FALSE,ticks=FALSE)),
    column(3,
           sliderInput("d1uclf2", "Transmission UCLF % (Assumption)", 
                       min(unique(runMasterdata[runMasterdata$policy_id==14,]$transmission.uclf))*100, 
                       max(unique(runMasterdata[runMasterdata$policy_id==14,]$transmission.uclf))*100, 
                       max(unique(runMasterdata[runMasterdata$policy_id==14,]$transmission.uclf))*100,
                       10,
                       animate=FALSE,ticks=FALSE) 
    ),
    column(1,checkboxInput("withoutGrandInga", "Without Grand Inga", FALSE)), 
    column(2,
           sliderInput("d1cons", "Consumption % Adjustment", 
                       min(unique(runMasterdata[runMasterdata$policy_id==14,]$consumption.adjustment))*100, 
                       max(unique(runMasterdata[runMasterdata$policy_id==14,]$consumption.adjustment))*100, 
                       mean(unique(runMasterdata[runMasterdata$policy_id==14,]$consumption.adjustment))*100,
                       10,
                       animate=FALSE,ticks=FALSE)) 
  ),
 
fluidRow(
  
#    box(width=3,
#          textInput("s1policyname", "Create New Policy Name for Assuptions:"),
#          bsAlert("s1alert"),
#          bsButton("s1createpolicy","Create Policy",style="primary"),
#          uiOutput("createlistui"),
#          fluidRow(
#            column(12,uiOutput("s1info"))
#          ),
#          fluidRow(
#            column(12,
#                   tags$h5("For Selected Policy:"), 
#                 bsButton("s1deletepolicy","Delete",style="warning"),
#                 bsButton("s1loadpolicy","Show >",style="info"),
#                 #bsButton("s1downloadpolicy","Download",style="info"),
#                 bsModal("modalDelete", "Delete Policy: Are you sure?", "s1deletepolicy", size = "small",
#                         bsButton("s1deletepolicyY","Yes - Delete",style="warning"),
#                         bsButton("s1deletepolicyN","No - Cancel",style="info"))
#            )
#          ),
#          tags$br(),
#          tags$h5("Help: Create policies based on assuptions by changing the slider positions (Generation Expantion Plan runs UNCONSTRAINT), then when happy enter a 'Policy Name' and then click 'Create Policy'."),
#          tags$h5("Note: Create at least 2 Policies before proceeding to STEP2, Policy names must be at least 6 Characters long."),
#          downloadButton('s1downloadpolicy', 'Download (for Slider Selection)')
#          
#   ),
  column(width=12,
         # MAgic Dashboard
         uiOutput("dashboarditems")
         
         
  )# col
))
