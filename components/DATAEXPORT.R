width=230
height=95
minwidth=230
minheight=95


library(data.table)
ui= tags$span(
                  "Download Data for Slider Selection",
                  downloadButton('s1downloadpolicyDASHBOARD.uuid', 'Download')
           );



# Store all Observers here for Lifetime Management - Start all with Suspended=TRUE
observerpool[["DASHBOARD.uuid"]] <<- list()


output$s1downloadpolicyDASHBOARD.uuid <- downloadHandler(
  filename = function() {
    paste("UnconstraintSample", "csv", sep = ".")
  },
  content = function(file) {
    write.table(datasetInputUC(), file, sep = ",",row.names = FALSE)
  }
)
