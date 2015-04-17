#Functions for the "Data" Tab
#------------------------------
#------------------------------
#Would be good to eventually move this to the "Start" page, in a small window.
output$dataT <- renderDataTable({
  if (!is.null(input$data_sel)){
    DT::datatable(uploadShpfile()@data) 
  }
})