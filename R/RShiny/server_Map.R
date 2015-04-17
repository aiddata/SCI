#Functions for the "Map" Tab
#------------------------------
#------------------------------
output$map <- renderPlot({
  if (!is.null(uploadShpfile())){
    shpFile <- uploadShpfile()
    plot(shpFile)
  }
})