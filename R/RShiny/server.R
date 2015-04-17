source('Dependencies/dep.R', chdir=T)

options(shiny.maxRequestSize = 200*1024^2)

shinyServer(function(input, output) {
  
  source("server_UploadStart.R", local=TRUE)

  source("server_Map.R", local=TRUE)
  
  source("server_dataTable.R", local=TRUE)
  
  source("server_descriptives.R", local=TRUE)
 
    
  

})