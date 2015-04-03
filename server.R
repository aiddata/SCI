source('Dependencies/dep.R', chdir=T)

shinyServer(function(input, output) {
  
  uploadShpfile <- reactive({
    if (!is.null(input$shpFile)){
      shpDF <- input$shpFile
      prevWD <- getwd()
      uploadDirectory <- dirname(shpDF$datapath[1])
      setwd(uploadDirectory)
      for (i in 1:nrow(shpDF)){
        file.rename(shpDF$datapath[i], shpDF$name[i])
      }
      shpName <- shpDF$name[grep(x=shpDF$name, pattern="*.shp")]
      shpPath <- paste(uploadDirectory, shpName, sep="/")
      setwd(prevWD)
      shpFile <- readShapePoly(shpPath)
      return(shpFile)
    } else {
      return()
    }
  })
  
  
  output$map <- renderPlot({
    if (!is.null(uploadShpfile())){
      shpFile <- uploadShpfile()
      plot(shpFile)
    }
  })
  
  output$mapT <- renderDataTable({
    if (!is.null(uploadShpfile())){
      shpFile <- uploadShpfile()
      shpFile@data
    }
    
  })
  
  output$DescDD<- renderUI({
    if (!is.null(uploadShpfile())){
      shpFile <- uploadShpfile()
      selectInput("data_sel","Select Treatment Column", names(shpFile@data))      
    }
  })
  
  output$DescCov<- renderUI({
    if (!is.null(uploadShpfile())){
      shpFile <- uploadShpfile()
      ""
      selectInput("cov_data","Select Covariate Column for Comparison", names(shpFile@data))  
    }
  })
  
  output$DescTrt<- renderPlot({
    if (!is.null(input$data_sel)){
      shpFile <- uploadShpfile()
      Treatment <- eval(parse(text=paste("shpFile@data$",input$data_sel,sep="")))
      Treatment <- Treatment[!is.na(Treatment)]
      bins <- seq(min(Treatment),max(Treatment),length.out=input$bins + 1)
      #clr <- ifelse((Treatment < max(input$rng_Trt)) & (Treatment > min(input$rng_Trt)), "grey", "red")
      clr <- "skyblue"
      warning(min(input$rng_Trt))
      warning(max(input$rng_Trt))
      hist(Treatment,breaks = bins, col=clr, border="white")
      #Add two vertical lines to show max and min values.
      abline(v=max(input$rng_Trt), col="red")
      abline(v=min(input$rng_Trt), col="blue")
    }
  })
  
  output$RngTrt<- renderUI({
    if (!is.null(input$data_sel)){
      shpFile <- uploadShpfile()
      Treatment <- eval(parse(text=paste("shpFile@data$",input$data_sel,sep="")))
      Treatment <- Treatment[!is.na(Treatment)]
      sliderInput("rng_Trt", "Treatment = 1 Range:", min=min(Treatment), max=max(Treatment), value=c(min(Treatment),max(Treatment)))  
    }
  })
    
  

})