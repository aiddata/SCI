library(shiny)

shinyUI(fluidPage(
  sidebarPanel(
    conditionalPanel(
      condition="input.conditionedPanels == 'Start & Upload Data'",
      fileInput(inputId="shpFile", label="Shp", multiple=TRUE),
      uiOutput("DescDD"),
      radioButtons("TrtBin", "Treatment Type:",
                   c("Manual Binary" = 1,
                     "Auto Binary (Off)" = 2,
                     "Continious (Off)" = 3)),
      conditionalPanel(
        condition="input.TrtBin == 1",
        sliderInput("bins", "Number of bins:", min=1, max=50, value=25),
        uiOutput("RngTrt"),
        uiOutput("DescSelAnc"),
        uiOutput("DescSelOut")  
      ),
    conditionalPanel(
      condition="input.conditionedPanels == 'Data Table'",
      "Data Table Options"  
    ),
    conditionalPanel(
      condition="input.conditionedPanels == 'Map'",
      "Map Options"  
    ),
    conditionalPanel(
      condition="input.conditionedPanels == 'Descriptives'",
      "Descriptive Options"
      )
      
    )
    ),
    
    mainPanel(
      navbarPage
      ("C-SAT",
       tabPanel(
         "Start & Upload Data",
         fluidRow(
           column(5,
                  conditionalPanel(
                    condition="input.TrtBin == 1",
                    plotOutput("DescTrtHist")
                  ))                   
         ),
         fluidRow(
           column(5,
                  conditionalPanel(
                    condition="input.TrtBin == 1",
                    plotOutput("DescTreatment")
             ))
         )
       ),
        tabPanel(
          "Data Table",
          conditionalPanel(
            condition="input.TrtBin == 1",
            dataTableOutput("dataT")
          )
        ),
        tabPanel(
          "Map",
          plotOutput("map")
        ),
        tabPanel(
          "Descriptives"
          ),
       id="conditionedPanels"
       
      )
  
      )
      
    )
  )
