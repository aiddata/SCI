library(shiny)

shinyUI(fluidPage(
  sidebarPanel(
    conditionalPanel(
      condition="input.conditionedPanels == 'Start & Upload Data'",
      fileInput(inputId="shpFile", label="Shp", multiple=TRUE)     
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
      uiOutput("DescDD"),
      radioButtons("TrtBin", "Binary Option:",
                   c("Manual" = 1,
                     "Auto" = 2)),
      conditionalPanel(
        condition="input.TrtBin == 1",
        sliderInput("bins", "Number of bins:", min=1, max=50, value=25),
        uiOutput("RngTrt"),
        uiOutput("DescCov"),
        selectInput("cov_data_type","Covariate Data Type", c("Timeseries","Factor","Binary","Continious")) 
      )
      
    )
    ),
    
    mainPanel(
      navbarPage
      ("C-SAT",
       tabPanel(
         "Start & Upload Data",
        "Helpful instructions on what to do."
       ),
        tabPanel(
          "Data Table",
          dataTableOutput("mapT")
        ),
        tabPanel(
          "Map",
          plotOutput("map")
        ),
        tabPanel(
          "Descriptives",
          fluidRow(
            column(5,
          conditionalPanel(
            condition="input.TrtBin == 1",
          plotOutput("DescTrt")
          )),
          column(6,
                 "test")
          )
          ),
       id="conditionedPanels"
       
      )
  
      )
      
    )
  )
