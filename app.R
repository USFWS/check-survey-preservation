library(jsonlite)
library(httr)
library(stringr)
library(shiny)
library(shinycssloaders)
source("functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Monitoring Data Asset Preservation"),
    titlePanel(h4(em("For All 'Current' PRIMR Surveys"))),
    mainPanel(
      br(),
      h3("Please Select a Region Below:"),
      selectInput(inputId = "regionDropdown",label = NULL,c("",1:8),selected = "",multiple = FALSE),
      h3("Please Select a Refuge Below:"),
      selectInput(inputId = "refugeDropdown",label = NULL,c(""), selected = "", multiple = FALSE),
      withSpinner(htmlOutput("report"), type=4)
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    observeEvent(input$regionDropdown,{
      updateSelectInput(session, "refugeDropdown", choices=c("",getRefugeList(input$regionDropdown)), selected = "")
    })
    
    output$report <- renderText({
      getReport(getCCC(input$refugeDropdown))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
