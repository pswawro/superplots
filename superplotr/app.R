library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("SuperplotR"),
    sidebarLayout(
        sidebarPanel(
            fileInput("data", "Load your data set (.csv)", accept = ".csv")),
        mainPanel(
            tabsetPanel(
                tabPanel("Raw Data", dataTableOutput("data_csv")))
)))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$data_csv <- renderDataTable({
        data_csv <- read_csv(req(input$data$datapath))
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
