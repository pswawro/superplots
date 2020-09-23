library(shiny)
library(tidyverse)
library(ggbeeswarm)
library(ggpubr)
library(extrafontdb)
library(extrafont)
library(Cairo)
library(broom)

loadfonts()

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Add title
    titlePanel("SuperplotR"),
    
    sidebarLayout(
        # Load data
        sidebarPanel(
            fileInput("data_raw", "Load your data set (.csv)", accept = ".csv"),
            textInput("y_label", "Label for y axis"),
            actionButton("update", "Plot")),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", plotOutput("plot")),
                tabPanel("Raw Data", dataTableOutput("data_raw")),
                tabPanel("Summary", dataTableOutput("summary"))
                )
            )
        )
    )

server <- function(input, output) {
    
    # Read csv
    data_raw <- reactive({
        read_csv(req(input$data_raw$datapath))
    })
    
    # Rename columns
    data <- reactive({
        data_raw() %>%
        set_names(c("condition", "rep", "val")) %>%
        mutate(rep = factor(rep))
    })
    
    # Create data summary   
    data_summary <- reactive({
        data() %>%
        group_by(condition, rep) %>%
        summarize(mean = mean(val),
                  median = median(val),
                  sem = sd(val) / sqrt(length(val)),
                  min = min(val),
                  max = max(val))
    })
    
    # Define ggplot theme
    theme <- theme(legend.position = "none",
                   text = element_text(family = "Arial", color = "black"),
                   axis.title.x = element_blank(),
                   axis.title.y = element_text(size = 9),
                   axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5),
                   axis.text.y = element_text(size = 8, margin = unit(c(0, 2, 0, 0), "mm")),
                   plot.tag = element_text(size = 8),
                   plot.tag.position = c(0.3, 0.06),
                   axis.ticks.length.y = unit(-1, "mm"))
    
    # Output raw data
    output$data_raw <- renderDataTable({
        data_raw()
        })
    
    # Output data summary
    output$summary <- renderDataTable({
        data_summary() %>%
            set_names(c("Condition", "Replicate", "Mean", "Median", "SEM", "Min", "Max"))
    })
    
        
    # Create superplot
    gg <- eventReactive(input$update, {
        ggplot(data = data()) +
        geom_beeswarm(aes(x = condition, y = val)) +
        geom_point(data = data_summary(), aes(x = condition, y = mean, color = rep, shape = rep), size = 5) +
   #    stat_pvalue_manual(data = data_signif, step.increase = 0.1) +
   #    scale_shape_manual(values = c(22, 23, 24)) +
        scale_x_discrete(name = NULL) +
        scale_y_continuous(name = input$y_label) +
        theme_classic() +
        theme
        })
        
    output$plot <- renderPlot({
        gg()
    })
}


shinyApp(ui = ui, server = server)
