library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggbeeswarm)
library(ggpubr)
library(extrafontdb)
library(extrafont)
library(Cairo)
library(broom)

loadfonts()
shinyWidgetsGallery()

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Add title
    titlePanel("SuperplotR"),
    
    sidebarLayout(
        # Load data
        sidebarPanel(
            fileInput("data_raw", "Load your data set (.csv)", accept = ".csv"),
            prettyRadioButtons("geom", "Plot type", choices = c("Beeswarm", "Violin", "Boxplot"), selected = "Beeswarm"),
            materialSwitch("expand_options", "Options", status = "primary", right = TRUE),
            
            conditionalPanel(condition = "input.expand_options == 1",
                             textInput("y_label", "Label for y axis"),
                             sliderInput("plot_width", "Plot width (in)", min = 4, max = 12, value = 6, step = 0.25),
                             sliderInput("plot_height", "Plot height (in)", min = 4, max = 12, value = 6, step = 0.25),
                             conditionalPanel(condition = "input.geom == 'Beeswarm'",
                                sliderInput("cex", "Point spread", min = 1, max = 3, value = 2, step = 0.25))),
        ),
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
    theme_plot <- reactive({theme(legend.position = "none",
                   text = element_text(family = "Arial", color = "black"),
                   axis.title.x = element_blank(),
                   axis.title.y = element_text(size = 9),
                   axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5),
                   axis.text.y = element_text(size = 8, margin = unit(c(0, 2, 0, 0), "mm")),
                   plot.tag = element_text(size = 8),
                   plot.tag.position = c(0.3, 0.06),
                   axis.ticks.length.y = unit(-1, "mm"))
    })
    
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
    gg <- reactive({
        ggplot(data = data(), aes(x = condition, y = val)) +
            #    stat_pvalue_manual(data = data_signif, step.increase = 0.1) +
            #    scale_shape_manual(values = c(22, 23, 24)) +
            scale_x_discrete(name = NULL) +
            scale_y_continuous(name = input$y_label) +
            theme_classic() +
            theme_plot()
    })
    output$plot <- renderPlot({
        if(input$geom == "Beeswarm") {
       gg() + geom_beeswarm(cex = input$cex, alpha = 0.6) +
            geom_beeswarm(data = data_summary(), aes(x = condition, y = mean, color = rep, shape = rep), size = 5)
        } else if(input$geom == "Violin") {
            gg() + geom_violin(fill = "grey82") +
                geom_beeswarm(data = data_summary(), aes(x = condition, y = mean, color = rep, shape = rep), position = "jitter", size = 5)
        } else if(input$geom == "Boxplot") {
            gg() + geom_boxplot() +
                geom_beeswarm(data = data_summary(), aes(x = condition, y = mean, color = rep, shape = rep), position = "jitter", size = 5)
        }
    }, width = function() {input$plot_width * 72}, height = function() {input$plot_height * 72}, res = 72)
}


shinyApp(ui = ui, server = server)
