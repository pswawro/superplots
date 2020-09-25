library(shiny)
library(shinyWidgets)
library(colourpicker)
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
            prettyRadioButtons("geom", "Plot type", choices = c("Beeswarm", "Violin", "Boxplot", "Barplot"), selected = "Beeswarm"),
            radioGroupButtons("options", choices = c("Data", "Display", "Export"), status = "primary", justified = TRUE),
            
            
            conditionalPanel(condition = "input.options == 'Display'",
                             textInput("y_label", "Label for y axis"),
                             fluidRow(
                                 column(6, sliderInput("plot_width", "Plot width (in)", min = 1, max = 12, value = 4.5, step = 0.25)),
                                 column(6, sliderInput("plot_height", "Plot height (in)", min = 1, max = 12, value = 6, step = 0.25))),
                             fluidRow(
                                 column(6, sliderInput("data_size", "Resize data points", min = 1, max = 4, value = 2.5, step = 0.25)),
                                 column(6, sliderInput("mean_size", "Resize mean points", min = 1, max = 6, value = 5, step = 0.25))),
                             sliderInput("y_scale", "Scale y axis", min = 0, max = 1, value = c(0, 1)),
                             conditionalPanel(condition = "input.geom == 'Beeswarm'",
                                sliderInput("cex", "Point spread", min = 1, max = 3, value = 2, step = 0.25))),
            conditionalPanel(condition = "input.options == 'Export'",
                            downloadButton("plot_to_pdf", "Save plot as pdf"))
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

server <- function(input, output, session) {
    
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
            scale_y_continuous(name = input$y_label, limits = c(input$y_scale[1], input$y_scale[2])) +
            theme_classic() +
            theme_plot()
    })
        
    superplot <- reactive({
        if(input$geom == "Beeswarm") {
        gg() + geom_beeswarm(cex = input$cex, alpha = 0.5, size = input$data_size) +
            geom_beeswarm(data = data_summary(), aes(x = condition, y = mean, fill = rep, shape = rep), size = input$mean_size) +
                scale_shape_manual(values = c(21, 22, 23, 24, 25))
    } else if(input$geom == "Violin") {
        gg() + geom_violin(fill = "grey82") +
            geom_beeswarm(data = data_summary(), aes(x = condition, y = mean, fill = rep, shape = rep), size = input$mean_size) +
            scale_shape_manual(values = c(21, 22, 23, 24, 25))
    } else if(input$geom == "Boxplot") {
        gg() + geom_boxplot() +
            geom_beeswarm(data = data_summary(), aes(x = condition, y = mean, fill = rep, shape = rep), size = input$mean_size) +
            scale_shape_manual(values = c(21, 22, 23, 24, 25))
    } else if(input$geom == "Barplot") {
        data() %>%
            group_by(condition) %>%
            summarize(mean = mean(val),
                      median = median(val),
                      sem = sd(val) / sqrt(length(val)),
                      min = min(val),
                      max = max(val)) %>%
            ggplot(aes(x = condition, y = mean)) +
                geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.25) +
                geom_col(fill = "grey56") +
                geom_beeswarm(data = data_summary(), aes(x = condition, y = mean, fill = rep, shape = rep), size = input$mean_size) +
                scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
                scale_x_discrete(name = NULL) +
                scale_y_continuous(name = input$y_label, limits = c(input$y_scale[1], input$y_scale[2])) +
                theme_classic() +
                theme_plot()
    }
    })
    
    # Superplot output
    output$plot <- renderPlot({
       superplot()
    }, width = function() {input$plot_width * 72}, height = function() {input$plot_height * 72}, res = 72)
    
    # Create rescaling sliders
    observe(updateSliderInput(session, "y_scale", min = 0, max = round(max(data()$val) * 1.1, digits = 2),
                              value = c(0, round(max(data()$val) * 1.1, digits = 2))
                              ))
    
    # Export plot
    output$plot_to_pdf <- downloadHandler(
        filename = paste(input$raw_data$name, ".pdf", sep = ""),
        content = function(file) {
            ggsave(file, superplot(), width = input$plot_width, height = input$plot_height, device = cairo_pdf())
        })
        
}


shinyApp(ui = ui, server = server)
