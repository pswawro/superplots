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
            # Choose plot type
            prettyRadioButtons("geom", "Plot type", choices = c("Beeswarm", "Violin", "Boxplot", "Barplot"), selected = "Beeswarm"),
            
            # Options
            radioGroupButtons("options", choices = c("Data", "Display", "Export"), justified = TRUE, status = "primary"),
            
            # Display data
            conditionalPanel(condition = "input.options == 'Data'",
                             # Choose data
                             fluidRow(
                                column(4, pickerInput("condition", "Condition", choices = c())),
                                column(4, pickerInput("rep", "Replicate", choices = c())),
                                column(4, pickerInput("y_value", "Value", choices = c()))),
                             # y label options
                             fluidRow(
                                 column(8, textInput("y_label", "Label for y axis")),
                                 column(4, pickerInput("y_label_size", "Size (pt)",
                                                       choices = c(1:50), select = 10, options = list(`live-search` = TRUE)))),
                             # x label options
                             fluidRow(
                                 column(8, textInput("x_label", "Label for x axis")),
                                 column(4, pickerInput("x_label_size", "Size (pt)",
                                                       choices = c(1:50), select = 10, options = list(`live-search` = TRUE))))),
            
            # Display options
            conditionalPanel(condition = "input.options == 'Display'",
                             
                             fluidRow(
                                 column(6, sliderInput("plot_width", "Plot width (in)", min = 1, max = 12, value = 4.5, step = 0.25)),
                                 column(6, sliderInput("plot_height", "Plot height (in)", min = 1, max = 12, value = 6, step = 0.25))),
                             
                             sliderInput("y_scale", "Scale y axis", min = 0, max = 1, value = c(0, 1)),
                             sliderInput("point_size_sum", "Point size (summary)", min = 1, max = 10, value = 5, step = 0.25),
                             conditionalPanel(condition = "input.geom == 'Beeswarm'",
                                              sliderInput("point_size_data", "Point size (individual data)", min = 1, max = 5, value = 2, step = 0.25),
                                              sliderInput("cex", "Point spread", min = 1, max = 3, value = 2, step = 0.25))),
            
            # Export options
            conditionalPanel(condition = "input.options == 'Export'",
                             downloadButton("plot_to_pdf", "Save plot as pdf"),
                             downloadButton("plot_to_png", "Save plot as png"))
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
    
    # Choose values to plot by
    col_names <- reactive({names(data_raw())})
    col_names_num <- reactive({
        data_raw() %>%
            select(where(is.numeric)) %>%
            names()
        })
    
    observe({
        updatePickerInput(session, "condition", choices = col_names(), selected = col_names()[1])
    })

    observe({
        updatePickerInput(session, "rep", choices = col_names(), selected = col_names()[2])
    })
    observe({
        updatePickerInput(session, "y_value", choices = col_names_num(), selected = col_names_num()[2])
    })
    
    
    # Create data summary   
    data_summary <- reactive({
        data_raw() %>%
            group_by(!!sym(input$condition), !!sym(input$rep)) %>%
            summarize(Mean = mean(!!sym(input$y_value)),
                      Median = median(!!sym(input$y_value)),
                      SEM = sd(!!sym(input$y_value)) / sqrt(length(!!sym(input$y_value))),
                      Min = min(!!sym(input$y_value)),
                      Max = max(!!sym(input$y_value)))
    })
    
    # Output raw data
    output$data_raw <- renderDataTable({
        data_raw()
    })
    
    # Output data summary
    output$summary <- renderDataTable({
        data_summary()
    })
    
    
    # Define ggplot theme
    theme_plot <- reactive({theme(legend.position = "none",
                                  text = element_text(family = "Arial", color = "black"),
                                  axis.title.x = element_text(size = input$x_label_size),
                                  axis.title.y = element_text(size = input$y_label_size),
                                  axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5),
                                  axis.text.y = element_text(size = 8, margin = unit(c(0, 2, 0, 0), "mm")),
                                  plot.tag = element_text(size = 8),
                                  plot.tag.position = c(0.3, 0.06),
                                  axis.ticks.length.y = unit(-1, "mm"))
    })
    
    # Create superplot
    gg <- reactive({
        ggplot(data = data_raw(), aes(x = !!sym(input$condition), y = !!sym(input$y_value))) +
            #    stat_pvalue_manual(data = data_signif, step.increase = 0.1) +
            scale_x_discrete(name = input$x_label) +
            scale_y_continuous(name = input$y_label, limits = c(input$y_scale[1], input$y_scale[2])) +
            theme_classic() +
            theme_plot()
    })
    
    superplot <- reactive({
        if(input$geom == "Beeswarm") {
            gg() + geom_beeswarm(cex = input$cex, alpha = 0.6, size = input$point_size_data) +
                geom_beeswarm(data = data_summary(), aes(x = !!sym(input$condition), y = Mean,
                                                         fill = factor(!!sym(input$rep)), shape = factor(!!sym(input$rep))),
                              size = input$point_size_sum) +
                scale_shape_manual(values = c(21:25))
        } else if(input$geom == "Violin") {
            gg() + geom_violin(fill = "grey82") +
                geom_beeswarm(data = data_summary(), aes(x = !!sym(input$condition), y = Mean,
                                                         fill = factor(!!sym(input$rep)), shape = factor(!!sym(input$rep))),
                              size = input$point_size_sum) +
                scale_shape_manual(values = c(21:25))
        } else if(input$geom == "Boxplot") {
            gg() + geom_boxplot() +
                geom_beeswarm(data = data_summary(), aes(x = !!sym(input$condition), y = Mean,
                                                         fill = factor(!!sym(input$rep)), shape = factor(!!sym(input$rep))),
                              size = input$point_size_sum) +
                scale_shape_manual(values = c(21:25))
        } else if(input$geom == "Barplot") {
            data_raw() %>%
                group_by(!!sym(input$condition)) %>%
                summarize(Mean = mean(!!sym(input$y_value)),
                          Median = median(!!sym(input$y_value)),
                          SEM = sd(!!sym(input$y_value)) / sqrt(length(!!sym(input$y_value))),
                          Min = min(!!sym(input$y_value)),
                          Max = max(!!sym(input$y_value))) %>%
                ggplot(aes(x = !!sym(input$condition), y = Mean)) +
                geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.25) +
                geom_col(fill = "grey56") +
                geom_beeswarm(data = data_summary(), aes(x = !!sym(input$condition), y = Mean,
                                                         fill = factor(!!sym(input$rep)), shape = factor(!!sym(input$rep))),
                              size = input$point_size_sum) +
                scale_shape_manual(values = c(21:25)) +
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
    observe(updateSliderInput(session, "y_scale", min = 0,
                              max = round(max(data_raw()[,req(input$y_value)]) * 1.1, digits = 2),
                              value = c(0, round(max(data_raw()[,req(input$y_value)]) * 1.1, digits = 2))
    ))
    
    # Export plot
    output$plot_to_pdf <- downloadHandler(
        filename = "plot.pdf",
        content = function(file) {
            ggsave(file, superplot(), width = input$plot_width, height = input$plot_height, device = cairo_pdf())
        })
    
    output$plot_to_png <- downloadHandler(
        filename = "plot.png",
        content = function(file) {
            ggsave(file, superplot(), width = input$plot_width, height = input$plot_height, device = "png", dpi = 300)
        })
    
}


shinyApp(ui = ui, server = server)