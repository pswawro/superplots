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

#### UI ####

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
            radioGroupButtons("options", choices = c("Data", "Signif", "Display", "Export"), justified = TRUE, status = "primary"),
            
            # Display data
            conditionalPanel(condition = "input.options == 'Data'",
                             # Choose data
                             fluidRow(
                                 column(4, pickerInput("condition", "Condition", choices = c())),
                                 column(4, pickerInput("rep", "Replicate", choices = c())),
                                 column(4, pickerInput("y_value", "Value", choices = c()))),
                             awesomeRadio("sum","Summary value to plot", choices = c("Mean", "Median"), selected = "Mean",
                                          inline = TRUE, checkbox = TRUE),
                             # Pick control
                             pickerInput("ref", "Choose control group", choices = c()),
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
            
            # Display significance test
            conditionalPanel(condition = "input.options == 'Signif'",
                             awesomeRadio("test","Choose significance test", choices = c("t.test", "Anova"), selected = "Anova",
                                          inline = TRUE, checkbox = TRUE),
                             prettySwitch("show_signif", "Show p value", value = TRUE, status = "primary"),
                             sliderInput("signif_position", "Move p value brackets", min = 0, max = 1, value = 1)),
            
            # Display options
            conditionalPanel(condition = "input.options == 'Display'",
                             
                             prettySwitch("show_summary", "Show replicate mean/median", value = FALSE, status = "primary"),
                             
                             fluidRow(
                                 column(6, sliderInput("plot_width", "Plot width (in)", min = 1, max = 12, value = 4.5, step = 0.25)),
                                 column(6, sliderInput("plot_height", "Plot height (in)", min = 1, max = 12, value = 6, step = 0.25))),
                             
                             sliderInput("y_scale", "Scale y axis", min = 0, max = 1, value = c(0, 1)),
                             sliderInput("point_size_sum", "Point size (summary)", min = 1, max = 10, value = 5, step = 0.25),
                             conditionalPanel(condition = "input.geom == 'Beeswarm'",
                                              sliderInput("point_size_data", "Point size (individual data)", min = 0.5, max = 5, value = 1.5, step = 0.25),
                                              sliderInput("cex", "Point spread", min = 1, max = 3, value = 1.5, step = 0.25))),
            
            # Export options
            conditionalPanel(condition = "input.options == 'Export'",
                             downloadButton("plot_to_pdf", "Save plot as pdf"),
                             downloadButton("plot_to_png", "Save plot as png"))
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", plotOutput("plot")),
                tabPanel("Raw Data", dataTableOutput("data")),
                tabPanel("Summary", dataTableOutput("summary")),
                tabPanel("Significance test", dataTableOutput("signif"))
            )
        )
    )
)

#### Server ####

server <- function(input, output, session) {
    # Read csv
    data_raw <- reactive({
        req(input$data_raw)
        read_csv(input$data_raw$datapath)
    })
    
    # Choose values to plot by
    observeEvent(data_raw(), {
        
        col_names <- names(data_raw())
        col_names_num <- data_raw() %>%
            select(where(is.numeric)) %>%
            names()
        
        updatePickerInput(session, "condition", choices = col_names, selected = col_names[1])
        
        updatePickerInput(session, "rep", choices = col_names, selected = col_names[2])
        
        updatePickerInput(session, "y_value", choices = col_names_num, selected = col_names_num[1])
    })
    
    # Update control input
    observe({
        req(data_raw(), input$condition)
        updatePickerInput(session, "ref", choices = unique(data_raw()[[input$condition]]))
    })
    
    # Final data
    data_fin <- eventReactive(list(data_raw(), input$ref, input$rep, input$y_value), {
        req(input$ref, input$rep, input$y_value)
        
        nlevels <- unique(data_raw()[[input$condition]])
        ref_level <- input$ref
        
        print(ref_level)
        data_raw() %>%
            mutate(Condition = !!sym(isolate(input$condition)),
                   Condition = factor(Condition, levels = nlevels, ordered = FALSE),
                   Condition = fct_relevel(Condition, ref_level),
                   Rep = !!sym(input$rep),
                   Rep = factor(Rep),
                   Value = as.numeric(!!sym(input$y_value))) %>%
            filter(!is.na(Value)) %>%
            select(Condition, Rep, Value)
    })
    
    
    # Create data summary   
    data_summary <- reactive({
        req(data_fin())
        data_fin() %>%
            group_by(Condition, Rep) %>%
            summarize(Mean = mean(Value, na.rm = TRUE),
                      Median = median(Value, na.rm = TRUE),
                      SEM = sd(Value, na.rm = TRUE) / sqrt(length(Value)),
                      Min = min(Value, na.rm = TRUE),
                      Max = max(Value, na.rm = TRUE)) %>%
            ungroup() %>%
            select(Condition, Rep, Mean, Median, SEM, Min, Max)
    })
    
    
    # Output raw data
    output$data <- renderDataTable({
        req(data_fin())
        data_fin()
    })
    
    
    # Output data summary
    output$summary <- renderDataTable({
        req(data_summary())
        data_summary()
    })
    
    
    
    # Calculate significance
    data_for_signif <- reactive({
        req(input$ref)
        
        ref_signif <- gsub('-', '**subformin**', input$ref, fixed = TRUE)
        
        data_summary() %>%
            mutate(Condition = gsub('-', '**subformin**', Condition, fixed = TRUE),
                   Condition = fct_relevel(Condition, ref_signif))
    })
    
    data_signif <- eventReactive(list(input$ref, data_fin(), data_for_signif(), input$sum, input$signif_position), {
        
        if(input$test == "t.test") {
            validate(need(
                length(unique(data_fin()$Condition)) == 2,
                message = "To perform t.test, the number of conditions must be 2. For 3 conditions and more please choose Anova."))
            
            t.test(as.formula(paste(input$sum, "~", "Condition")), data_for_signif(), paired = TRUE) %>%
                tidy() %>%
                transmute(group1 = unique(Condition)[2],
                          group2 = unique(Condition)[1],
                          p = signif(p.value, 3),
                          y.position = input$signif_position)
        } else {
            TukeyHSD(aov(as.formula(paste(input$sum, "~", "Condition")), data = data_for_signif())) %>%
                tidy() %>%
                separate(contrast, into = c("group1", "group2"), sep = "-", extra = "merge") %>%
                transmute(group1 = gsub('**subformin**', '-', group1, fixed = TRUE),
                          group2 = gsub('**subformin**', '-', group2, fixed = TRUE),
                          p = signif(adj.p.value, 3),
                          y.position =  input$signif_position) %>%
                filter(group2 == input$ref)
        }
    })
    
    output$signif <- renderDataTable({
        req(input$data_raw)
        data_signif() %>%
            select(Control = group2, Compare_group = group1, p.value = p)
    })
    
    
    # Define ggplot theme
    theme_plot <- reactive({
        req(input$x_label_size, input$y_label_size)
        theme(legend.position = "none",
              text = element_text(family = "Arial", color = "black"),
              axis.title.x = element_text(size = input$x_label_size),
              axis.title.y = element_text(size = input$y_label_size),
              axis.text.x = element_text(size = 10, angle = 45, vjust = 0.95, hjust=0.95),
              axis.text.y = element_text(size = 8, margin = unit(c(0, 2, 0, 0), "mm")),
              plot.tag = element_text(size = 8),
              plot.tag.position = c(0.3, 0.06),
              axis.ticks.length.y = unit(-1, "mm"))
    })
    
    # Create superplot
    ## Superplot without p value
    superplot <- reactive({
        
        req(data_fin(), input$cex, input$ref, input$point_size_data, input$sum, input$point_size_sum, input$y_scale, input$geom)
        
        if(input$geom %in% c("Beeswarm", "Violin", "Boxplot")) {
            
            data_fin() %>%
                ggplot(aes(x = Condition, y = Value)) +
                {if(input$geom == "Beeswarm") geom_beeswarm(cex = input$cex, alpha = 0.6, size = input$point_size_data, groupOnX = TRUE)} +
                {if(input$geom == "Violin") geom_violin(fill = "grey82")} +
                {if(input$geom == "Boxplot") geom_boxplot()} +
                {if(input$show_summary) geom_beeswarm(data = data_summary(), aes(x = Condition, y = !!sym(input$sum),
                                                                                      fill = Rep, shape = Rep),
                                                           size = input$point_size_sum, groupOnX = TRUE)} +
                {if(input$show_signif) stat_pvalue_manual(data = data_signif(), step.increase = 0.15, size = 2.5)} +
                scale_shape_manual(values = c(21:25, 1:20)) +
                scale_x_discrete(name = NULL) +
                scale_y_continuous(name = input$y_label, limits = c(input$y_scale[1], input$y_scale[2])) +
                theme_classic() +
                theme_plot()
            
        } else if(input$geom == "Barplot") {
            data_fin() %>%
                group_by(Condition) %>%
                summarize(Mean = mean(Value),
                          Median = median(Value),
                          SEM = sd(Value) / sqrt(length(Value)),
                          Min = min(Value),
                          Max = max(Value)) %>%
                ggplot(aes(x = Condition, y = !!sym(input$sum))) +
                geom_errorbar(aes(ymin = !!sym(input$sum) - SEM, ymax = !!sym(input$sum) + SEM), width = 0.25) +
                geom_col(fill = "grey56") +
                {if(input$show_summary) geom_beeswarm(data = data_summary(), aes(x = Condition, y = !!sym(input$sum),
                                                                                 fill = Rep, shape = Rep),
                                                      size = input$point_size_sum, groupOnX = TRUE)} +
                {if(input$show_signif) stat_pvalue_manual(data = data_signif(), step.increase = 0.15)} +
                scale_shape_manual(values = c(21:25, 1:20)) +
                scale_x_discrete(name = NULL) +
                scale_y_continuous(name = input$y_label, limits = c(input$y_scale[1], input$y_scale[2])) +
                theme_classic() +
                theme_plot()
        }
    })
    
    
    # Superplot output
    output$plot <- renderPlot({
        req(superplot())
        
        superplot()
        
    }, width = function() {input$plot_width * 72}, height = function() {input$plot_height * 72}, res = 72)
    
    
    # Create rescaling sliders
    observeEvent(input$y_value, {
        req(data_raw(), input$y_value)
        updateSliderInput(session, "y_scale", min = 0,
                          max = round(max(data_raw()[[input$y_value]], na.rm = TRUE) * 1.5, digits = 2),
                          value = c(0, round(max(data_raw()[[input$y_value]], na.rm = TRUE) * 1.25, digits = 2)))
        
        # Update p value position brackets
        updateSliderInput(session, "signif_position", min = 0,
                          max = round(max(data_raw()[[input$y_value]], na.rm = TRUE) * 1.5, digits = 2),
                          value = round(max(data_raw()[[input$y_value]], na.rm = TRUE), digits = 2))
    })
    
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