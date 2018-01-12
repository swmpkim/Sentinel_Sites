# this app makes simple histograms of pin and MH data
# based on interactive selection of an individual SET

library(shiny)
library(tidyverse)


# load files
load("SET_data_files.R")



# user interface
ui <- fluidPage(
    selectInput(inputId = "SET", 
                label = "Choose an individual SET", 
                choices = c("CLMAJ-1", "CLMAJ-2", "CLMAJ-3",
                            "PANNE-1", "PANNE-2", "PANNE-3",
                            "JURO-1", "JURO-2", "JURO-3",
                            "JURO-4", "JURO-5", "JURO-6",
                            "SPAL-1", "SPAL-2", "SPAL-3"),
                selected = "JURO-1",
                multiple = FALSE),
    plotOutput("pinhist"),
    plotOutput("pinbox"),
    plotOutput("pintimeseries", click="plot_hover"),
    verbatimTextOutput("pinsumm"),
    plotOutput("mhhist"),
    plotOutput("mhbox"),
    verbatimTextOutput("mhsumm")

)




# server instructions
server <- function(input, output) {

    # reactive datasets
    pinreact <- reactive({
        sets_long %>%
            ungroup() %>%
            filter(SET == as.character(input$SET)) 
        
    })
    
    mhreact <- reactive({
        markers_long %>%
            ungroup() %>%
            filter(SET == as.character(input$SET))
    })
    
    # output for pins
    output$pinhist <- renderPlot({
        hist(pinreact()$value,
             col = "slategray3",
             main = paste0("Histogram of pin readings for ", input$SET),
             xlab = "value",
             ylab = "frequency")
        
    })
    
    output$pinbox <- renderPlot({
        boxplot(pinreact()$value,
                col = "lightgray",
                horizontal = TRUE,
                main = paste0("Boxplot of pin readings for ", input$SET),
                xlab = "value")
    })
    
    output$pinsumm <- renderPrint(summary(pinreact()$value))
    
    output$pintimeseries <- renderPlot({
        ggplot(pinreact(), aes(x=date, y=value)) +
            geom_point(aes(col = pin), aes=0.6) +
            facet_grid(arm~.) +
            theme_bw()
        
    })
    
    
    # output for markers
    output$mhhist <- renderPlot({
        hist(mhreact()$value,
             col = "slategray3",
             main = paste0("Histogram of marker horizon readings for ", input$SET),
             xlab = "value",
             ylab = "frequency")
        
    })
    
    output$mhbox <- renderPlot({
        boxplot(mhreact()$value,
                col = "lightgray",
                horizontal = TRUE,
                main = paste0("Boxplot of marker horizon readings for ", input$SET),
                xlab = "value")
    })
    
    output$mhsumm <- renderPrint(summary(mhreact()$value))
}

shinyApp(ui = ui, server = server)