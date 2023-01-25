#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list=ls())

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
source('Scripts/CPIfunctions.R')

filename = 'Data/CPI_time_series_November_2022.xls'
data <- cpi_data(filename)
year_range <- data %>% select(Year) %>% distinct()
month_range <- c("No month selected", month.name)
productc <- data %>% select(Products) %>% distinct()
sourcec <- data %>% select(Source) %>% distinct() 
sourcec <- as.vector(sourcec$Source) %>% as.list()



# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("CPI Data Rwanda"),
    sidebarPanel(
    selectInput("product", label = "Product", choices = productc , multiple=FALSE),
    selectInput("month", label = "Month", choices = month_range, multiple=FALSE, selected="No month selected"),
    checkboxGroupInput("source", label="Source", choices=sourcec, selected = sourcec, inline=TRUE),
    sliderInput("years", "Years to show", min = min(year_range), max=max(year_range), value=c(min(year_range), max(year_range)), sep = ""),
    width = 3),
    mainPanel(plotlyOutput("plot1")
              
  ))


# Define server logic required to draw a histogram
server <- function(input, output) {
    data1 <- reactive({cpi_clean(data, input$month, input$years, input$source, input$product)
    })
    
    output$plot1<- renderPlotly({cpi_plot(data1())})
    }

# Run the application 
shinyApp(ui = ui, server = server)
