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
#setwd('C:/Users/RPHC5/Documents/Coding/R Scripts/CPI')
data <- read.csv('november.csv')%>%mutate(Date=as.Date(Date), year = year(Date), month = month(Date, label=TRUE, abbr=FALSE))

year_range <- data %>% select(year) %>% distinct()
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
    
    output$plot1<- renderPlotly({plot_ly(data1(), x = ~Date, y = ~Index, type = 'scatter', mode = 'lines', color=~Source)%>%
                                  layout(title = input$product,
                                  xaxis = list(title = ""),
                                  yaxis = list (title = "Index, February 2014 = 100"))})
    }

# Run the application 
shinyApp(ui = ui, server = server)
