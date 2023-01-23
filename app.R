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
#setwd('C:/Users/RPHC5/Documents/Coding/R Scripts/CPI')
data <- read.csv('november.csv')%>%mutate(Date=as.Date(Date))

year_range <- data %>% mutate(years = as.numeric(format(Date, format="%Y")))%>%select(years)%>%distinct()
month_range <- data %>% mutate(months = as.numeric(format(Date, format="%M")))%>%select(months)%>%distinct()
productc <- data %>% select(Products) %>% distinct()
sourcec <- data %>% select(Source) %>% distinct() 
sourcec <- as.vector(sourcec$Source) %>% as.list()



# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("CPI Data Rwanda"),
    selectInput("product", label = "Product", choices = productc, selected="GENERAL INDEX (CPI)", multiple=FALSE),
    selectInput("month", label = "Month", choices = month_range, multiple=FALSE),
    checkboxGroupInput("source", label="Source", choices=sourcec, selected = sourcec, inline=TRUE),
    sliderInput("years", "Years to show", min = min(year_range), max=max(year_range), value=c(min(year_range), max(year_range)), sep = ""),
    mainPanel(plotlyOutput("plot1")
  ))


# Define server logic required to draw a histogram
server <- function(input, output) {
    data1 <- reactive({data%>%filter(Products %in% input$product)%>%filter(Source %in% input$source)
    })
    output$plot1<- renderPlotly({plot_ly(data1(), x = ~Date, y = ~Index, type = 'scatter', mode = 'lines', color=~Source)%>%
                                  layout(title = input$product,
                                  xaxis = list(title = ""),
                                  yaxis = list (title = "Index, February 2014 = 100"))})
    }

# Run the application 
shinyApp(ui = ui, server = server)
