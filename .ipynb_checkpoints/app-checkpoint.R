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
    mainPanel(plotOutput("plot1")
  ))


# Define server logic required to draw a histogram
server <- function(input, output) {
    data1 <- reactive({data%>%filter(Products %in% input$product)%>%filter(Source %in% input$source)
    })
    title1 <- reactive({paste(input$product())})
    output$plot1<- renderPlot({ggplot(data1(),aes(x=Date,y=Index, colour=Source))+geom_line(linewidth=1.25)+
        scale_x_date(date_labels = "%b %y", breaks = "15 months")+
        scale_y_continuous(expand=c(0,0))+
        facet_wrap(~Products)+
        theme_classic()+
        labs(x="",
             y="")+
        theme(legend.position="bottom",
              panel.grid.major.x = element_line(linewidth = 0.1, colour="grey"),
              #panel.grid.minor.x = element_line(colour="grey20"),
              panel.grid.major.y = element_line(linewidth=0.1,colour="grey"),
              #panel.grid.minor.y = element_line(colour="grey20"),
              axis.text = element_text(size = 12),
              legend.key.size = unit(0.5, 'cm'),
              legend.text = element_text(size=12))},
        height = 600,width = 900)
    
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
