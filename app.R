## CPI/Inflation Dashboard App

##Import packages, including functions to work with CPI/Inflation data and functions to generate UI elements
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
source('Scripts/CPIfunctions.R')
source('Scripts/InputFunctions.R')
source('Scripts/InflationFunctions.R')

#Import data to be used in dashboard

#Latest file can be found at following link https://statistics.gov.rw/statistical-publications/subject/consumer-price-index-%28cpi%29

filename = 'Data/CPI_time_series_December_2022.xls'
CPIdata <- cpi_data(filename)
INFdata <- inflation_data(filename)

# Define User Interface for application - this has two tabs which each display a plot and a common header
ui <- fixedPage(
    #Title + Subtitle
  
    titlePanel(h4(titleText)),
    tags$h5(subtitleText),
    
    #NISR logo 
    
    tags$div(style="position:absolute; right:150px; top:10px;", nisrLogo),
    
    #Tab panel
    
    tabsetPanel(
      
      #Tab panel 1 - CPI 
      tabPanel("CPI", 
               mainPanel(
                 fixedRow(
                   #Descriptive text
                   column(6,
                          introTextCPI
                   ),
                   #Right hand side inputs
                   column(6,
                          productInput(CPIdata),
                          monthInput(CPIdata),
                          sourceInput(CPIdata),
                          align="center"
                   )
                 ),
                 fixedRow(
                   #Year slider
                   column(12, yearInput(CPIdata))
                 ),
                 fixedRow(
                   #Plot for CPI data
                   column(12,plotlyOutput("CPIplot"))
                 ), width=12)
    ),
    
    #Tab panel 2 - Inflation
    tabPanel("Inflation", 
             mainPanel(
               fixedRow(
                 #Descriptive text
                 column(6,
                        introTextINF
                 ),
                 #Right hand side inputs
                 column(6,
                        productInput(INFdata),
                        monthInput(INFdata),
                        sourceInput(INFdata),
                        align="centre"
                 )
               ),
               fixedRow(
                 #Year slider
                 column(12, yearInput(INFdata))
               ),
               fixedRow(
                 #Plot for Inflation data
                 column(12,plotlyOutput("INFplot"))
               ), width=12)
    )
  ))


# Define server logic for two plots
server <- function(input, output) {
    #CPI data cleaning
    CPIdata_clean <- reactive({cpi_clean(CPIdata, input$CPImonth, input$CPIyears, input$CPIsource, input$CPIproduct)})
    #Inflation data cleaning
    INFdata_clean <- reactive({inflation_clean(INFdata, input$INFmonth, input$INFyears, input$INFsource, input$INFproduct)})
    #CPI plot
    output$CPIplot<- renderPlotly({cpi_plot(CPIdata_clean())})
    #Inflation plot
    output$INFplot <- renderPlotly({inflation_plot(INFdata_clean())})
    }

# Run the application 
shinyApp(ui = ui, server = server)
