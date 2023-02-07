library(tidyverse)

#Functions and variables for use in the dashboard ui - including text elements and input elements

#Dashboard title

titleText <- tags$div("Rwanda Consumer Price Index / Inflation Dashboard")

#Dashboard subtitle

subtitleText <- tags$div("Source: National Institute of Statistics of Rwanda")

#NISR logo

nisrLogo <- img(src="NISRlogo.jpg", height = "90px", width="200px")

#CPI description text

introTextCPI <-  tags$div(tags$br(), "This dashboard presents the trend of the percentage change in CPI.", tags$br(), tags$br(), 
  "1. Use Select Item to see the inflation rate for a specific product", tags$br(), tags$br(),
  "2. Use Select Month to visualise monthly or yearly trends for each item", tags$br(), tags$br(),
  "3. Use select Location to visualise different regions of Rwanda")

#Inflation description text

introTextINF <- tags$div(tags$br(), "This dashboard presents the trend of Consumer Price Index (CPI).", tags$br(), tags$br(), 
                         "1. Use Select Item to see the CPI for a specific product", tags$br(), tags$br(),
                         "2. Use Select Month to visualise monthly or yearly trends for each item", tags$br(), tags$br(),
                         "3. Use select Location to visualise different regions of Rwanda")

#Function to generate the product input selector - the type generates the input name for use on the server

productInput <- function(data) {
  product <- data %>% select(Product) %>% distinct()
  type <- data %>% select(Type) %>% distinct()
  selectInput(paste(type,"product", sep=""), label = "Select Item", choices = product , multiple=FALSE)
}

#Function to generate the month input selector - the type generates the input name for use on the server

monthInput <- function(data) {
  month_range <- c("All", month.name)
  type <- data %>% select(Type) %>% distinct()
  selectInput(paste(type,"month", sep=""), label = "Select Month", choices = month_range, multiple=FALSE, selected="All")
}

#Function to generate the location input checkbox - the type generates the input name for use on the server

sourceInput <- function(data) {
  sourcec <- data %>% select(Source) %>% distinct() 
  sourcec <- as.vector(sourcec$Source) %>% as.list()
  type <- data %>% select(Type) %>% distinct()
  checkboxGroupInput(paste(type,"source", sep=""), label="Select Location", choices=sourcec, selected = sourcec, inline=TRUE)
}

#Function to generate the year input slider - the type generates the input name for use on the server

yearInput <- function(data) {
  year_range <- data %>% select(Year) %>% distinct()
  type <- data %>% select(Type) %>% distinct()
  sliderInput(paste(type,"years", sep=""), "Years to show", min = min(year_range), max=max(year_range), value=c(min(year_range), max(year_range)), sep = "")
}
