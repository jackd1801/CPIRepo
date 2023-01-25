library(tidyverse)
library(lubridate)
library(plotly)

data <- read.csv('november.csv')

data <- data %>% filter(Products=="GENERAL INDEX (CPI)")

plot <- plot_ly(data, x = ~Date, y = ~Index, type = 'scatter', mode = 'lines', color=~Source) %>% 
        layout(title = Products,xaxis = list(title = ""),
         yaxis = list(title = "Index, February 2014 = 100"))
filename = 'Data/CPI_time_series_November_2022.xls'
data <- cpi_data(data)

cpi_data

Sources = c("Rural", "Urban", "All Rwanda")
datalist = list()
datalist = vector("list", length = length(Sources))
for (i in Sources) {
  data <- read_excel(filename, sheet=i, skip=3, col_names=TRUE)%>%
    slice(-1,-20)%>%
    rename(Province = "...1",
           U_R = "...2",
           COICOP = "...3",
           Products = "...4")%>%
    pivot_longer(!c(Province, U_R, COICOP, Products, Weights), names_to = "Date", values_to = "Index")%>%
    mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
           Products = gsub("v", "", Products),
           Products = str_squish(Products),
           Year = year(Date),
           Month = month(Date),
           Source = i)
  datalist[[i]] <- data}

cpi_clean <- function(data, monthi, yeari, sourcei, producti){
  df <- data %>% 
    filter(Products==producti) %>%
    filter(Source %in% sourcei) %>%
    filter(Year >= min(yeari) & Year <= max(yeari))
  if (monthi %in% month.name){
    df <- df %>% filter(Month==monthi) 
  } 
  return (df)
}

filename = 'Data/CPI_time_series_November_2022.xls'
data <- cpi_data(filename)
month = ''
year = c(2009, 2020)
source = c("All Rwanda", "Rural")
product = "GENERAL INDEX (CPI)"

clean <- cpi_clean(data, month, year, source, product)
