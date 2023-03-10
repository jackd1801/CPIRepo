rm(list = ls())

library('tidyverse')
library('readxl')

cpi_data <- function(file_name){
Sources = c("Rural", "Urban", "All Rwanda")
datalist = list()
datalist = vector("list", length = length(Sources))
for (i in Sources) {
  data <- read_excel(file_name, sheet=i, skip=3, col_names=TRUE)%>%
    slice(-1,-20)%>%
    rename(Province = "...1",
           U_R = "...2",
           COICOP = "...3",
           Products = "...4")%>%
    pivot_longer(!c(Province, U_R, COICOP, Products, Weights), names_to = "Date", values_to = "Index")%>%
    mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
           Products = gsub("v", "", Products),
           Products = str_squish(Products),
           Source = i)
  datalist[[i]] <- data
}
combined = do.call(rbind, datalist)
return(combined)
}

cpi_plot <- function(dataframe, inflation){}



