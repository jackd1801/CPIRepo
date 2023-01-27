library('tidyverse')
library('readxl')
library('plotly')



inflation_data <- function(filename){
  Sources = c("Rural", "Urban", "All Rwanda")
  datalist = list()
  for (i in Sources) {
    data <- read_excel(filename, sheet=i, skip=3, col_names=TRUE) %>%
      slice(-1,-20) %>%
      rename(Province = "...1",
             U_R = "...2",
             COICOP = "...3",
             Products = "...4") %>%
      pivot_longer(!c(Province, U_R, COICOP, Products, Weights), names_to = "Date", values_to = "Index") %>%
      mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
             Products = gsub("v", "", Products),
             Products = str_squish(Products),
             Year = year(Date),
             Month = month(Date, label = TRUE, abbr = FALSEs),
             Source = i)%>%
      group_by(Products)%>%
      arrange(Date)%>%
      mutate(IndexLag = lag(Index),
             InflationMonth = ((IndexLag+Index)/IndexLag))%>%
      ungroup()%>%
      group_by(Products, )
      select(Products, Weights, Date, Month, Year, Source, Inflation)
    datalist[[i]] <- data
  }
  combined_data = do.call(rbind, datalist)
  return(combined_data)
}

inflation_clean <- function(data, monthi, yeari, sourcei, producti){
  df <- data %>% 
    filter(Products==producti) %>%
    filter(Source %in% sourcei) %>%
    filter(Year >= min(yeari) & Year <= max(yeari))
  if (monthi %in% month.name){
    df <- df %>% filter(Month==monthi) 
  } 
  return (df)
}



inflation_plot <- function(data){
  plot.title = unique(data$Products)
  plot = plot_ly(data, x = ~Date, y = ~Inflation, type = 'scatter', mode = 'lines', color=~Source)%>%
    layout(title = plot.title,
           xaxis = list(title = ""),
           yaxis = list (title = "Index, February 2014 = 100"))
  return (plot)
}