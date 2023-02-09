library('tidyverse')
library('readxl')
library('plotly')

## Functions to load and clean CPI data for use in shiny dashboard


#Function to take input of the excel file containing the latest CPI data. The input is the excel file and the output is the combined dataframe.
# It uses a for loop to read in the data from each source and clean the data.
# It adds a year, month and source column for use in filtering the data for the dashboard.

cpi_data <- function(filename){
  Sources = c("Rural", "Urban", "All Rwanda")
  datalist = list()
  for (i in Sources) {
    data <- read_excel(filename, sheet=i, range="D4:FP23", col_names=TRUE) %>%
      slice(-1) %>%
      rename(Product = "...1") %>%
      pivot_longer(!c(Product, Weights), names_to = "Date", values_to = "Index") %>%
      #Clean up date formatting, string formatting, and add columns for filtering 
      mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
             Product = gsub("v", "", Product),
             Product = str_squish(Product),
             Product = replace(Product, Product=="GENERAL INDEX (CPI)", "General Index"),
             Year = year(Date),
             Month = month(Date, label = TRUE, abbr = FALSE),
             Source = i,
             Type="CPI")%>%
      select(Product, Weights, Date, Month, Year, Source, Index, Type)
    datalist[[i]] <- data
  }
  combined_data = do.call(rbind, datalist)
  return(combined_data)
}

#Function to clean the data once the inputs have been selected in the app. The input is the dataframe and different variables to filter on, and the output is the filtered dataframe.
#It selects the product, the sources to include, the year range, and a month if selected. 

cpi_clean <- function(data, monthi, yeari, sourcei, producti){
  df <- data %>% 
    filter(Product==producti) %>%
    filter(Source %in% sourcei) %>%
    filter(Year >= min(yeari) & Year <= max(yeari))
  if (monthi %in% month.name){
    df <- df %>% filter(Month==monthi) 
  } 
  return (df)
}

#Function to plot the data based on selected inputs. The input is a dataframe and the output is a plot.

cpi_plot <- function(data){
  plot.title = unique(data$Product)
  plot = plot_ly(data, x = ~Date, y = ~Index, type = 'scatter', mode = 'lines', color=~Source)%>%
    layout(title = paste("CPI for",plot.title),
           xaxis = list(title = ""),
           yaxis = list (title = "Index, February 2014 = 100"),
           legend = list(orientation='h', xanchor = "center", x = 0.45))
  return (plot)
}


#Function which combines cpi_clean and cpi_plot to take the data, month, year, source and product as inputs and outputs a completed plot

cpi_analysis <- function(data, monthi, yeari, sourcei, producti){
  clean_data <- cpi_clean(data, monthi, yeari, sourcei, producti)
  plot_data <- cpi_plot(clean_data)
  return (plot_data)
}
