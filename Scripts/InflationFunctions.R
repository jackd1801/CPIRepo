library('tidyverse')
library('readxl')
library('plotly')

## Functions to load and clean Inflation data for use in shiny dashboard


#Function to take input of the excel file containing the latest inflation data. The input is the excel file and the output is the combined dataframe.
# It uses a for loop to read in the data from each source and clean the data.
# It creates a monthly inflation figure and a yearly inflation figure centered on each month.


inflation_data <- function(filename){
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
             Type="INF")%>%
      #Generate monthly inflation column
      group_by(Product)%>%
      arrange(Date)%>%
      mutate(IndexLag = lag(Index),
             Inflation = ((Index-IndexLag)/IndexLag))%>%
      ungroup()%>%
      #Generate yearly inflation column
      group_by(Product, Month) %>%
      arrange(Date)%>%
      mutate(IndexMonthLag=lag(Index),
             InflationMonth = (Index-IndexMonthLag)/IndexMonthLag)%>%
      ungroup()%>%
      select(Product, Weights, Date, Month, Year, Source, Inflation, InflationMonth, Type)
    datalist[[i]] <- data
  }
  combined_data = do.call(rbind, datalist)
  return(combined_data)
}

#Function to clean the data once the inputs have been selected in the app. The input is the dataframe and different variables to filter on, and the output is the filtered dataframe.
#It selects the product, the sources to include, the year range, and a month if selected. 

inflation_clean <- function(data, monthi, yeari, sourcei, producti){
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
#If a single month has been selected then the plot uses the yearly inflation column, if no month is selected it uses the monthly values.

inflation_plot <- function(data){
  #Generate plot title
  plot.title = unique(data$Product)
  #Identify number of unique months in dataframe
  unique.months = unique(data$Month)
  
  if(length(unique.months) == 1){
    plot = plot_ly(data, x = ~Date, y = ~InflationMonth, type = 'scatter', mode = 'lines', color=~Source)
  } else {
    plot = plot_ly(data, x = ~Date, y = ~Inflation, type = 'scatter', mode = 'lines', color=~Source)
  }
  
  plot = plot %>% 
          layout(title = paste("Inflation rate for", plot.title),
                  xaxis = list(title = ""),
                  yaxis = list (tickformat='.1%', title = "Inflation"),
                  legend = list(orientation='h', xanchor = "center", x = 0.45))
  return (plot)
}


#Function which combines cpi_clean and cpi_plot to take the data, month, year, source and product as inputs and outputs a completed plot

inflation_analysis <- function(data, monthi, yeari, sourcei, producti){
  clean_data <- inflation_clean(data, monthi, yeari, sourcei, producti)
  plot <- inflation_plot(clean_data)
  return (plot)
}