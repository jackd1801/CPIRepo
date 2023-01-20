library(tidyverse)
library(lubridate)

filter_test <- data %>% filter(Date > 2019)

datecolumns <- data %>%
                mutate(years = format(Date, format="%Y"))%>%
                select(years)%>%
                distinct()
datecolumns <- as.vector(datecolumns$years)%>%
                as.list()
