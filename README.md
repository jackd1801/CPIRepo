This repository contains an R Shiny app to present the Rwandan CPI data. 

This is based on the Tableau dashboard already produced by NISR found here: https://public.tableau.com/app/profile/data.revolution.and.big.data.unit/viz/ConsumerPriceIndexofRwanda/CPI

The structure of this repository is as follows.

There are 3 folders. 

Data contains the latest available CPI data - this is used as the input for the app to generate the dashboard. 

Scripts contains 3 scripts which make the app.r file easier to format: 
  - CPIfunctions: This script has 3 functions which format the initial dataframe from the excel input, clean the data based on the inputs selected in the dashboard, and outputs the cleaned data in a plot. 
  - InflationFunctions: This script has 3 functions which do the same as CPIfunctions. The initial import function also adds a month-month and year-year inflation column.
  - InputFunctions: This script contains all the text and input objects so they can be inputted to the UI in a simpler way. 
  
 www contains the NISR logo for use in the header.
 
 The app.R file contains the dashboard app. 

