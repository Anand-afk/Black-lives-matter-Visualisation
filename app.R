#importing all the required libraries
library(shinydashboard)
library(shiny)
library(dplyr)
library(plotly)
library(corrgram)
library(countrycode)

#reading the csv file
data_f <- read.csv('fatal-police-shootings-data.csv',stringsAsFactors = F,header=T)
data_f <- tbl_df(data_f)

#updating the dataframe with only the required columns



########################################################################################################

# Application title

ui <- dashboardPage()
###############################################################################################################
#The server starts here

server <- function(input, output) {
  #declaring reactive functions for first tab
  
}

shinyApp(ui = ui, server = server)