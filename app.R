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
# Shiny UI -------
ui <- fluidPage(
  theme = "bootstrap.css",
  includeCSS("www/styles.css"),
  
navbarPage(
  "The US police shootings",
  id = "main_navbar"
))
###############################################################################################################
#The server starts here

server <- function(input, output) {
  #declaring reactive functions for first tab
  
}

shinyApp(ui = ui, server = server)