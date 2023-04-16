# Import packages
library(shiny) # Package required for shiny
library(rsconnect) # Connection to shinyapps.io servers
library(shinythemes) # UI themes for shiny
library(shinydashboard) # Dashboard functionality
library(DT) # Data table functionality
library(tidyverse) # Data wrangling/transformation
library(shinyalert) # Used to create the popups/alerts
library(shinyBS) # Tooltip descriptions
library(shinyjs) # 'Quit' page to prevent freezes on local machine
library(leaflet) # Location and mapping
library(zipcodeR) # Zipcode input functionality
library(plotly) # Nice plots
library(lubridate) # Dealing with time series data
library(httr) # API calls
library(reshape2)
library(writexl) # Download excel files
library(xlsx)
library(readxl)
#library(lpSolve) # Linear programming for financial model

# Connect ui and server source files
source('ui-solar.R')
source('server-solar.R')

# Run shiny app
shinyApp(ui = ui, server = server)