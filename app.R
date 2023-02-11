# Import shiny packages
library(shiny)
library(reticulate) # Python to R language wrapper
library(shinythemes) # UI themes for shiny
library(shinydashboard)
library(DT) # Data table functionality
library(tidyverse)
library(markdown)
library(shinyalert) # Used to create the popups/alerts
library(shinyBS) # Tooltip descriptions
library(shinyjs) # 'Quit' page to prevent freezes on local machine
library(leaflet) # Location and mapping
library(zipcodeR) # Zipcode input functionality
library(plotly) # Nice plots
library(lubridate) # Dealing with time series data

# Connect ui and server source files
source('ui-solar.R')
source('server-solar.R')

# Run shiny app
shinyApp(ui = ui, server = server)