library(reticulate)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(tidyverse)
library(markdown)
library(shinyalert) # Used to create the popups/alerts
library(shinyBS) # Tooltip descriptions
library(shinyjs) # 'Quit' page to prevent freezes on local machine



source_python('solar_panel_functions.py')

# Simple function to close window (not needed in shiny apps version) but useful when testing (sometimes shiny can glitch and force a restart)
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

Header_Details <- list(h3("Welcome to my solar panel simulation application!", align="center", style="font-size:30px"),
                       br(),
                       br(),
                       p("This application utilizes the National Renewable Energy Laboratory's (NREL) System Adivsor Model (SAM) to analyze solar panel energy and produce financial outputs after inputing metrics from the user. Calculations and models are run through a Python wrapper of SAM called PySAM. Weather data is gathered from a National Solar Radiation Database (NSRDB) API in the form of 2020 typical meteorological year (TMY).", align = "left", style = "font-size:15px; padding-left:40px"),
                       br(),
                       br())

Footer_Details <- list(hr(),
                       p("Please contact me at", tags$a(href="danielsaul@me.com","danielsaul@me.com"), "for any questions.", align="center"), 
                       p("Copyright 2022", align="center"))

Instruction_Details <- list(h3("How to use this simulation:", align="left", style="font-size:20px"),
                       br(),
                       p("1. Start by entering in the latitude and longitude of the location you wanted to simulate, rounded to two decimals, in the 'Location Data' tab.", align="left", style="padding-left:40px; font-size:15px"),
                       p("2. Move to the 'Simulation' tab and enter the greenhouse, system design, lifetime degradation, and financial inputs for the weather data you selected in step 1.", align="left", style="padding-left:40px; font-size:15px"),
                       p("3. Next, utilize the output tabs included in the 'Simulation' tab after running your model.", align="left", style="padding-left:40px; font-size:15px"),
                       p("4. Navigate to the 'Data Dictionary' tab for further information on the model, inputs, and outputs. Or, go to the 'Quit' tab to end your session.", align="left", style="padding-left:40px; font-size:15px"),
                       h3("Notes:", align="left", style="font-size:20px"),
                       p("- User information entered in this estimator is not stored.", align="left", style="padding-left:40px; font-size:15px"),
                       p("- All information is discarded once you exit the simulator.", align="left", style="padding-left:40px; font-size:15px"),
                       p("- If the application goes dark or isn't working properly, please go to the 'Quit' tab or exit the browser and restart to start a new session.", align="left", style="padding-left:40px; font-size:15px"))

#navBar_title <- tags$a(tags$img(src='uga-logo.png', height = 35, width = 24), "Solar Panel Simulation")

more_info <- read_csv("Data/Solar Data Dictionary - Sheet1 (1).csv")
                       

ui <- fluidPage (
  theme = shinytheme("spacelab"),
  navbarPage(title="Solar Panel Simulation",
             windowTitle = "Solar Panel Simulation",
             footer = isolate({Footer_Details}),
             tabPanel("Instructions", icon = icon("house"), isolate({Header_Details}), isolate({Instruction_Details})),
             tabPanel("Location Data", icon = icon("table"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Weather Data Location"),
                          numericInput(inputId = "lat",
                                    label = "Latitude:",
                                    width = '200px',
                                    value = '',
                                    min=-90,
                                    max=90),
                          bsTooltip('lat', "Latitude of weather location. Please round to two decimals and provide a value between -90 and 90.",'right',options = list(container = 'body')),
                          
                          numericInput(inputId = "lon",
                                    label = "Longitude:",
                                    width = '200px',
                                    value = '',
                                    min=-180,
                                    max=180),
                          bsTooltip('lon', "Longitude of weather location. Please round to two decimals and provide a value between -180 and 180.",'right',options = list(container = 'body')),
                          
                          helpText("Please wait a moment for the application to update, gather data, and output the table."),
                          actionButton(inputId = "WeatherDataButton", 
                                       label = "Submit",
                                       width = '100px',
                                       icon("arrows-rotate")),
                        ),
                        mainPanel(
                          DT::dataTableOutput("weather_table")
                        )
                      )),
             tabPanel("Simulation", icon = icon("bolt"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Panel & System Inputs"),
                          h3("Greenhouse Panels"),
                          
                          numericInput(inputId = 'roof_length',
                                       label = 'Roof Length (meters):',
                                       width = '200px',
                                       value = '',
                                       min=0),
                          bsTooltip('roof_length', "The length of the greenhouse roof.",'right',options = list(container = 'body')),
                          
                          numericInput(inputId = 'roof_width',
                                       label = 'Roof Width (meters):',
                                       width = '200px',
                                       value = '',
                                       min=0),
                          bsTooltip('roof_width', "The width of the greenhouse roof.",'right',options = list(container = 'body')),
                          
                          HTML(paste0("<b>","Roof Area:","</b>")),
                          br(),
                          textOutput('roof_area_out'),
                          
                          br(),
                          
                          numericInput(inputId = 'panel_coverage',
                                       label = 'Panel Roof Coverage (%):',
                                       width = '200px',
                                       value = '',
                                       min=0),
                          bsTooltip('panel_coverage', "The percentage of the greenhouse roof the panels cover.",'right',options = list(container = 'body')),
                          
                          HTML(paste0("<b>","Panel Coverage Area:","</b>")),
                          br(),
                          textOutput('panelcov_area_out'),
                          
                          br(),
                          
                          numericInput(inputId = 'tilt',
                                       label = 'Panel Tilt (degrees):',
                                       width = '200px',
                                       value = '',
                                       min=0,
                                       max=90),
                          bsTooltip('tilt', "Degree tilt of the solar panels.",'right',options = list(container = 'body')),
                          
                          numericInput(inputId = 'azimuth',
                                       label = 'Azimuth (degrees):',
                                       width = '200px',
                                       value = '',
                                       min=0,
                                       max=360),
                          bsTooltip('azimuth', "Azimuth is the angle that the solar panels are facing and is measured in a clockwise direction from north.",'right',options = list(container = 'body')),
                          
                          h3("System Design"),
                          
                          HTML(paste0("<b>","System Capacity:","</b>")),
                          br(),
                          textOutput('system_capacity_out'),
                          
                          br(),
                          
                          numericInput(inputId = 'inv_eff',
                                       label = 'Inverter Efficiency (%):',
                                       width = '200px',
                                       value = '',
                                       min=90,
                                       max=99.5),
                          bsTooltip('inv_eff', "The ratio of the usable AC output power to the sum of the DC input power and any AC input power. Typically, between 95 to 98%.",'right',options = list(container = 'body')),
                          
                          numericInput(inputId = 'losses',
                                       label = 'Losses (%):',
                                       width = '200px',
                                       value = '',
                                       min=-5,
                                       max=99),
                          bsTooltip('losses', "Total system losses (do NOT include a percent sign %).",'right',options = list(container = 'body')),
                          
                          numericInput(inputId = 'gcr',
                                       label = 'Ground Coverage Ratio:',
                                       width = '200px',
                                       value = '',
                                       min=0.01,
                                       max=0.99),
                          bsTooltip('gcr', "The ratio of module surface area to the area of the ground or roof occupied by the array. Typical values range from 0.3 to 0.6.",'right',options = list(container = 'body')),
                          
                          numericInput(inputId = 'dc_ac_ratio',
                                       label = 'DC to AC Ratio:',
                                       width = '200px',
                                       value = '',
                                       min=0),
                          bsTooltip('dc_ac_ratio', "The ratio of installed DC capacity to the inverters AC power rating.",'right',options = list(container = 'body')),
                          
                          radioButtons(inputId = 'array_type', 
                                       label = 'Array Type:', 
                                       choiceNames = c('Fixed - Open Rack','Fixed - Roof Mounted','1-Axis','Backtracked','2-Axis'), 
                                       choiceValues = c(0,1,2,3,4)),
                          
                          radioButtons(inputId = 'module_type', 
                                       label = 'Module Type:', 
                                       choiceNames = c('Standard: Polycrystalline','Premium: Monocrystalline','Thin film'), 
                                       choiceValues = c(0,1,2)),
                          
                          h3("System Lifetime"),
                          
                          radioButtons(inputId = 'system_use_lifetime_output', 
                                       label = 'Use lifetime simulation?:', 
                                       choiceNames = c('No','Yes'), 
                                       choiceValues = c(0,1)),
                          
                          numericInput(inputId = 'analysis_period',
                                       label = 'Analysis Period (years):',
                                       width = '200px',
                                       value = '0',
                                       min=0),
                          bsTooltip('analysis_period', "The number of years you want to use as the analysis period for the simulation.",'right',options = list(container = 'body')),
                          
                          textInput(inputId = 'dc_degradation',
                                       label = 'DC Degradation (%/year):',
                                       width = '200px',
                                       value = '0'),
                          bsTooltip('dc_degradation', "The percent per year annual DC degradation for lifetime simulations. Please enter your input either as sequence object, requiring this format `0.5, 0.6, 0.7` matching each analysis period year, for instance, that format would use 3 years.",'right',options = list(container = 'body')),
                          
                          
                          h3("Financials"),
                          actionButton(inputId = "SimulateButton", 
                                       label = "Simulate",
                                       width = '100px',
                                       icon("arrows-rotate")),
                          
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Annual Outputs", tableOutput("annual_outputs")),
                            tabPanel("Hourly Outputs", DT::dataTableOutput("hourly_outputs")),
                            tabPanel("Monthly Outputs", tableOutput("monthly_outputs")),
                            tabPanel("Plots")
                          )
                        )
                      )),
             tabPanel("More Information", icon = icon("book"),
                      sidebarLayout(sidebarPanel(h3("Sources"), 
                                                 h3("Licenses"), 
                                                 h3("Creator"),
                                                 p("Daniel Saul"),
                                                 p("Student Assistant - University of Georgia"),
                                                 actionLink('github', label = 'GitHub', icon = icon("github"), onclick ="window.open(href='https://github.com/DanielPSaul');"),
                                                 br(),
                                                 actionLink('linkedin', label = 'LinkedIn', icon = icon("linkedin"), onclick ="window.open(href='https://www.linkedin.com/in/danielsaul1/');"),
                                                 br(),
                                                 actionLink('Email', label = 'Email', icon = icon("envelope"), onclick ="location.href='mailto:danielsaul@me.com';"),
                                                 ),
                                    mainPanel(width = 8, 
                                              h3("Data Dictionary"),
                                              DT::dataTableOutput("more_info_table")),
                                    position = c("right"),
                                    fluid = TRUE
                                    
                                    )
                      ),
             tabPanel(title = "Quit", icon = icon("circle-xmark"), actionButton("close", "Click Here to End Session"))
             
  )
)

server <- function(input, output, session) {
  
  useShinyjs()
  extendShinyjs(text = jscode, functions = c("closeWindow"))
  
  
  # Close/exit the server session (useful when coding locally. Can be bypassed via shiny apps)
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })
  
  output$more_info_table <- DT::renderDataTable(more_info,
                                              options = list(autoWidth = TRUE,
                                                             pageLength = 3),
                                              rownames = FALSE)
  
  
  
  # Get weather data and table
  observeEvent(input$WeatherDataButton, {
    shinyalert("Gathering Data...", showConfirmButton = FALSE, timer = 2000)
    
    data_outputs <- getWeatherData(isolate(input$lat), isolate(input$lon))
    data <- data_outputs[[1]]
    
    weather_data <- data %>%
      select(., 6:14) %>%
      rownames_to_column(., var = "Timestamp")
    
    output$weather_table <- DT::renderDataTable(weather_data,
                                                options = list(autoWidth = TRUE,
                                                               pageLength = 5),
                                                rownames = FALSE)
    
    shinyalert("Gathering Complete.", showConfirmButton = TRUE, type = "success")
    
  })
  
  observeEvent(input$SimulateButton, {
    shinyalert("Running Simulation...", showConfirmButton = FALSE, timer = 2000)
    
    data_outputs <- getWeatherData(isolate(input$lat), isolate(input$lon))
    df <- data_outputs[[1]]
    info <- data_outputs[[2]]
    
    tilt <- isolate(input$tilt) 
    azimuth <- isolate(input$azimuth) 
    inv_eff <-  isolate(input$inv_eff) 
    losses <- isolate(input$losses) 
    array_type <- as.numeric(isolate(input$array_type))
    gcr <- isolate(input$gcr) 
    module_type <- as.numeric(isolate(input$module_type)) 
    dc_ac_ratio <- isolate(input$dc_ac_ratio)
    
    system_use_lifetime_output <- as.numeric(isolate(input$system_use_lifetime_output))
    analysis_period <- as.numeric(isolate(input$analysis_period))
    dc_degradation <- isolate(input$dc_degradation)
    
    
    roof_length <- as.numeric(isolate(input$roof_length))
    roof_width <- as.numeric(isolate(input$roof_width))
    panel_coverage <- as.numeric(isolate(input$panel_coverage)/100)
    
    roof_area <- roof_length * roof_width
    panelcov_area <- roof_area * panel_coverage
    
    output$roof_area_out <- renderText({
      paste(roof_area, "meters\u00B2")
    })
    
    output$panelcov_area_out <- renderText({
      paste(panelcov_area, "meters\u00B2")
    })
    
    
    module_efficiency <- 0
    
    if(module_type == 0) {
      module_efficiency = 0.19
    } else if(module_type == 1) {
      module_efficiency = 0.21
    } else if(module_type == 2) {
      module_efficiency = 0.18
    }
    
    system_capacity <- panelcov_area * 1 * module_efficiency
    
    output$system_capacity_out <- renderText({
      paste(system_capacity, "kW")
    })
    
    
    outputs_data <- runSimulation(df, info, tilt, azimuth, inv_eff, losses, array_type, gcr, module_type, dc_ac_ratio, system_capacity, system_use_lifetime_output, analysis_period, dc_degradation)
    
    hourly_output <- outputs_data[[1]]
    monthly_output <- outputs_data[[2]]
    annual_output <- outputs_data[[3]]
    
    output$annual_outputs <- renderTable(annual_output)
    output$hourly_outputs <- DT::renderDataTable(hourly_output)
    output$monthly_outputs <- renderTable(monthly_output)
    
    shinyalert("Simulation Complete.", showConfirmButton = TRUE, type = "success")
  })
}

shinyApp(ui = ui, server = server)

