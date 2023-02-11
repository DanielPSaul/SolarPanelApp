
# Simple function to close window (not needed in shiny apps version) but useful when testing (sometimes shiny can glitch and force a restart)
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# Header text for the home page
Header_Details <- list(h3("Welcome to my solar panel simulation application!", align="center", style="font-size:30px"),
                       br(),
                       br(),
                       p("This application utilizes the National Renewable Energy Laboratory's (NREL) System Adivsor Model (SAM) to analyze solar panel energy and produce financial outputs after inputing metrics from the user. Calculations and models are run through a Python wrapper of SAM called PySAM. Weather data is gathered from a National Solar Radiation Database (NSRDB) API in the form of 2020 typical meteorological year (TMY).", align = "left", style = "font-size:15px; padding-left:40px"),
                       br(),
                       br())

# Footer text for all pages 
Footer_Details <- list(hr(),
                       p("Please contact me at", tags$a(href="mailto:danielsaul@me.com","danielsaul@me.com"), "for any questions.", align="center"), 
                       p("Copyright 2022", align="center"))

# Instructional text on the home page
Instruction_Details <- list(h3("How to use this simulation:", align="left", style="font-size:20px"),
                            br(),
                            p("1. Start by entering in the latitude and longitude, or zipcode, of the location you wanted to simulate, rounded to two decimals, in the 'Location Data' tab.", align="left", style="padding-left:40px; font-size:15px"),
                            p("2. Move to the 'Simulation' tab and enter the greenhouse, system design, and lifetime degradation inputs for the weather data you selected in step 1.", align="left", style="padding-left:40px; font-size:15px"),
                            p("3. Next, utilize the output tabs included in the 'Simulation' tab after running your model.", align="left", style="padding-left:40px; font-size:15px"),
                            p("4. Navigate to the 'More Information' tab for further information on the model, inputs, and outputs. Or, go to the 'Quit' tab to end your session.", align="left", style="padding-left:40px; font-size:15px"),
                            h3("Notes:", align="left", style="font-size:20px"),
                            p("- User information entered in this estimator is not stored.", align="left", style="padding-left:40px; font-size:15px"),
                            p("- All information is discarded once you exit the simulator.", align="left", style="padding-left:40px; font-size:15px"),
                            p("- If the application goes dark or isn't working properly, please go to the 'Quit' tab or exit the browser and restart to start a new session.", align="left", style="padding-left:40px; font-size:15px"))

#navBar_title <- tags$a(tags$img(src='uga-logo.png', height = 35, width = 24), "Solar Panel Simulation")

# Table of definitions and information in the more info page
more_info <- read_csv("Solar Data Dictionary - Sheet1 (1).csv")


################################################################################
#                             USER INTERFACE (UI)
################################################################################

ui <- fluidPage (
  
  theme = shinytheme("spacelab"),
  navbarPage(title="Solar Panel Simulation", 
             windowTitle = "Solar Panel Simulation", 
             footer = isolate({Footer_Details}),
             
             # Instructions tab
             tabPanel("Instructions", 
                      icon = icon("house"), 
                      isolate({Header_Details}), 
                      isolate({Instruction_Details})),
             
             # Location data tab
             tabPanel("Location Data", 
                      icon = icon("table"), 
                      sidebarLayout(
                        sidebarPanel(titlePanel("Weather Data Location"),
                                     
                                     selectInput(inputId = "location_type", 
                                                 label = "Location Input Type:", 
                                                 width = "200px", 
                                                 choices = c("Latitude/Longitude", "Zipcode")),
                                     
                                     conditionalPanel(condition = "input.location_type == 'Latitude/Longitude'",
                                                      numericInput(inputId = "lat", 
                                                                   label = "Latitude:", 
                                                                   width = '200px', 
                                                                   value = '', 
                                                                   min=-90, 
                                                                   max=90),
                                                      bsTooltip('lat', 
                                                                "Latitude of weather location. Please round to two decimals and provide a value between -90 and 90.",
                                                                'right',
                                                                options = list(container = 'body')),
                                                      
                                                      numericInput(inputId = "lon", 
                                                                   label = "Longitude:", 
                                                                   width = '200px', 
                                                                   value = '', 
                                                                   min=-180, 
                                                                   max=180),
                                                      bsTooltip('lon', 
                                                                "Longitude of weather location. Please round to two decimals and provide a value between -180 and 180.",
                                                                'right',
                                                                options = list(container = 'body'))),
                                     
                                     conditionalPanel(condition = "input.location_type == 'Zipcode'",
                                                      
                                                      numericInput(inputId = "zipcode",
                                                                   label = "Zipcode:", 
                                                                   width = '200px',
                                                                   value = '',
                                                                   min=00000,
                                                                   max=99999),
                                                      bsTooltip('zipcode', 
                                                                "The 5-digit zipcode of the weather location. Please do not add any spaces or dashes.",
                                                                'right',
                                                                options = list(container = 'body')),
                                                      
                                                      HTML(paste0("<b>","Latitude:","</b>")),
                                                      textOutput('lat'),
                                                      
                                                      HTML(paste0("<b>","Longitude:","</b>")),
                                                      textOutput('lon')),
                                     
                                     helpText("Please wait a moment for the application to update, gather data, and output the table. If you change input values, please click the button again."),
                                     
                                     actionButton(inputId = "WeatherDataButton",
                                                  label = "Submit",
                                                  width = '100px',
                                                  icon("arrows-rotate")),
                                     
                                     br(),
                                     br(),
                                     
                                     leafletOutput("weather_map")
                                     
                        ),
                        
                        mainPanel(DT::dataTableOutput("weather_table"),
                                  br(),
                                  br(),
                                  plotlyOutput(outputId = "irradiance_plot", height = 'auto', width = 'auto'))
                      )
             ),
             
             # Simulation tab
             tabPanel("Simulation", 
                      icon = icon("bolt"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Panel & System Inputs"),
                          helpText("Please fill in all inputs as they are required and may produce errors if empty or not entered correctly."),
                          h3("Greenhouse Panels"),
                          
                          numericInput(inputId = 'roof_length',
                                       label = 'Roof Length (meters):',
                                       width = '200px',
                                       value = '',
                                       min=0),
                          bsTooltip('roof_length', 
                                    "The length of the greenhouse roof.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          numericInput(inputId = 'roof_width',
                                       label = 'Roof Width (meters):',
                                       width = '200px',
                                       value = '',
                                       min=0),
                          bsTooltip('roof_width', 
                                    "The width of the greenhouse roof.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          HTML(paste0("<b>","Roof Area:","</b>")),
                          br(),
                          textOutput('roof_area_out'),
                          br(),
                          
                          numericInput(inputId = 'panel_coverage',
                                       label = 'Panel Roof Coverage (%):',
                                       width = '200px',
                                       value = '',
                                       min=0),
                          bsTooltip('panel_coverage', 
                                    "The percentage of the greenhouse roof the panels cover.",
                                    'right',
                                    options = list(container = 'body')),
                          
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
                          bsTooltip('tilt', 
                                    "Degree tilt of the solar panels.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          numericInput(inputId = 'azimuth',
                                       label = 'Azimuth (degrees):',
                                       width = '200px',
                                       value = '',
                                       min=0,
                                       max=360),
                          bsTooltip('azimuth', 
                                    "Azimuth is the angle that the solar panels are facing and is measured in a clockwise direction from north.",
                                    'right',
                                    options = list(container = 'body')),
                          
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
                          bsTooltip('inv_eff', 
                                    "The ratio of the usable AC output power to the sum of the DC input power and any AC input power. Typically, between 95 to 98%.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          numericInput(inputId = 'losses',
                                       label = 'Losses (%):',
                                       width = '200px',
                                       value = '',
                                       min=-5,
                                       max=99),
                          bsTooltip('losses', 
                                    "Total system losses (do NOT include a percent sign %).",
                                    'right',
                                    options = list(container = 'body')),
                          
                          numericInput(inputId = 'gcr',
                                       label = 'Ground Coverage Ratio:',
                                       width = '200px',
                                       value = '',
                                       min=0.01,
                                       max=0.99),
                          bsTooltip('gcr', 
                                    "The ratio of module surface area to the area of the ground or roof occupied by the array. Typical values range from 0.3 to 0.6.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          numericInput(inputId = 'dc_ac_ratio',
                                       label = 'DC to AC Ratio:',
                                       width = '200px',
                                       value = '',
                                       min=0),
                          bsTooltip('dc_ac_ratio', 
                                    "The ratio of installed DC capacity to the inverters AC power rating.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          radioButtons(inputId = 'array_type', 
                                       label = 'Array Type:', 
                                       choiceNames = c('Fixed - Open Rack','Fixed - Roof Mounted','1-Axis','Backtracked','2-Axis'), 
                                       choiceValues = c(0,1,2,3,4)),
                          
                          radioButtons(inputId = 'module_type', 
                                       label = 'Module Type:', 
                                       choiceNames = c('Standard: Polycrystalline','Premium: Monocrystalline','Thin film'), 
                                       choiceValues = c(0,1,2)),
                          
                          HTML(paste0("<b>","Module Efficiency:","</b>")),
                          br(),
                          textOutput('mod_eff'),
                          
                          
                          h3("System Lifetime"),
                          
                          radioButtons(inputId = 'system_use_lifetime_output', 
                                       label = 'Use lifetime simulation?:', 
                                       choiceNames = c('No','Yes'), 
                                       choiceValues = c(0,1)),
                          
                          conditionalPanel(condition = "input.system_use_lifetime_output == 1",
                                           numericInput(inputId = 'analysis_period',
                                                        label = 'Analysis Period (years):',
                                                        width = '200px',
                                                        value = '0',
                                                        min=0),
                                           bsTooltip('analysis_period', 
                                                     "The number of years you want to use as the analysis period for the simulation.",
                                                     'right',
                                                     options = list(container = 'body')),
                                           
                                           textInput(inputId = 'dc_degradation',
                                                     label = 'DC Degradation (%/year):',
                                                     width = '200px',
                                                     value = '0'),
                                           bsTooltip('dc_degradation', 
                                                     "The percent per year annual DC degradation for lifetime simulations. Please enter your input either as sequence object, requiring this format `0.5, 0.6, 0.7` matching each analysis period year, for instance, that format would use 3 years.",
                                                     'right',
                                                     options = list(container = 'body'))),
                          
                          helpText("Please wait a moment for the application to simulate and generate outputs. If you change input values, please click the button again."),
                          
                          actionButton(inputId = "SimulateButton", 
                                       label = "Simulate",
                                       width = '100px',
                                       icon("arrows-rotate"))
                          
                        ),
                        mainPanel(
                          tabsetPanel(
                            
                            tabPanel("Annual Outputs", 
                                     tableOutput("annual_outputs")),
                            
                            tabPanel("Hourly Outputs", 
                                     DT::dataTableOutput("hourly_outputs"),
                                     br(),
                                     br(),
                                     plotlyOutput(outputId = "daily_plot", height = 'auto', width = 'auto')),
                            
                            tabPanel("Monthly Outputs", 
                                     tableOutput("monthly_outputs"),
                                     br(),
                                     br(),
                                     plotlyOutput(outputId = "monthly_plot", height = 'auto', width = 'auto'))
                          )
                        )
                      )),
             
             # More information tab
             tabPanel("More Information", icon = icon("book"),
                      sidebarLayout(
                        sidebarPanel(h3("Sources"),
                                     actionLink('nrel', label = 'National Renewable Energy Laboratory (NREL)', icon = icon("link"), onclick ="window.open(href='https://www.nrel.gov/');"),
                                     br(),
                                     actionLink('nsrdb', label = 'National Solar Radiation Database (NSRDB)', icon = icon("link"), onclick ="window.open(href='https://nsrdb.nrel.gov/');"),
                                     br(),
                                     actionLink('sam', label = 'System Advisor Model (SAM)', icon = icon("link"), onclick ="window.open(href='https://sam.nrel.gov/');"),
                                     br(),
                                     actionLink('pvwatts', label = 'PVWatts', icon = icon("link"), onclick ="window.open(href='https://pvwatts.nrel.gov/pvwatts.php');"),
                                     
                                     h3("Licenses"),
                                     p("BSD 3-Clause"),
                                     
                                     h3("Creator"),
                                     p("Daniel Saul"),
                                     p(em("Student Assistant - University of Georgia")),
                                     actionLink('github', label = 'GitHub', icon = icon("github"), onclick ="window.open(href='https://github.com/DanielPSaul/SolarPanelApp');"),
                                     br(),
                                     actionLink('linkedin', label = 'LinkedIn', icon = icon("linkedin"), onclick ="window.open(href='https://www.linkedin.com/in/danielsaul1/');"),
                                     br(),
                                     actionLink('Email', label = 'Email', icon = icon("envelope"), onclick ="location.href='mailto:danielsaul@me.com';")),
                        
                        mainPanel(width = 8, 
                                  h3("Data Dictionary"),
                                  DT::dataTableOutput("more_info_table")),
                        position = c("right"),
                        fluid = TRUE)),
             
             # Quit tab (for restarting the session/app)
             tabPanel(title = "Quit", 
                      icon = icon("circle-xmark"), 
                      actionButton("close", "Click Here to End Session"))
             
  )
)