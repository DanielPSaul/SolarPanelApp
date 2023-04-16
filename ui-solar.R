

# Header text for the home page
Header_Details <- list(h3("Welcome to my solar panel simulation application!", align="center", style="font-size:30px"),
                       br(),
                       br(),
                       p("This application utilizes the National Renewable Energy Laboratory's (NREL) System Adivsor Model (SAM) to analyze solar panel energy and produce financial outputs after inputing metrics from the user. System calculations and models are run through the PVWatts API with the various inputs the user will enter. Other calculations such as financial and lifetime instances are calculated by our code and not the API. Weather data is gathered from a National Solar Radiation Database (NSRDB) API in the form of 2020 typical meteorological year (TMY).", align = "left", style = "font-size:15px; padding-left:40px"),
                       br(),
                       br())

# Footer text for all pages 
Footer_Details <- list(hr(),
                       p("Please contact me at", tags$a(href="mailto:danielsaul@me.com","danielsaul@me.com"), "for any questions.", align="center"), 
                       p("Copyright 2023", align="center"))

# Instructional text on the home page
Instruction_Details <- list(h3("How to use this simulation:", align="left", style="font-size:20px"),
                            br(),
                            p("1. Start by entering in the latitude and longitude, or zipcode, of the location you wanted to simulate, rounded to two decimals, in the 'Location Data' tab.", align="left", style="padding-left:40px; font-size:15px"),
                            p("2. Move to the 'Simulation' tab and enter the greenhouse, system design, and lifetime inputs for the weather data you selected in step 1.", align="left", style="padding-left:40px; font-size:15px"),
                            p("3. Next, utilize the output tabs included in the 'Simulation' tab after running your model.", align="left", style="padding-left:40px; font-size:15px"),
                            p("4. If necessary, download the table outputs and plots after running the simulation. If you hover over the plots, there is a button where you can download the plot as a PNG.", align="left", style="padding-left:40px; font-size:15px"),
                            p("5. Navigate to the 'More Information' tab for further information on the model, inputs, and outputs. Or, go to the 'Restart' tab to restart your session. To stop the session altogether, just close out the tab you are running the app on.", align="left", style="padding-left:40px; font-size:15px"),
                            h3("Notes:", align="left", style="font-size:20px"),
                            p("- For the weather data inputs, default values are for Athens, Georgia. For all other simulation inputs, the default values are those provided by NREL's PVWatts.", align="left", style="padding-left:40px; font-size:15px"),
                            p("- User information entered in this estimator is not stored.", align="left", style="padding-left:40px; font-size:15px"),
                            p("- All information is discarded once you exit the simulator.", align="left", style="padding-left:40px; font-size:15px"),
                            p("- If the application goes dark or isn't working properly, please go to the 'Restart' tab or exit the browser and restart to start a new session.", align="left", style="padding-left:40px; font-size:15px"))

#navBar_title <- tags$a(tags$img(src='uga-logo.png', height = 35, width = 24), "Solar Panel Simulation")

# Table of definitions and information in the more info page
more_info <- read_csv("Solar Data Dictionary.csv")


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
                                                                   value = '33.94', 
                                                                   min=-90, 
                                                                   max=90),
                                                      bsTooltip('lat', 
                                                                "Latitude of weather location. Please round to two decimals and provide a value between -90 and 90.",
                                                                'right',
                                                                options = list(container = 'body')),
                                                      
                                                      numericInput(inputId = "lon", 
                                                                   label = "Longitude:", 
                                                                   width = '200px', 
                                                                   value = '-83.37', 
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
                                                                   value = '30601',
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
                        
                        mainPanel(downloadButton('download_table', 'Download Table'),
                                  helpText('Downloading the table may take a few moments.'),
                                  br(),
                                  DT::dataTableOutput("weather_table"),
                                  br(),
                                  hr(),
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
                          
                          # Roof length input
                          numericInput(inputId = 'roof_length',
                                       label = 'Roof Length (meters):',
                                       width = '200px',
                                       value = '10',
                                       min=0),
                          bsTooltip('roof_length', 
                                    "The length of the greenhouse roof.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          # Roof width input
                          numericInput(inputId = 'roof_width',
                                       label = 'Roof Width (meters):',
                                       width = '200px',
                                       value = '10',
                                       min=0),
                          bsTooltip('roof_width', 
                                    "The width of the greenhouse roof.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          # Roof area output
                          HTML(paste0("<b>","Roof Area:","</b>")),
                          br(),
                          textOutput('roof_area_out'),
                          br(),
                          
                          # Roof-Panel cover input
                          numericInput(inputId = 'panel_coverage',
                                       label = 'Panel Roof Coverage (%):',
                                       width = '200px',
                                       value = '80',
                                       min=0),
                          bsTooltip('panel_coverage', 
                                    "The percentage of the greenhouse roof the panels cover.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          
                          # Panel coverage output
                          HTML(paste0("<b>","Panel Coverage Area:","</b>")),
                          br(),
                          textOutput('panelcov_area_out'),
                          br(),
                          
                          # Panel tilt input
                          numericInput(inputId = 'tilt',
                                       label = 'Panel Tilt (degrees):',
                                       width = '200px',
                                       value = '0',
                                       min=0,
                                       max=90),
                          bsTooltip('tilt', 
                                    "Degree tilt of the solar panels.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          # Azimuth input
                          numericInput(inputId = 'azimuth',
                                       label = 'Azimuth (degrees):',
                                       width = '200px',
                                       value = '180',
                                       min=0,
                                       max=360),
                          bsTooltip('azimuth', 
                                    "Azimuth is the angle that the solar panels are facing and is measured in a clockwise direction from north.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          h3("System Design"),
                          
                          # System capacity output
                          HTML(paste0("<b>","System Capacity:","</b>")),
                          br(),
                          textOutput('system_capacity_out'),
                          br(),
                          
                          # Inverter efficiency input
                          numericInput(inputId = 'inv_eff',
                                       label = 'Inverter Efficiency (%):',
                                       width = '200px',
                                       value = '96',
                                       min=90,
                                       max=99.5),
                          bsTooltip('inv_eff', 
                                    "The ratio of the usable AC output power to the sum of the DC input power and any AC input power. Typically, between 95 to 98%.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          
                          # Losses button
                          shinyjs::hidden(
                                actionButton('loss_button', 'Losses', icon = icon('list-alt'))
                          ),
                          
                          br(),
                          br(),
                          
                          # Losses panel
                          bsModal(id = 'loss_pan', title = 'Potential Energy Losses', trigger = 'loss_button', size = 'large',
                                  radioButtons(inputId = 'loss_type', 
                                               label = 'Losses Type:', 
                                               choices = c('Non-Specific','Specific')),
                                  
                                  
                                  # Specific losses
                                  conditionalPanel(condition = "input.loss_type == 'Specific'",
                                                   
                                                   fluidRow(
                                                     column(
                                                       width = 3,
                                                       
                                                       # Shading
                                                       helpText('For shading, please use a value of 0 if you are using the One-Axis Tracking array type because PVWatts uses the value of GCR to calculate this.'),
                                                       numericInput(inputId = "shading",
                                                                    label = "Shading (%):", 
                                                                    width = '200px',
                                                                    value = '3',
                                                                    min=0,
                                                                    max=100),
                                                       bsTooltip(id = 'shading', 
                                                                 "Reduction in the incident solar radiation from shadows caused by objects near the array such as buildings or trees, or by self-shading for modules arranged in rows when modules in one row cause shadows on those in an adjacent row.",
                                                                 'right',
                                                                 options = list(container = 'body'))
                                                       
                                                     ),
                                                     column(
                                                       width = 3,
                                                       
                                                       # Soiling
                                                       numericInput(inputId = "soiling",
                                                                    label = "Soiling (%):", 
                                                                    width = '200px',
                                                                    value = '2',
                                                                    min=0,
                                                                    max=100),
                                                       bsTooltip(id = 'soiling', 
                                                                 "Losses due to dirt and other foreign matter on the surface of the PV module that prevent solar radiation from reaching the cells.",
                                                                 'right',
                                                                 options = list(container = 'body')),
                                                       
                                                       
                                                       # Snow
                                                       numericInput(inputId = "snow",
                                                                    label = "Snow (%):", 
                                                                    width = '200px',
                                                                    value = '0',
                                                                    min=0,
                                                                    max=100),
                                                       bsTooltip(id = 'snow', 
                                                                 "Reduction in the system's annual output due to snow covering the array.",
                                                                 'right',
                                                                 options = list(container = 'body')),
                                                       
                                                       
                                                       # Mismatch
                                                       numericInput(inputId = "mismatch",
                                                                    label = "Mismatch (%):", 
                                                                    width = '200px',
                                                                    value = '2',
                                                                    min=0,
                                                                    max=100),
                                                       bsTooltip(id = 'mismatch', 
                                                                 "Electrical losses due to slight differences caused by manufacturing imperfections between modules in the array that cause the modules to have slightly different current-voltage characteristics.",
                                                                 'right',
                                                                 options = list(container = 'body'))
                                                       
                                                     ),
                                                     column(
                                                       width= 3,
                                                       
                                                       # Wiring
                                                       numericInput(inputId = "wiring",
                                                                    label = "Wiring (%):", 
                                                                    width = '200px',
                                                                    value = '2',
                                                                    min=0,
                                                                    max=100),
                                                       bsTooltip(id = 'wiring', 
                                                                 "Resistive losses in the DC and AC wires connecting modules, inverters, and other parts of the system.",
                                                                 'right',
                                                                 options = list(container = 'body')),
                                                       
                                                       # Connections
                                                       numericInput(inputId = "connections",
                                                                    label = "Connections (%):", 
                                                                    width = '200px',
                                                                    value = '0.5',
                                                                    min=0,
                                                                    max=100),
                                                       bsTooltip(id = 'connections', 
                                                                 "Resistive losses in electrical connectors in the system.",
                                                                 'right',
                                                                 options = list(container = 'body')),
                                                       
                                                       # Light-Induced Degradation
                                                       numericInput(inputId = "light_induc_deg",
                                                                    label = "LI Degradation (%):", 
                                                                    width = '200px',
                                                                    value = '1.5',
                                                                    min=0,
                                                                    max=100),
                                                       bsTooltip(id = 'light_induc_deg', 
                                                                 "LI (light-induced) degradation of photovoltaic cells causes the effect of the reduction in the array's power during the first few months of its operation.",
                                                                 'right',
                                                                 options = list(container = 'body'))
                                                       
                                                     ),
                                                     column(
                                                       width = 3,
                                                       
                                                       # Nameplate Rating
                                                       numericInput(inputId = "name_p_rating",
                                                                    label = "Nameplate Rating (%):", 
                                                                    width = '200px',
                                                                    value = '1',
                                                                    min=0,
                                                                    max=100),
                                                       bsTooltip(id = 'name_p_rating', 
                                                                 "The nameplate rating loss accounts for the accuracy of the manufacturer's nameplate rating. Field measurements of the electrical characteristics of photovoltaic modules in the array may show that they differ from their nameplate rating.",
                                                                 'right',
                                                                 options = list(container = 'body')),
                                                       
                                                       # Age 
                                                       numericInput(inputId = "age",
                                                                    label = "Age (%):", 
                                                                    width = '200px',
                                                                    value = '0',
                                                                    min=0,
                                                                    max=100),
                                                       bsTooltip(id = 'age', 
                                                                 "Effect of weathering of the photovoltaic modules on the array's performance over time.",
                                                                 'right',
                                                                 options = list(container = 'body')),
                                                       
                                                       # Availability
                                                       numericInput(inputId = "availability",
                                                                    label = "Availability (%):", 
                                                                    width = '200px',
                                                                    value = '3',
                                                                    min=0,
                                                                    max=100),
                                                       bsTooltip(id = 'availability', 
                                                                 "Reduction in the system's output cause by scheduled and unscheduled system shutdown for maintenance, grid outages, and other operational factors.",
                                                                 'right',
                                                                 options = list(container = 'body'))
                                                       
                                                     )
                                                   ),
                                                   
                                                   
                                                   textOutput('losses2')
                                                

                                  ),
                                  
                                  # Non-Specific losses
                                  conditionalPanel(condition = "input.loss_type == 'Non-Specific'",
                                                   
                                                   numericInput(inputId = 'losses',
                                                                label = 'Losses (%):',
                                                                width = '200px',
                                                                value = '14',
                                                                min=-5,
                                                                max=99),
                                                   bsTooltip('losses', 
                                                             "Estimated total system losses.",
                                                             'right',
                                                             options = list(container = 'body')),
                                                   
                                                   
                                  )),
                          
                          # GCR input
                          numericInput(inputId = 'gcr',
                                       label = 'Ground Coverage Ratio:',
                                       width = '200px',
                                       value = '0.4',
                                       min=0.01,
                                       max=0.99),
                          bsTooltip('gcr', 
                                    "The ratio of module surface area to the area of the ground or roof occupied by the array. Typical values range from 0.3 to 0.6.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          # DC-AC ratio input
                          numericInput(inputId = 'dc_ac_ratio',
                                       label = 'DC to AC Ratio:',
                                       width = '200px',
                                       value = '1.2',
                                       min=0),
                          bsTooltip('dc_ac_ratio', 
                                    "The ratio of installed DC capacity to the inverters AC power rating.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          helpText("Please reference the 'More Information' tab to understand the differences in module and array types."),
                          
                          # Array type input
                          radioButtons(inputId = 'array_type', 
                                       label = 'Array Type:', 
                                       choiceNames = c('Fixed - Open Rack','Fixed - Roof Mounted','1-Axis','Backtracked','2-Axis'), 
                                       choiceValues = c(0,1,2,3,4)),
                          
                          # Module type input
                          radioButtons(inputId = 'module_type', 
                                       label = 'Module Type:', 
                                       choiceNames = c('Standard: Polycrystalline','Premium: Monocrystalline','Thin film'), 
                                       choiceValues = c(0,1,2)),
                          
                          # Bifaciality input
                          helpText('Please use a value of 0 for bifaciality if you are using a Fixed Roof Mount array type because bifacial modules require space between the module and roof surface to allow sunlight to reach the back of the module.'),
                          numericInput(inputId = 'bifaciality', 
                                       label = 'Bifaciality:',
                                       width = '200px',
                                       value = '0',
                                       min=0,
                                       max=1),
                          bsTooltip('bifaciality', 
                                    "The ratio of rear-side efficiency to front-side efficiency. Typically a value between 0.65 and 0.9.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          # Module efficiency input
                          numericInput(inputId = 'module_efficiency',
                                       label = 'Module Efficiency (%):',
                                       width = '200px',
                                       value = '18',
                                       min=0,
                                       max=100),
                          bsTooltip('module_efficiency', 
                                    "The efficiency of the system module for converting sunlight into usable energy. Please enter this number as a percent and not a decimal.",
                                    'right',
                                    options = list(container = 'body')),
                          
                          # System lifetime inputs
                          h3("Lifetime & Degradation"),
                          
                          radioButtons(inputId = 'system_use_lifetime_output', 
                                       label = 'Use lifetime simulation?:', 
                                       choiceNames = c('No','Yes'), 
                                       choiceValues = c(0,1)),
                          
                          conditionalPanel(condition = "input.system_use_lifetime_output == 1",
                                           helpText('Using the system lifetime factor will only show the adjusted annual energy output per year. All other monthly and hourly outputs and plots will be based on the first year of the analysis period without applying degradation.'),
                                           numericInput(inputId = 'analysis_period',
                                                        label = 'Analysis Period (years):',
                                                        width = '200px',
                                                        value = '0',
                                                        min = 0),
                                           bsTooltip('system_lifetime', 
                                                     "The number of years you want to use as the analysis period for the simulation or how long the solar panel system will last.",
                                                     'right',
                                                     options = list(container = 'body')),
                                           
                                           textInput(inputId = 'degradation_rate',
                                                     label = 'Degradation Rate (%/year):',
                                                     width = '200px',
                                                     value = '0'),
                                           bsTooltip('degradation_rate', 
                                                     "The percent per year a panel decreases energy production over time. If the rate changes each year, please enter the rates separated by a comma and a space like this format `0.5, 0.6, 0.7` matching each analysis period year, for instance, that format would use 3 years.",
                                                     'right',
                                                     options = list(container = 'body'))),
                          
                          
                          helpText("Please wait a moment for the application to simulate and generate outputs. If you change input values, please click the button again."),
                          
                          actionButton(inputId = "SimulateButton", 
                                       label = "Simulate",
                                       width = '100px',
                                       icon("arrows-rotate"))
                          
                        ),
                        mainPanel(
                          downloadButton('download_tables', 'Download Tables'),
                          helpText('Downloading the table may take a few moments.'),
                          br(),
                          tabsetPanel(
                            
                            tabPanel("Annual Outputs",
                                     br(),
                                     br(),
                                     br(),
                                     fluidRow(
                                       column(4,
                                              HTML(paste0("<b>","Energy Generated:","</b>")),
                                              br(),
                                              tableOutput('ac_annual_out'),
                                              textOutput('lifetime_energy'),
                                              br(),
                                              br(),
                                              HTML(paste0("<b>","Solar Radiation:","</b>")),
                                              textOutput('solrad_annual_out'),
                                              br(),
                                              br(),
                                              HTML(paste0("<b>","Capacity Factor:","</b>")),
                                              textOutput('capacity_factor_out')),
                                       column(4,
                                              HTML(paste0("<b>","State of Weather Station:","</b>")),
                                              textOutput('station_state_out'),
                                              br(),
                                              br(),
                                              HTML(paste0("<b>","Elevation of Weather Station:","</b>")),
                                              textOutput('station_elev_out'))
                                         )
                                      ),

                            tabPanel("Monthly Outputs",
                                     br(),
                                     tableOutput("monthly_outputs"),
                                     hr(),
                                     plotlyOutput(outputId = "monthly_energy_plot", height = 'auto', width = 'auto'),
                                     hr(),
                                     plotlyOutput(outputId = "monthly_poa_plot", height = 'auto', width = 'auto'),
                                     hr(),
                                     plotlyOutput(outputId = "monthly_solrad_plot", height = 'auto', width = 'auto')),
                            
                            tabPanel("Hourly Outputs",
                                     br(),
                                     DT::dataTableOutput("hourly_outputs"),
                                     hr(),
                                     plotlyOutput(outputId = "daily_plot", height = 'auto', width = 'auto'),
                                     hr(),
                                     verbatimTextOutput("hourly_desc")
                            )
                            
                            
                          )
                        )
                      )),
             
             # Financial analysis tab
             # tabPanel("Financial Model", icon = icon("dollar-sign"),
             #          sidebarLayout(
             #            sidebarPanel(
             #              titlePanel("Financial Inputs"),
             #              
             #              h3("Time Measures"),
             #              
             #              numericInput(inputId = 'system_lifetime',
             #                           label = 'System Lifetime (years):',
             #                           width = '200px',
             #                           value = '25',
             #                           min = 0),
             #              
             #              numericInput(inputId = 'time_horizon',
             #                           label = 'Time Horizon (years):',
             #                           width = '200px',
             #                           value = '10',
             #                           min = 0000,
             #                           max = 100),
             #              
             #              
             #              h3("Installation & Costs"),
             #              
             #              numericInput(inputId = 'system_cost',
             #                           label = 'System Cost ($):',
             #                           width = '200px',
             #                           value = '15000',
             #                           min = 0),
             #              
             #              numericInput(inputId = 'installation_perc',
             #                           label = 'Installation Cost (% of capital cost):',
             #                           width = '200px',
             #                           value = '20',
             #                           min = 0),
             #              
             #              numericInput(inputId = 'subsidies',
             #                           label = 'Subsidies (%):',
             #                           width = '200px',
             #                           value = '25',
             #                           min = 0),
             #              
             #              h3("Rates & Demand"),
             #              
             #              numericInput(inputId = 'energy_demand',
             #                           label = 'Annual Energy Demanded (kWh):',
             #                           width = '200px',
             #                           value = '12000',
             #                           min = 0),
             #              
             #              numericInput(inputId = 'elec_rate',
             #                           label = 'Electricity Rate ($/kWh):',
             #                           width = '200px',
             #                           value = '0.18',
             #                           min = 0),
             #              
             #              helpText('These rate changes below should be based on annual averages.'),
             #              numericInput(inputId = 'elec_price_change',
             #                           label = 'Electricity Price Change (%):',
             #                           width = '200px',
             #                           value = '1.8',
             #                           min = 0),
             #              
             #              numericInput(inputId = 'elec_demand_change',
             #                           label = 'Electricity Demand Change (%):',
             #                           width = '200px',
             #                           value = '1',
             #                           min = 0),
             #              
             #              numericInput(inputId = 'panel_deg',
             #                           label = 'Solar Panel Degradation (%):',
             #                           width = '200px',
             #                           value = '0.5',
             #                           min = 0),
             #              
             #              
             #              
             #              
             #              actionButton(inputId = "SimulateButton2", 
             #                           label = "Simulate",
             #                           width = '100px',
             #                           icon("arrows-rotate"))
             #              
             #              ),
             #                         
             #            
             #            mainPanel(
             #              
             #              downloadButton('download_table2', 'Download Table'),
             #              helpText('Downloading the table may take a few moments.'),
             #              br(),
             #              
             #              fluidRow(
             #                column(4,
             #                       HTML(paste0("<b>","Number of Optimal Panels:","</b>")),
             #                       br(),
             #                       textOutput('opt_panels'),
             #                       br(),
             #                       br(),
             #                       HTML(paste0("<b>","Energy Purchased:","</b>")),
             #                       textOutput('energy_purchased'),
             #                       br(),
             #                       br(),
             #                       HTML(paste0("<b>","Energy Provided:","</b>")),
             #                       textOutput('energy_provided'),
             #                       br(),
             #                       br(),
             #                       HTML(paste0("<b>","Lifetime Savings from Solar:","</b>")),
             #                       textOutput('solar_savings')),
             #                
             #                column(4,
             #                       HTML(paste0("<b>","Average Yearly Optimal System Cost:","</b>")),
             #                       textOutput('opt_cost'),
             #                       br(),
             #                       br(),
             #                       HTML(paste0("<b>","Total Grid Cost:","</b>")),
             #                       textOutput('grid_cost'),
             #                       br(),
             #                       br(),
             #                       HTML(paste0("<b>","Lifetime Levelized Cost:","</b>")),
             #                       textOutput('levelized_cost'),
             #                       br(),
             #                       br(),
             #                       HTML(paste0("<b>","Total Cost:","</b>")),
             #                       textOutput('total_cost'))
             #                
             #              ),
             #              
             #              br(),
             #              br(),
             #              DT::dataTableOutput("financial_data")
             #              
             #            ))
             #          
             #          ),
             
             # More information tab
             tabPanel("More Information", icon = icon("book"),
                      sidebarLayout(
                        sidebarPanel(h4("Sources"),
                                     actionLink('nrel', label = 'National Renewable Energy Laboratory (NREL)', icon = icon("link"), onclick ="window.open(href='https://www.nrel.gov/');"),
                                     br(),
                                     actionLink('nsrdb', label = 'National Solar Radiation Database (NSRDB)', icon = icon("link"), onclick ="window.open(href='https://nsrdb.nrel.gov/');"),
                                     br(),
                                     actionLink('sam', label = 'System Advisor Model (SAM)', icon = icon("link"), onclick ="window.open(href='https://sam.nrel.gov/');"),
                                     br(),
                                     actionLink('pvwatts', label = 'PVWatts', icon = icon("link"), onclick ="window.open(href='https://pvwatts.nrel.gov/pvwatts.php');"),
                                     
                                     h3("Licenses"),
                                     p("BSD 3-Clause"),
                                     p("MIT License"),
                                     
                                     h3("Creator"),
                                     p("Daniel Saul"),
                                     p(em("Student Assistant - University of Georgia")),
                                     actionLink('github', label = 'GitHub', icon = icon("github"), onclick ="window.open(href='https://github.com/DanielPSaul/SolarPanelApp');"),
                                     br(),
                                     actionLink('linkedin', label = 'LinkedIn', icon = icon("linkedin"), onclick ="window.open(href='https://www.linkedin.com/in/danielsaul1/');"),
                                     br(),
                                     actionLink('Email', label = 'Email', icon = icon("envelope"), onclick ="location.href='mailto:danielsaul@me.com';")),
                        
                        mainPanel(width = 8, 
                                  h4("Data Dictionary"),
                                  DT::dataTableOutput("more_info_table")),
                        position = c("right"),
                        fluid = TRUE)
                      ),
             
             # Quit tab (for restarting the session/app)
             tabPanel(title = "Restart", 
                      icon = icon("circle-xmark"),
                      actionButton("reset_session", "Restart Session")
             )
             
  )
)