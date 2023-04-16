################################################################################
#                                   SERVER
################################################################################

server <- function(input, output, session) {
  
  # Make this reactive and able to use across pages
  ac_annual_val <- reactiveVal()
  
  

  
  observeEvent(input$reset_session, {
    session$reload()
  })
  
  # The definitions included in the more information tab
  output$more_info_table <- DT::renderDataTable(more_info,
                                                options = list(autoWidth = TRUE,
                                                               pageLength = 3),
                                                rownames = FALSE)
  
  # Initialize variables for API calls
  api_key <- '9UUX1nAVhZoj90XY9R1QHD4U5foWHVABoQxlbnxt'
  timeframe <- 'hourly'
  dataset <- 'nsrdb'
  interval <- '60'
  email <- 'danielsaul@uga.edu'
  names <- 'tmy-2020'
  use_wf_albedo <- 1
  
  # Render leaflet weather map in the Location tab
  output$weather_map <- renderLeaflet({
    
    leaflet() %>%
      setView(-83.37, 33.94, 12) %>%
      addTiles()
    
  })
  
  
  # Get weather data and table
  observeEvent(input$WeatherDataButton, {
    shinyalert("Gathering Data...", showConfirmButton = FALSE, timer = 0)
    
    # Convert Zipcode input to latitude and longitude
    if (input$location_type == "Zipcode") {
      
      codes <- geocode_zip(input$zipcode)
      
      lat <- codes[[2]]
      lon <- codes[[3]]
      
      output$lat <- renderText({
        paste(lat, "degrees")
      })
      
      output$lon <- renderText({
        paste(lon, "degrees")
      })
      
    } else {
      
      lat = input$lat
      lon = input$lon
      
    }
    
    # Update map with new location values
    leafletProxy('weather_map') %>%
      clearMarkers() %>%
      setView(lon, lat, 12) %>%
      addMarkers(lon, lat)
    
    # Retrieve weather data from NSRDB
    url <- paste0('https://developer.nrel.gov/api/nsrdb/v2/solar/psm3-tmy-download.csv?api_key=',api_key,'&wkt=POINT(', lon, '+', lat, ')','&names=',names,'&email=',email)
    data <- read.csv(url, skip = 2)
    info <- read.csv(url, nrows = 1)
    
    data$Timestamp <- ymd_hms(paste0(data$Year, "-", data$Month, "-", data$Day, " ", data$Hour, ":", data$Minute, ":00"))
    rownames(data) <- data$Timestamp
    
    weather_data <- data %>%
      subset(., select=c('Dew.Point', 'DHI', 'DNI', 'GHI', 'Surface.Albedo', 'Pressure', 'Temperature', 'Wind.Direction', 'Wind.Speed')) %>%
      rename(., 'Dew Point' = 'Dew.Point',
             'Surface Albedo' = 'Surface.Albedo',
             'Wind Direction' = 'Wind.Direction',
             'Wind Speed' = 'Wind.Speed')
    
    output$weather_table <- DT::renderDataTable(weather_data,
                                                options = list(autoWidth = TRUE,
                                                               pageLength = 5),
                                                rownames = TRUE)
    
    # Weather data irradiance plot
    output$irradiance_plot <- renderPlotly({
      
      new_df <- data %>%
        group_by(Month = month(Month)) %>%
        summarise(DHI = mean(DHI), DNI = mean(DNI), GHI = mean(GHI))
      
      new_df$Months <-c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
      
      theme_set(theme_classic())
      p3 <- ggplot(data = new_df, aes(x = factor(Months, level=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')))) +
        geom_line(aes(y = DHI, group=1, colour = "DHI")) +
        geom_point(aes(y = DHI, colour = "DHI", text = paste("Month:", Months, 
                                                             "\nIrradiance Type: DHI",
                                                             "\nAvg. DHI:", round(DHI,2)))) +
        
        geom_line(aes(y = DNI, group=1, colour = "DNI")) +
        geom_point(aes(y = DNI, colour = "DNI", text = paste("Month:", Months, 
                                                             "\nIrradiance Type: DNI",
                                                             "\nAvg. DNI:", round(DNI,2)))) +
        
        geom_line(aes(y = GHI, group=1, colour = "GHI")) +
        geom_point(aes(y = GHI, colour = "GHI", text = paste("Month:", Months, 
                                                             "\nIrradiance Type: GHI",
                                                             "\nAvg. GHI:", round(GHI,2)))) +
        
        labs(title = "Monthly Average Irradiance", x = "", y = "Watts per Square Meter", color='Irradiance Type') +
        theme(axis.text.x=element_text(angle=60, hjust=1))
      
      p3 <- ggplotly(p3, tooltip = "text")
      
    })
    
    
    output$download_table <- downloadHandler(
      filename = function() {
        paste("Weather-Data-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        

        write.xlsx(weather_data, file, col.names = TRUE, row.names = TRUE, showNA = FALSE)

      }
    )
    
    shinyalert("Gathering Complete.", showConfirmButton = TRUE, type = "success", immediate = TRUE)
    
  }) # End of weather data gathering tab
  
  
  
  # Simulation tab running when button is clicked
  observeEvent(input$SimulateButton, {
    shinyalert("Running Simulation...", showConfirmButton = FALSE, timer = 0)
    
    # Convert Zipcode input to latitude and longitude
    if (input$location_type == "Zipcode") {
      
      codes <- geocode_zip(input$zipcode)
      
      lat <- codes[[2]]
      lon <- codes[[3]]
      
    } else {
      
      lat = input$lat
      lon = input$lon
      
    }
    
    # Establish inputs
    tilt <- isolate(input$tilt) 
    azimuth <- isolate(input$azimuth) 
    inv_eff <-  isolate(input$inv_eff) 
    array_type <- as.numeric(isolate(input$array_type))
    gcr <- isolate(input$gcr) 
    module_type <- as.numeric(isolate(input$module_type)) 
    dc_ac_ratio <- isolate(input$dc_ac_ratio)
    bifaciality <- isolate(input$bifaciality)
    module_efficiency <- as.numeric(isolate(input$module_efficiency)/100)
    
    # Greenhouse inputs
    roof_length <- as.numeric(isolate(input$roof_length))
    roof_width <- as.numeric(isolate(input$roof_width))
    panel_coverage <- as.numeric(isolate(input$panel_coverage)/100)
    roof_area <- roof_length * roof_width
    panelcov_area <- roof_area * panel_coverage
    
    # Losses
    
    if (input$loss_type == 'Specific') {
      
      soiling <- as.numeric(isolate(input$soiling))/100
      shading <- as.numeric(isolate(input$shading))/100
      snow <- as.numeric(isolate(input$snow))/100
      mismatch <- as.numeric(isolate(input$mismatch))/100
      wiring <- as.numeric(isolate(input$wiring))/100
      connections <- as.numeric(isolate(input$connections))/100
      light_induc_deg <- as.numeric(isolate(input$light_induc_deg))/100
      name_p_rating <- as.numeric(isolate(input$name_p_rating))/100
      age <- as.numeric(isolate(input$age))/100
      availability <- as.numeric(isolate(input$availability))/100
      
      losses = 100 * (1-(1-soiling)*(1-shading)*(1-snow)*(1-mismatch)*(1-wiring)*(1-connections)*(1-light_induc_deg)*(1-name_p_rating)*(1-age)*(1-availability))
      
    } else {
      
      losses <- round(input$losses,2)
      
    }

    # Losses output
    output$losses2 <- renderText({
      paste0("Total Losses: ", losses, "%")
    })
    
    # Various small outputs and calculations
    output$roof_area_out <- renderText({
      paste(roof_area, "meters\u00B2")
    })
    
    output$panelcov_area_out <- renderText({
      paste(panelcov_area, "meters\u00B2")
    })
    
    
    system_capacity <- panelcov_area * 1 * module_efficiency
    
    output$system_capacity_out <- renderText({
      paste(system_capacity, "kW")
    })
    
    # Run the PVWatts simulation
    solar_data_api_outputs <- jsonlite::fromJSON(paste0('https://developer.nrel.gov/api/pvwatts/v8.json?api_key=',api_key,'&lat=',lat,'&lon=',lon,'&system_capacity=',system_capacity,'&azimuth=',azimuth,'&tilt=',tilt,'&array_type=',array_type,'&module_type=',module_type,'&losses=',losses,'&dc_ac_ratio=',dc_ac_ratio,'&gcr=',gcr,'&inv_eff=',inv_eff,'&timeframe=',timeframe,'&dataset=',dataset,'&use_wf_albedo=',use_wf_albedo, '&bifaciality=',bifaciality))
    
    # Get annual outputs
    station_elev <- round(solar_data_api_outputs[["station_info"]][["elev"]], 2)
    station_state <- solar_data_api_outputs[["station_info"]][["state"]]
    ac_annual <- round(solar_data_api_outputs[["outputs"]][["ac_annual"]], 2)
    ac_annual_val <- ac_annual_val(ac_annual) # for use in other pages
    solrad_annual <- round(solar_data_api_outputs[["outputs"]][["solrad_annual"]], 2)
    capacity_factor <- round(solar_data_api_outputs[["outputs"]][["capacity_factor"]], 2)
    
    # System lifetime factors
    if (input$system_use_lifetime_output == 1) {
      
      # Lifetime inputs if system lifetime is included
      analysis_period <- as.numeric(isolate(input$analysis_period))
      degradation_rate <- as.numeric(strsplit(isolate(input$degradation_rate), ", ")[[1]])
      
      if (length(degradation_rate) > 1) {
        
        years <- seq(1, analysis_period, by=1)
        energy_output <- rep(0, length(years))
        for (i in 1:length(years)) {
          energy_output[i] <- ac_annual*prod((1-degradation_rate[1:i]/100))
        }
        
      } else {
        
        years <- seq(1, analysis_period, by=1)
        energy_output <- rep(0, length(years))
        for (i in 1:length(years)) {
          energy_output[i] <- ac_annual*(1-panel_degradation_rate/100)^years[i]
        }
        
        }
        
      
      
      deg_table <- data.frame(energy_output)
      deg_table <- rownames_to_column(deg_table, var = "index")
      colnames(deg_table) <- c('Period', 'kWh')
      deg_table2 <- deg_table
      deg_table2$kWh <- sapply(deg_table2$kWh, FUN=function(x) prettyNum(x, big.mark = ","))
      
      # output code (table of annual energy output per period)
      output$ac_annual_out <- renderTable(deg_table2)
      output$lifetime_energy <- renderText({
        summed <- sum(deg_table$kWh)
        summed <- prettyNum(summed, big.mark = ",")
        paste('Total:', summed, 'kWh')
        
      })
      
    } else {
      
      
      # output code (single annual energy output)
      output$ac_annual_out <- renderText({
        
        ac_annual <- prettyNum(ac_annual, big.mark = ",")
        paste0(ac_annual, " kWh")
        
      })
      
    }
    
    
    # Show general api outputs
    output$solrad_annual_out <- renderText({
      paste0(solrad_annual, " kWh/m\u00B2/day")
    })
    
    output$capacity_factor_out <- renderText({
      paste0(capacity_factor, "%")
    })
    
    output$station_state_out <- renderText({
      paste0(station_state, ", United States")
    })
    
    output$station_elev_out <- renderText({
      paste0(station_elev, " meters")
    })
    
    # Get monthly outputs
    ac_monthly <- round(solar_data_api_outputs[["outputs"]][["ac_monthly"]], 2)
    poa_monthly<- round(solar_data_api_outputs[["outputs"]][["poa_monthly"]], 2)
    solrad_monthly <- round(solar_data_api_outputs[["outputs"]][["solrad_monthly"]], 2)
    dc_monthly <- round(solar_data_api_outputs[["outputs"]][["dc_monthly"]], 2)
    
    # Monthly output table preparation
    months = c('January','February','March','April','May','June','July','August','September','October','November','December')
    monthly_output <- cbind(months, ac_monthly, poa_monthly, solrad_monthly, dc_monthly)
    monthly_output <- data.frame(monthly_output)
    colnames(monthly_output) <- c('Month', 'AC Energy (kWh)', 'Plane of Array (kWh/m\u00B2)', 'Solar Radiation (kWh/m\u00B2/day)', 'DC Energy (kWh)')
    
    monthly_output2 <- monthly_output
    monthly_output2$`AC Energy (kWh)` <- sapply(monthly_output2$`AC Energy (kWh)`, FUN=function(x) prettyNum(x, big.mark = ","))
    monthly_output2$`DC Energy (kWh)` <- sapply(monthly_output2$`DC Energy (kWh)`, FUN=function(x) prettyNum(x, big.mark = ","))
    
    output$monthly_outputs <- renderTable(monthly_output2)
    
    
    # Monthly output line plot
    output$monthly_energy_plot <- renderPlotly({
      
      rownames(monthly_output) <- monthly_output$Month
      theme_set(theme_classic())

      p <- ggplot(data = monthly_output, aes(x = factor(Month, level=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')))) +
        geom_line(aes(y = as.numeric(monthly_output[[2]]), group=1, colour = 'AC Energy (kWh)')) +
        geom_point(aes(y = as.numeric(monthly_output[[2]]), colour = 'AC Energy (kWh)', text = paste("Month:", Month,
                                                                                                     "\nAvg. AC Energy:", as.numeric(monthly_output[[2]])))) +
        geom_line(aes(y = as.numeric(monthly_output[[5]]), group=1, colour = 'DC Energy (kWh)')) +
        geom_point(aes(y = as.numeric(monthly_output[[5]]), colour = 'DC Energy (kWh)', text = paste("Month:", Month,
                                                                                                     "\nAvg. DC Energy:", as.numeric(monthly_output[[5]])))) +
        labs(title = "Monthly Average Energy Output", x = "", y = "Kilowatt-Hours", color='Energy Type') +
        theme(axis.text.x=element_text(angle=60, hjust=1))

      p <- ggplotly(p, tooltip = 'text')

    })
    
    
    
    # POA plot
    output$monthly_poa_plot <- renderPlotly({
      
      theme_set(theme_classic())
      poa <- ggplot(data = monthly_output, aes(x = factor(Month, level=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')), y = as.numeric(monthly_output[[3]]), text = paste("Month:", Month,
                                                                                                                                                                                                                                                         "\nAvg. POA:", as.numeric(monthly_output[[3]])))) +
        geom_bar(stat = 'identity') +
        labs(title = "Monthly Plane of Array Irradiance", x = "", y = "Kilowatt-Hours/Meters\u00B2") +
        theme(axis.text.x=element_text(angle=60, hjust=1))
      
      poa <- ggplotly(poa, tooltip = 'text')
      
    })
    
    # Solar Radiation plot
    output$monthly_solrad_plot <- renderPlotly({
      
      theme_set(theme_classic())
      solrad_p <- ggplot(monthly_output, aes(x=factor(Month, level=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')), y=as.numeric(monthly_output[[4]]), text = paste("Avg. Solar Radiation:", as.numeric(monthly_output[[4]])))) + 
        geom_point(col="tomato2", size=3) +   # Draw points
        geom_segment(aes(x=factor(Month, level=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')), 
                         xend=factor(Month, level=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')), 
                         y=min(as.numeric(monthly_output[[4]])), 
                         yend=max(as.numeric(monthly_output[[4]]))), 
                     linetype="dashed", 
                     size=0.1) +   # Draw dashed lines
        labs(title = "Monthly Average Solar Radiation", x = "Kilowatt-Hours/Meters\u00B2/Day", y = "") +
        coord_flip()
      
      solrad_p <- ggplotly(solrad_p, tooltip = 'text')
      
    })
    
    # Hourly Outputs
    # Get hourly outputs
    ac_hourly <- round(solar_data_api_outputs[["outputs"]][["ac"]], 2)
    dc_hourly <- round(solar_data_api_outputs[["outputs"]][["dc"]], 2)
    poa_hourly <- round(solar_data_api_outputs[["outputs"]][["poa"]], 2)
    tamb_hourly <- solar_data_api_outputs[["outputs"]][["tamb"]]
    tcell_hourly <- solar_data_api_outputs[["outputs"]][["tcell"]]
    
    # Create hourly outputs dataframe for table and plot usage
    hourly_output <- cbind(ac_hourly, dc_hourly, poa_hourly, tamb_hourly, tcell_hourly)
    hourly_output <- data.frame(hourly_output)
    
    url <- paste0('https://developer.nrel.gov/api/nsrdb/v2/solar/psm3-tmy-download.csv?api_key=',api_key,'&wkt=POINT(', lon, '+', lat, ')','&names=',names,'&email=',email)
    data <- read.csv(url, skip = 2)
    data$Timestamp <- ymd_hms(paste0(data$Year, "-", data$Month, "-", data$Day, " ", data$Hour, ":", data$Minute, ":00"))
    hourly_output$Timestamp <- data$Timestamp
    rownames(hourly_output) <- hourly_output$Timestamp
    
    hourly_output <- subset(hourly_output, select = -c(Timestamp))
    
    colnames(hourly_output) <- c('AC Energy (W)', 'DC Energy (W)', 'Plane of Array (W/m\u00B2)', 'Ambient Temperature (Celsius)', 'Cell Temperature (Celsius)')
    
    # Hourly outputs data table
    output$hourly_outputs <- DT::renderDataTable(hourly_output,
                                                 options = list(autoWidth = TRUE,
                                                                pageLength = 5),
                                                 rownames = TRUE)
    
    # Hourly/Daily output line plot
    output$daily_plot <- renderPlotly({

      hour_plot_df <- hourly_output %>%
        group_by(Day = yday(row.names(hourly_output))) %>%
        summarise(`AC Energy (W)` = round(mean(`AC Energy (W)`), 2))

      p2 <- ggplot(data = hour_plot_df, aes(x = Day, y = `AC Energy (W)`)) +
        geom_line(aes(group=1)) +
        geom_point() +
        labs(title = "Daily Average Energy Output", x = "Day of Year", y = "Watts")

      p2 <- ggplotly(p2)

    })
    
    # Hourly descriptive statistics
    output$hourly_desc <- renderPrint({
      
      summary_data <- summary(hourly_output)
      summary_data
      
    })
    
    
    output$download_tables <- downloadHandler(
      filename = function() {
        paste("Panel-Outputs-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        
        hourly_output$Timestamp <- data$Timestamp
        write_xlsx(list("Annual" = deg_table, "Monthly" = monthly_output, "Hourly" = hourly_output), file, col_names = TRUE, format_headers = TRUE)
        #write.xlsx(weather_data, file, col.names = TRUE, row.names = TRUE, showNA = FALSE)
        
      }
    )
    
    shinyalert("Simulation Complete.", showConfirmButton = TRUE, type = "success", immediate = TRUE)
  })
  
  
  # Financial calculations
  # observeEvent(input$SimulateButton2, {
  #   
  #   shinyalert("Running Model...", showConfirmButton = FALSE, timer = 0)
  #   
  #   # Get inputs
  #   system_lifetime <- as.numeric(isolate(input$system_lifetime))
  #   time_horizon <- as.numeric(isolate(input$time_horizon))
  #   installation_perc <- as.numeric(isolate(input$installation_perc))/100
  #   system_cost <- as.numeric(isolate(input$system_cost))
  #   subsidies <- as.numeric(isolate(input$subsidies))/100
  #   energy_demand <- as.numeric(isolate(input$energy_demand))
  #   elec_rate <- as.numeric(isolate(input$elec_rate))
  #   elec_price_change <- as.numeric(isolate(input$elec_price_change))/100
  #   elec_demand_change <- as.numeric(isolate(input$elec_demand_change))/100
  #   panel_deg <- as.numeric(isolate(input$panel_deg))/100
  #   
  #   # Basic calculations
  #   ac_annual2 <- ac_annual_val()
  #   capital_cost <- (1-subsidies) * system_cost # Capital cost reduced by subsidy amount
  #   installation_cost <- installation_perc * system_cost
  #   
  #   years <- c(1:ceiling(time_horizon)) #system_life
  #   panel_ac_output <- ac_annual2
  #   panel_cost_per_year <- (capital_cost + installation_cost) / system_lifetime
  #   
  #   # number of periods
  #   n <- time_horizon #system_life
  #   
  #   # Panel costs and change over time
  #   Pinit <- panel_cost_per_year
  #   Prate <- 1
  #   
  #   # Electricity cost and change over time
  #   Einit <- elec_rate
  #   Erate <- (1 + elec_price_change)
  #   
  #   # Demand for electricity and change over time
  #   Dinit <- energy_demand
  #   Drate <- (1 + elec_demand_change)
  #   
  #   # Electricity generated per panel and change over time
  #   Ginit <- panel_ac_output
  #   Grate <- (1-panel_deg) #degradation rate
  #   
  #   # Set up the vectors for the period
  #   P <- vector("numeric", n)
  #   P[1] <- Pinit
  #   for(i in 2:n) {
  #     P[i] <- P[i-1]*Prate
  #   }
  #   E <- vector("numeric", n)
  #   E[1] <- Einit
  #   for(i in 2:n) {
  #     E[i] <- E[i-1]*Erate
  #   }
  #   D <- vector("numeric", n)
  #   D[1] <- Dinit
  #   for(i in 2:n) {
  #     D[i] <- D[i-1]*Drate
  #   }
  #   G <- vector("numeric", n)
  #   G[1] <- Ginit
  #   for(i in 2:n) {
  #     G[i] <- G[i-1]* Grate
  #   }
  #   
  #   # Compute the number of panels and purchased kilowatt hours
  #   f.obj <- rbind(P,E)
  #   f.obj <-  as.vector(f.obj)
  #   
  #   f.con <- matrix(0,nrow = n, ncol = n*2)
  #   for (i in 1:n) {
  #     j <-(i-1)*2 + 1
  #     f.con[i,j] <-  G[(i+1)/2]
  #     f.con[i,j+1] <- 1
  #   }
  #   
  #   f.rhs <- D
  #   
  #   f.eq <- c(rep(">=",n))
  #   int.vec <- seq(1,n*2,2)
  #   
  #   results <- lp('min', f.obj, f.con, f.eq, f.rhs, transpose.constraints = TRUE, int.vec)
  #   opt_num_panels <- results$solution[c(TRUE,FALSE)]
  #   opt_elec_purchase <- results$solution[c(FALSE,TRUE)]
  #   
  #   e_demand <- round(D, 0)
  #   e_rate <- round(E, 3)
  #   yearly_system_cost_col <- round(P*opt_num_panels,0)
  #   energy_purchased_col <- round(opt_elec_purchase,0)
  #   yearly_solar_output_col <- round(G * opt_num_panels,0)
  #   yearly_levelized_cost_col <- round(P/G,3) #P and G in same units #round((opt_num_panels * panel_cost_per_year) / yearly_solar_output_col,3) # Since different panels will have different outputs and 'yearly costs' bc of different installation points
  #   grid_electricity_cost_col <- round(opt_elec_purchase*e_rate,0)
  #   yearly_solar_savings_col <- ifelse(opt_num_panels == 0, 0, round((e_rate * e_demand) - (yearly_levelized_cost_col*e_demand),0))  # Solar savings will depend on the yearly mix of panels which have different outputs and costs and different yearly levelized costs
  #   
  #   # Adjustment
  #   if(results$solution[c(TRUE,FALSE)][1] == 0){
  #     for (i in 1:length(opt_num_panels)) {
  #       opt_num_panels[i] <- 0
  #       yearly_system_cost_col <- round(P*opt_num_panels,0)
  #       energy_purchased_col <- round(D,0)
  #       yearly_solar_output_col[i] <- 0
  #       opt_elec_purchase <- round(D,0)
  #       yearly_levelized_cost_col[i] <- 0
  #       grid_electricity_cost_col <- round(opt_elec_purchase*e_rate,0)
  #       yearly_solar_savings_col[i] <- 0
  #     }
  #     shinyalert("Sorry but it is not cost efficient to consider solar with this setup. Please come back another year or try again with a different system.")
  #   }
  #   
  #   yearly_system_spend <- round((opt_num_panels * (capital_cost + installation_cost)) / system_lifetime,0)
  #   
  #   #Create the dataframe
  #   financial_data <- data.frame(years, e_demand, opt_num_panels, yearly_system_spend, yearly_solar_output_col, yearly_levelized_cost_col, e_rate, energy_purchased_col, grid_electricity_cost_col, yearly_solar_savings_col) #round(yearly_system_cost,0)
  #   colnames(financial_data) <- c('Year', 'Energy Demand (kWh)', 'Optimal Number of Panels', 'Yearly System Cost', 'Yearly Solar Output (kWh)', 'Annual Levelized Cost ($/kWh)', 'Electricity Cost ($/kWh)', 'Energy Purchased (kWh)', 'Grid Electricity Cost ($)', 'Annual Solar Savings ($)')
  #   
  #   output$financial_data <- DT::renderDataTable(financial_data,
  #                                                options = list(autoWidth = TRUE,
  #                                                               pageLength = 10))
  #   
  #   shinyalert("Model Complete.", showConfirmButton = TRUE, type = "success", immediate = TRUE)
  #   
  #   
  # }) # End of financial input button calc
  
  
  
}