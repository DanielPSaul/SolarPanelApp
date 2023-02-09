################################################################################
#                                   SERVER
################################################################################


# Connect Python file and packages for simulation functions
reticulate::source_python('solar_panel_functions.py')



server <- function(input, output, session) {
  
  
  
  
  # Close/exit the server session 
  useShinyjs()
  extendShinyjs(text = jscode, functions = c("closeWindow"))
  
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })
  
  # The definitions included in the more information tab
  output$more_info_table <- DT::renderDataTable(more_info,
                                                options = list(autoWidth = TRUE,
                                                               pageLength = 3),
                                                rownames = FALSE)
  
  # # Render leaflet weather map in the Location tab
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
      lat1 <- codes[[2]]
      lon1 <- codes[[3]]
      
      lat = lat1
      lon = lon1
      
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
    
    # Retrieve weather data from NSRDB (python function)
    data_outputs <- getWeatherData(isolate(lat), isolate(lon))
    data <- data_outputs[[1]]
    
    weather_data <- data %>%
      select(., 6:14) %>%
      rownames_to_column(., var = "Timestamp")
    
    output$weather_table <- DT::renderDataTable(weather_data,
                                                options = list(autoWidth = TRUE,
                                                               pageLength = 5),
                                                rownames = FALSE)
    
    # Weather data irradiance plot
    output$irradiance_plot <- renderPlotly({
      
      new_df <- data %>%
        group_by(Month = month(Month)) %>%
        summarise(DHI = mean(DHI), DNI = mean(DNI), GHI = mean(GHI))
      
      new_df$Months <-c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
      
      p3 <- ggplot(data = new_df, aes(x = factor(Months, level=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')))) +
        geom_line(aes(y = DHI, group=1, colour = "DHI")) +
        geom_point(aes(y = DHI, colour = "DHI", text = paste("Month:", Months, 
                                                             "\nIrradiance Type: DHI",
                                                             "\nValue:", DHI))) +
        
        geom_line(aes(y = DNI, group=1, colour = "DNI")) +
        geom_point(aes(y = DNI, colour = "DNI", text = paste("Month:", Months, 
                                                             "\nIrradiance Type: DNI",
                                                             "\nValue:", DNI))) +
        
        geom_line(aes(y = GHI, group=1, colour = "GHI")) +
        geom_point(aes(y = GHI, colour = "GHI", text = paste("Month:", Months, 
                                                             "\nIrradiance Type: GHI",
                                                             "\nValue:", GHI))) +
        
        labs(title = "Monthly Average Irradiance", x = "", y = "Watts per Square Meter", color='Irradiance Type') +
        theme(axis.text.x=element_text(angle=60, hjust=1))
      
      p3 <- ggplotly(p3, tooltip = "text")
      
    })
    
    shinyalert("Gathering Complete.", showConfirmButton = TRUE, type = "success", immediate = TRUE)
    
  })
  
  # Simulation tab running when button is clicked
  observeEvent(input$SimulateButton, {
    shinyalert("Running Simulation...", showConfirmButton = FALSE, timer = 0)
    
    # Retrieve weather data again
    data_outputs <- getWeatherData(isolate(lat), isolate(lon))
    df <- data_outputs[[1]]
    info <- data_outputs[[2]]
    
    # Establish inputs
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
    
    # Various small outputs and calculations
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
    
    output$mod_eff <- renderText({
      paste(module_efficiency)
    })
    
    system_capacity <- panelcov_area * 1 * module_efficiency
    
    output$system_capacity_out <- renderText({
      paste(system_capacity, "kW")
    })
    
    # Run the energy simulation (python function)
    outputs_data <- runSimulation(df, info, tilt, azimuth, inv_eff, losses, array_type, gcr, module_type, dc_ac_ratio, system_capacity, system_use_lifetime_output, analysis_period, dc_degradation)
    
    hourly_output <- outputs_data[[1]]
    monthly_output <- outputs_data[[2]]
    annual_output <- outputs_data[[3]]
    
    output$annual_outputs <- renderTable(annual_output)
    output$hourly_outputs <- DT::renderDataTable(hourly_output)
    output$monthly_outputs <- renderTable(monthly_output)
    
    # Monthly output line plot
    output$monthly_plot <- renderPlotly({
      
      p <- ggplot(data = monthly_output, aes(x = factor(Month, level=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')))) +
        geom_line(aes(y = `DC Output (kWh)`, group=1, colour = "DC Output")) +
        geom_point(aes(y = `DC Output (kWh)`, colour = "DC Output", text = paste("Month:", Month, 
                                                                                 "\nDC Output:", `DC Output (kWh)`))) +
        
        geom_line(aes(y = `Energy Gen (kWh)`, group=1, colour = "Energy Gen")) +
        geom_point(aes(y = `Energy Gen (kWh)`, colour = "Energy Gen", text = paste("Month:", Month, 
                                                                                 "\nEnergy Gen:", `Energy Gen (kWh)`))) +
        
        labs(title = "Monthly Average Energy Output", x = "", y = "Kilowatt-hours", color='Energy Type') +
        theme(axis.text.x=element_text(angle=60, hjust=1))
      
      p <- ggplotly(p, tooltip = 'text')
      
    })
    
    # Hourly/Daily output line plot
    output$daily_plot <- renderPlotly({
      
      hourly_output$Time=row.names(hourly_output)
      
      hour_plot_df <- hourly_output %>%
        group_by(Day = yday(Time)) %>%
        summarise(`System Power Gen (kW)` = mean(`System Power Gen (kW)`))
      
      p2 <- ggplot(data = hour_plot_df, aes(x = Day, y = `System Power Gen (kW)`)) +
        geom_line(aes(group=1)) +
        geom_point() +
        labs(title = "Daily Average Energy Output", x = "Day of Year", y = "Kilowatts")
      
      p2 <- ggplotly(p2)
      
    })
    
    shinyalert("Simulation Complete.", showConfirmButton = TRUE, type = "success", immediate = TRUE)
  })
}