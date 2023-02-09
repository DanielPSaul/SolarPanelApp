# Import required packages
import PySAM
from PySAM import Pvwattsv8 as PVWatts
import pandas as pd
import numpy as np

# Establish API key and email as credentials
api_key = '9UUX1nAVhZoj90XY9R1QHD4U5foWHVABoQxlbnxt'
email = 'danielsaul@uga.edu'

# Define function to retrieve weather data from location via API
def getWeatherData(lat, lon):
  url = 'https://developer.nrel.gov/api/nsrdb/v2/solar/psm3-tmy-download.csv?names=tmy-2020&wkt=POINT({lon}%20{lat})&interval=60&api_key={api_key}&email={email}&utc=false'.format(api_key=api_key, email=email, lon=lon, lat=lat)
  df = pd.read_csv(url, skiprows=2)
  info = pd.read_csv(url, nrows=1)
  df = df.set_index(pd.to_datetime(df[['Year', 'Month', 'Day', 'Hour', 'Minute']]))
  df.index.rename('Timestamp', inplace=True)
  return df, info

# Define function to run simulation based on inputs and data, producing output tables and metrics
def runSimulation(df, info, tilt, azimuth, inv_eff, losses, array_type, gcr, module_type, dc_ac_ratio, system_capacity, system_use_lifetime_output, analysis_period, dc_degradation):
  use_wf_albedo = 1 # Use weather file's albedo values
  constant = 0 # Initialize a constant variable for sim use
  dc_degradation = tuple(map(float, dc_degradation.split(', '))) # Format DC degradation value
  
  # Solar model with a new model
  solar_model = PVWatts.new("PVWattsNone")

  # SolarResource
  weather_data = np.array([
      df['Year'],
      df['Month'],
      df['Day'],
      df['Hour'],
      df['Minute'],
      df['Dew Point'],
      df['DHI'],
      df['DNI'],
      df['GHI'],
      df['Surface Albedo'],
      df['Pressure'],
      df['Temperature'],
      df['Wind Direction'],
      df['Wind Speed']
  ])
  
  solar_resource_data = {
          'tz': info['Local Time Zone'], # timezone
          'elev': info['Elevation'], # elevation
          'lat': info['Latitude'], # latitude
          'lon': info['Longitude'], # longitude
          'year': tuple(weather_data[0]), # year
          'month': tuple(weather_data[1]), # month
          'day': tuple(weather_data[2]), # day
          'hour': tuple(weather_data[3]), # hour
          'minute': tuple(weather_data[4]), # minute
          'df': tuple(weather_data[6]), # direct normal irradiance
          'dn': tuple(weather_data[7]), # diffuse irradiance
          'gh': tuple(weather_data[8]), # global horizontal irradiance
          'wspd': tuple(weather_data[13]), # windspeed
          'tdry': tuple(weather_data[11]) # dry bulb temperature
          }
  
  solar_model.SolarResource.assign({'solar_resource_data': solar_resource_data})
  solar_model.SolarResource.assign({'use_wf_albedo': use_wf_albedo})
  
  #Lifetime
  if system_use_lifetime_output == 1:
      life = {'system_use_lifetime_output':system_use_lifetime_output,
          'dc_degradation':dc_degradation, 
          'analysis_period':analysis_period}
      solar_model.Lifetime.assign(life)
  
  
  # AdjustmentFactors
  solar_model.AdjustmentFactors.assign({'constant':constant})
  
  # SystemDesign
  sysD = {'azimuth':azimuth,
          'gcr':gcr,
          'array_type':array_type,
          'dc_ac_ratio':dc_ac_ratio,
          'inv_eff':inv_eff,
          'losses':losses,
          'module_type':module_type,
          'system_capacity':system_capacity,
          'tilt':tilt}
  solar_model.SystemDesign.assign(sysD)
  
  solar_model.execute(0)
  
  df['AC Output (W)'] = solar_model.Outputs.ac
  df['AC Output (W)'] = df['AC Output (W)']
  df['System Power Gen (kW)'] = solar_model.Outputs.gen
  df['System Power Gen (kW)'] = df['System Power Gen (kW)']
  df['DC Input (W)'] = solar_model.Outputs.dc
  df['DC Input (W)'] = df['DC Input (W)']
  hourly_data = df.iloc[:, 14:17].round(2).abs()
  
  months = ['January','February','March','April','May','June','July','August','September','October','November','December']
  monthly_data = pd.DataFrame(months)
  monthly_data['AC Output (kWh)'] = solar_model.Outputs.ac_monthly
  monthly_data['DC Output (kWh)'] = solar_model.Outputs.dc_monthly
  monthly_data['Energy Gen (kWh)'] = solar_model.Outputs.monthly_energy
  monthly_data['Sol Radiation (kWh/m2/day)'] = solar_model.Outputs.solrad_monthly
  monthly_data.rename(columns = {0:'Month'}, inplace = True)
  monthly_data = monthly_data.round(2)
  
  annual_columns = ['Energy Gen (kWh)', 'Capacity Factor (%)', 'Capacity Factor AC (%)','Site Elevation (m)','Energy Yield (kWh/kW)']
  annual_data = pd.DataFrame(annual_columns)
  annual_data['Energy Gen (kWh)'] = round(solar_model.Outputs.annual_energy,2)
  annual_data['Capacity Factor (%)'] = round(solar_model.Outputs.capacity_factor,2)
  annual_data['Capacity Factor AC (%)'] = round(solar_model.Outputs.capacity_factor_ac,2)
  annual_data['Site Elevation (m)'] = solar_model.Outputs.elev
  annual_data['Energy Yield (kWh/kW)'] = round(solar_model.Outputs.kwh_per_kw,2)
  annual_data = annual_data.rename(columns = {0:'Metric'})
  annual_data = annual_data.transpose()
  annual_data = annual_data.drop(columns=[1,2,3,4])
  annual_data = annual_data.iloc[1: , :]
  annual_data = annual_data.rename(columns = {0:'Output'})
  annual_data.reset_index(inplace=True)
  annual_data = annual_data.rename(columns = {'index':'Type'})
  
  return hourly_data, monthly_data, annual_data
