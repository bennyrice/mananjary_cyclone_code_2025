library(tidyverse)
library(geodata)
library(ggnewscale)
library(patchwork)
library(paletteer)
library(raster)
library(ncdf4)

#########################################################################################################################
# Preparing data
#########################################################################################################################


# Data TEMP: Temperature data from ERA5

#TEMPERATURE (mean daily temperature)

#Data accessed 20250127 from:
#ERA5-Land post-processed daily statistics from 1950 to present
#2019-2024 2m temperature
#   2m temperature (K)
#   "Temperature of air at 2m above the surface of land, sea or in-land waters. 
#   2m temperature is calculated by interpolating between the lowest model level and the Earth's surface, 
#   taking account of the atmospheric conditions. Temperature measured in kelvin can be converted to degrees 
#   Celsius (°C) by subtracting 273.15"
# Daily mean
# Time zone: UTC+3
# 1-hourly
# Region following coords:
# Coordinates of MNJ District
# North: -20.8
# West: 48.1
# East: 48.5
# South: -21.7

# LINK: https://cds.climate.copernicus.eu/datasets/derived-era5-land-daily-statistics?tab=download

#Data format:
#x, y : lon and lat for grids within the specified region for which data was downloaded from ERA5
#Columns: X0 --> X30 (day 1 to day 31 of month (e.g., Jan 1-Jan 31))
#Rows with NaN - grids in ocean and no data (only looking at land temp)
#1 row per grid (1 daily average temp)


temperature.nc_file.paths <- c("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2019_01.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2019_02.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2019_03.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2019_04.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2019_05.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2019_06.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2019_07.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2019_08.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2019_09.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2019_10.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2019_11.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2019_12.nc",
                               
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2020_01.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2020_02.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2020_03.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2020_04.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2020_05.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2020_06.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2020_07.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2020_08.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2020_09.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2020_10.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2020_11.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2020_12.nc",
                               
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2021_01.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2021_02.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2021_03.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2021_04.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2021_05.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2021_06.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2021_07.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2021_08.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2021_09.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2021_10.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2021_11.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2021_12.nc",
                               
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2022_01.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2022_02.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2022_03.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2022_04.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2022_05.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2022_06.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2022_07.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2022_08.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2022_09.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2022_10.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2022_11.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2022_12.nc",
                               
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2023_01.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2023_02.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2023_03.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2023_04.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2023_05.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2023_06.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2023_07.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2023_08.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2023_09.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2023_10.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2023_11.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2023_12.nc",
                               
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2024_01.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2024_02.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2024_03.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2024_04.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2024_05.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2024_06.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2024_07.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2024_08.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2024_09.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2024_10.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2024_11.nc",
                               "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/temperature/2024_12.nc")

#storing year and month of data in a vector for extracting data below
v.years.temp <- c(rep(2019, 12), rep(2020, 12), rep(2021, 12), rep(2022, 12), rep(2023, 12), rep(2024, 12))
v.months.temp <- c(rep(1:12, 6))


f.temperature_bricks <- function(file.paths, v.years, v.months){
  
    list1 <- list(NA)
  
  for(i in 1:length(file.paths)){
    
    month.i <- v.months[i]
    year.i <- v.years[i]
    
    dfi <- raster::as.data.frame(brick(file.paths[i]), xy = TRUE) %>% 
      pivot_longer(!c(x, y), names_to = "Xday", values_to = "temp.K") %>% 
      filter(!is.na(temp.K)) %>%
      mutate(month = month.i) %>%
      mutate(year = year.i, day = as.numeric(substr(Xday, 2, 3)) + 1) %>% mutate(date = ymd(paste0(year, "-", month, "-", day))) %>%
      mutate(temp = temp.K - 273.15)
    list1[[i]] <- dfi
  }
  
  df.out <- bind_rows(list1)
  
  return(df.out)
}

df.ERA5.T <- f.temperature_bricks(file.paths = temperature.nc_file.paths, v.years = v.years.temp, v.months = v.months.temp)

df.mean.T <- df.ERA5.T %>% group_by(date) %>% summarize(mean.temp.C = mean(temp))

###Exporting ERA5 temp data as a CSV for convenience
# write_csv(df.mean.T, '/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/processed/mean_daily_temp_ERA5.csv')


#########################################################################################################################
# Data Precip: Precipitation data from ERA5

#Data accessed 20250127 from:
#ERA5-Land hourly data from 1950 to present
#2019-2024 Total Precipitation
#   2m temperature (K)
#   "This parameter is the accumulated liquid and frozen water, comprising rain and snow, 
#    that falls to the Earth's surface. It is the sum of large-scale precipitation and 
#    convective precipitation."
# Hourly total
# Region following coords:
# Coordinates of MNJ District
# North: -20.8
# West: 48.1
# East: 48.5
# South: -21.7

# LINK: https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land?tab=download

#PRECIP
precip.nc_file.paths <- c("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2019_01.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2019_02.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2019_03.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2019_04.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2019_05.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2019_06.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2019_07.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2019_08.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2019_09.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2019_10.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2019_11.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2019_12.grib",
                          
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2020_01.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2020_02.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2020_03.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2020_04.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2020_05.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2020_06.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2020_07.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2020_08.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2020_09.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2020_10.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2020_11.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2020_12.grib",
                          
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2021_01.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2021_02.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2021_03.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2021_04.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2021_05.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2021_06.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2021_07.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2021_08.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2021_09.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2021_10.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2021_11.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2021_12.grib",
                          
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2022_01.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2022_02.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2022_03.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2022_04.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2022_05.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2022_06.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2022_07.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2022_08.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2022_09.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2022_10.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2022_11.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2022_12.grib",
                          
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2023_01.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2023_02.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2023_03.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2023_04.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2023_05.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2023_06.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2023_07.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2023_08.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2023_09.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2023_10.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2023_11.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2023_12.grib",
                          
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2024_01.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2024_02.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2024_03.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2024_04.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2024_05.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2024_06.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2024_07.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2024_08.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2024_09.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2024_10.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2024_11.grib",
                          "/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/precip/2024_12.grib")

#storing year of data in a vector for extracting data below
v.years.precip  <- c(rep(2019, 12), rep(2020, 12), rep(2021, 12), rep(2022, 12), rep(2023, 12), rep(2024, 12))
v.months.precip <- c(rep(1:12, 6))

f.precip_bricks <- function(file.paths, v.years, v.months){
  
  list1 <- list(NA)
  
  for(i in 1:length(file.paths)){
    
    month.i <- v.months[i]
    year.i <- v.years[i]

    dfi <- raster::as.data.frame(brick(file.paths[i]), xy = TRUE) %>% 
      pivot_longer(!c(x, y), names_to = "Xday.time", values_to = "precip.M") %>% 
      #Substring to select the count of day and time (day 1 hour 1...hour 24, day 2 hour 1...24, etc)
      mutate(Xday.time = as.numeric(substr(Xday.time, 56, 59))) %>%
      mutate(month = month.i) %>% mutate(year = year.i) %>%
      #rep 24 hours by number of days in the month (using lubridate::days_in_month to get correct count)
      #Have to account for February 2024 ERA5 data which erroneously does not have February 29
      group_by(x, y) %>% 
      mutate(hour = 
               ifelse(ymd(paste0(year.i, "-", month.i, "-", 1)) != "2024-02-01",
                      rep(1:24, as.numeric(days_in_month(ymd(paste0(year.i, "-", month.i, "-", 1))))),
                      rep(1:24, 28)
                      )
             ) %>% 
      ungroup() %>% 
      mutate(day.decimal = (Xday.time - 1)/24 + 1) %>% mutate(day = floor(day.decimal)) %>% 
      mutate(date = ymd(paste0(year, "-", month, "-", day))) %>%
      filter(!is.na(precip.M)) %>%
      group_by(date, x, y) %>% summarize(precip.mm = sum(precip.M)*1000)
      
    list1[[i]] <- dfi
  }
  
  df.out <- bind_rows(list1)
  
  return(df.out)
}

#Calling f.precip_bricks to extract data
df.ERA5.precip <- f.precip_bricks(file.paths = precip.nc_file.paths, v.years = v.years.precip, v.months = v.months.precip)

df.mean.precip <- df.ERA5.precip %>% group_by(date) %>% summarize(mean.precip.mm = mean(precip.mm))


###Exporting ERA5 temp data as a CSV for convenience
# write_csv(df.mean.precip, '/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_climate_ERA5/processed/mean_daily_precip_ERA5.csv')



#########################################################################################################################
# Data 2:Cyclone landfall approach dates
# using file from output of a function call in the cyclone mapping (Figure 1) script

df.cycs <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_ibtracs_cyclones/processed/endemic_country_cyclone_hit_points.csv") %>%
  filter(COUNTRY == "Madagascar") %>%
  mutate(cyc.month = month(date), cyc.week = week(date), cyc.day = day(date))


#counts of storms by date
df.cycs.date_counts <- df.cycs %>%
  group_by(cyc.month, cyc.day) %>% summarize(cyc.n = n()) %>%
  mutate(date = ymd(paste0(2024, "-", cyc.month, "-", cyc.day)))

#Creating a data frame with all dates in a calendar year as rows (using 2024 as an example for convenience due to leap years)
dfm.day <- tibble(date = seq(ymd('2024-01-01'),ymd('2024-12-31'), by = 'days'),
              month = month(date),
              day = day(date)) %>%
  left_join(df.cycs.date_counts, by = join_by(date)) %>%
  #Setting dates with zero storms to 0
  mutate(cyc.n = ifelse(is.na(cyc.n), 0, cyc.n)) %>%
  mutate(cyc.n.fac = factor(cyc.n, levels = c("0", "1", "2", "3", "4")))

#Test plotting
dfm.day %>% 
  ggplot(aes(x = date, y = 10, fill = cyc.n.fac)) +
  geom_tile(color = "white", linewidth = 0.05) +
  geom_point(aes(x = date, y = 10.6), color = "black", size = 0.1) +
  scale_fill_viridis_d(name = "Cyclone Count", 
                       option = "mako") +
  ylim(0, 11.5) +
  coord_polar() +
  theme_void()

#########################################################################################################################
# Joining cyclone and climate data

#joining ERA5 data and aligning to 2024 dates for plotting
df.ERA5.clim <- df.mean.precip %>% full_join(df.mean.T, by = join_by(date)) %>% 
  mutate(month = month(date), day = day(date), date.2024 = ymd(paste0("2024", "-", month, "-", day))) %>%
  dplyr::select(-date) %>%
  #Averaging across years
  group_by(date.2024) %>% summarize(avg.precip = mean(mean.precip.mm, na.rm = TRUE), avg.temp = mean(mean.temp.C, na.rm = TRUE))

dfm.i <- dfm.day %>% full_join(df.ERA5.clim, by = join_by(date == date.2024))

#########################################################################################################################
# Adding malaria data (using site S6 age 14 year olds as an example)
df.foi <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_foi/foi_bookends 20231023.csv") %>%
  dplyr::select(date, S6.14) %>% rename(prob.inf = S6.14) %>%
  #note 2024 was a leap year so will need to account for Feb 29
  mutate(date.2024 = ymd(paste0(2024, "-", month(date), "-", day(date)))) %>% dplyr::select(-date)

dfm <- dfm.i %>% left_join(df.foi, by = join_by(date == date.2024)) %>%
  #interpolating February 29th simply for visualization: using midpoint between day before and day after
  mutate(prob.inf   = ifelse(date == "2024-02-29", (0.2612645 + 0.2633793)/2, prob.inf)) %>%
  mutate(avg.precip = ifelse(date == "2024-02-29", (58.476622 + 43.096434)/2, avg.precip)) %>%
  
  #adding a month label
  mutate(month.label = month(date, label = TRUE, abbr = TRUE))

#Making a vector of the date halfway through a month to facilitate labeling
v.mid.months <- ymd(c("2024-01-15",
                      "2024-02-14",
                      "2024-03-15",
                      "2024-04-15",
                      "2024-05-15",
                      "2024-06-15",
                      "2024-07-15",
                      "2024-08-15",
                      "2024-09-15",
                      "2024-10-15",
                      "2024-11-15",
                      "2024-12-15"))

#Making a vector of the first date of months to facilitate labeling
v.months.bounds <- ymd(c("2024-01-01",
                         "2024-02-01",
                         "2024-03-01",
                         "2024-04-01",
                         "2024-05-01",
                         "2024-06-01",
                         "2024-07-01",
                         "2024-08-01",
                         "2024-09-01",
                         "2024-10-01",
                         "2024-11-01",
                         "2024-12-01"))


#########################################################################################################################
# Figure 3B: Cyclones-Precip-Temp-Malaria (for an exemplar site)
#########################################################################################################################
#Circular
p.c <- dfm %>% 
  ggplot() +
  
  #Ring 1: Malaria
  geom_tile(aes(x = date, y = 10, fill = prob.inf), height = 2, width = 1.1) +
  #Malaria color scale
  scale_fill_paletteer_c("grDevices::Reds 2", name = "Cumulative probability\nof malaria infection\n(per 31 days)", direction = -1, limits = c(0, 0.47), guide = guide_colourbar(order = 1)) +

  #Ring 2: temp
  new_scale_fill() +
  geom_tile(aes(x = date, y = 8.75, fill = avg.temp), height = 0.5, width = 1.1) +
  scale_fill_paletteer_c("grDevices::Lajolla", name = "Mean Temp (C)", direction = 1, guide = guide_colourbar(order = 2)) +
  
  #Ring 3: precip
  new_scale_fill() +
  geom_tile(aes(x = date, y = 8.25, fill = avg.precip), height = 0.5, width = 1.1) +
  scale_fill_paletteer_c("grDevices::Blues 3", name = "Mean Precip (mm)", direction = -1, guide = guide_colourbar(order = 3),
                         trans = scales::log_trans(), breaks = c(5, 50, 500), limits = c(1, 500)) +

  #Ring 4: cyclone day counts
  new_scale_fill() +
  geom_tile(aes(x = date, y = 7.5, fill = cyc.n.fac), color = "white", height = 1, width = 1.1) +
  #scale_fill_manual(name = "Historical Cyclone\nCount", values = c("white", "#F5B6C8", "#DE89B0", "#C868A1", "#9C4286"),
  scale_fill_manual(name = "Historical Cyclone\nCount", values = c("white", "grey80", "grey50", "grey20", "grey5"), guide = guide_legend(order = 4)) +
  
  #Outer labels:
  new_scale_fill() +
  #Ticks for each day along 'x' axis
  geom_segment(aes(x = date, xend = date, y = 11.05, yend = 11.15), color = "black", linewidth = 0.25, alpha = 1, linetype = "solid") +
  #Month boundaries
  geom_segment(data = dfm %>% filter(date %in% v.months.bounds),
               aes(x = date, xend = date, y = 11.25, yend = 12), color = "black", linewidth = 0.35, alpha = 0.7, linetype = "solid") +
  #Month labels
  geom_text(data = dfm %>% filter(date %in% v.mid.months),
            aes(x = date, y = 11.75, label = month.label), color = "black", alpha = 0.8, size = 3.5) +

  #Dots for Cyclones Freddy and Batsirai
  geom_point(data = dfm %>% filter(date %in% c(ymd("2024-02-05"), ymd("2024-2-21"))),
             aes(x = date, y = 6.75), color = "black") +
  
  #Theme and coord params
  scale_y_continuous(n.breaks = 24, limits = c(0, 12)) +
  coord_polar() +
  theme_void() +
  theme(legend.key = element_rect(color = "black"),
        legend.position = "left"
        #legend.key.size = unit(1.1, 'cm'),
        #legend.title = element_text(size = 14)
        )
p.c



#########################
#Linear Plot
#########################

#Row 1: cyclone day counts
p.l.1 <- dfm %>% 
  ggplot() +
  geom_tile(aes(x = date, y = 1, fill = cyc.n.fac), color = "white", height = 1) +
  scale_fill_manual(name = "Historical Cyclone\nCount", values = c("white", "grey85", "grey55", "grey25", "grey5"),
                    guide = guide_legend(order = 1)) +
  ylab(NULL) + xlab(NULL) +
  scale_x_date(date_labels = "%b", date_breaks = "month") +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.key = element_rect(color = "black"))
p.l.1

#Row 2: Precip
p.l.2 <- dfm %>%
  ggplot() +
  geom_line(aes(x = date, y = avg.precip), color = "#439FB7") +
  ylab("Mean Precipitation (mm)") + xlab(NULL) +
  scale_x_date(date_labels = "%b", date_breaks = "month") +
  theme_bw() +
  theme(panel.grid = element_blank())
p.l.2

#Row 3: Temp
p.l.3 <- dfm %>%
  ggplot() +
  geom_line(aes(x = date, y = avg.temp), color = "#F49639") +
  ylab("Mean Temperature (C)") + xlab(NULL) +
  scale_x_date(date_labels = "%b", date_breaks = "month") +
  theme_bw() +
  theme(panel.grid = element_blank())
p.l.3

#Row 4: Malaria
p.l.4 <- dfm %>%
  ggplot() +
  geom_tile(aes(x = date, y = 1, fill = prob.inf), height = 1) +
  scale_fill_paletteer_c("grDevices::Reds", direction = -1, name = "Cumulative probability\nof malaria infection\n(per 31 days)", limits = c(0, 0.47)) +
  scale_x_date(date_labels = "%b", date_breaks = "month") +
  xlab("Month") + ylab(NULL) +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid = element_blank())
p.l.4

p.l.1 / p.l.2 / p.l.3 / p.l.4


##################################################################################################################################
## Linear for course of study ####################################################################################################
##################################################################################################################################

# Pieces # 1 # Cyclones

#Cyclones in Mada since 1980 with date
# df.cycs
#Cyclones per day using 2024 as year
# dfm.day

#Extracting Cyclone Freddy and Cyclone Batsirai
df.cycs.of_note <- df.cycs %>% 
  filter(date >= as.Date("2021-07-01")) %>%
  filter(date <= as.Date("2023-04-30")) %>%
  filter(NAME %in% c("ANA", "BATSIRAI", "EMNATI", "FREDDY"))

#Making a 2021-2023 time series of cyclone freq
df.cyc.day.ts <- tibble(date = seq(from = as.Date("2021-07-01"), to = as.Date("2023-04-30"), by = "day"),
                     month = month(date),
                     day = day(date)) %>%
  left_join(dfm.day, by = join_by(month, day)) %>%
  dplyr::select(date.x, month, day, cyc.n) %>% rename(date = date.x) %>%
  mutate(cyc.n.fac = factor(cyc.n))

# Pieces # 2 # Precip

#Mean daily precip
# df.mean.precip

#Making a 2021-2023 time series of daily precip
df.mean.precip.ts <- df.mean.precip %>% 
  filter(date >= as.Date("2021-07-01")) %>%
  filter(date <= as.Date("2023-04-30"))
#Extracting precip associated with the cyclones of note
df.cycs.of_note.precip <- df.cycs.of_note %>%
  left_join(df.mean.precip.ts, by = join_by(date))


# Pieces # 3 # Temp

#Mean daily precip
# df.mean.T

#Making a 2021-2023 time series of daily precip
df.mean.T.ts <- df.mean.T %>% 
  filter(date >= as.Date("2021-07-01")) %>%
  filter(date <= as.Date("2023-04-30"))


# Pieces # 4 # Malaria

#Making a 2021-2023 time series of malaria
df.mal.ts <- tibble(date = seq(from = as.Date("2021-07-01"), to = as.Date("2023-04-30"), by = "day"),
                    month = month(date),
                    day = day(date)) %>%
  left_join(dfm, by = join_by(month, day)) %>%
  dplyr::select(date.x, month, day, prob.inf) %>% rename(date = date.x)


p3B.cyc.tile <- df.cyc.day.ts %>% 
  ggplot() +
  geom_tile(aes(x = date, y = 1, fill = cyc.n.fac), height = 1) +
  scale_fill_viridis_d(option = "mako", 
                       #direction = -1,
                       name = "Historical Cyclone\nCount") +
  xlab(NULL) +
  ylab("Cyclone\nSeasonality") + 
  scale_x_date(date_breaks = "1 month", limits = as.Date(c('2021-06-25','2023-05-05')), expand =c(0,0)) +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none",
        #axis.text.x = element_text(angle = 90),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        legend.key = element_rect(color = "black"))
p3B.cyc.tile

p3B.precip <- df.mean.precip.ts %>% 
  ggplot() +
  #annotate("segment", x = df.cycs.of_note$date, xend = df.cycs.of_note$date, y = 0, yend = Inf, linetype = "dashed", alpha = 0.5, color = "grey20") +
  geom_line(aes(x = date, y = mean.precip.mm), color = "#5586B3", linewidth = 1) +
  #adding points for storms
  #geom_point(data = df.cycs.of_note.precip, aes(x = date, y = mean.precip.mm + 100)) +
  geom_text_repel(data = df.cycs.of_note.precip, aes(x = date, y = mean.precip.mm + 50, label = NAME),
                  color = "grey40",
                  segment.color	= "grey40",
                  #nudge_x = 50,
                  nudge_y = 500,
                  #point.padding = 100,
                  angle = 90,
                  direction = "y",
                  min.segment.length = 0,
                  segment.curvature = -1e-20) +
  
  ylab("Precipitation\n(mm)") + xlab(NULL) +
  scale_x_date(date_breaks = "1 month", limits = as.Date(c('2021-06-25','2023-05-05')), expand =c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_text(angle = 90),
        axis.text.x = element_blank())
p3B.precip

p3B.temp <- df.mean.T.ts %>% 
  ggplot() +
  geom_vline(xintercept = df.cycs.of_note$date, linetype = "dashed", alpha = 0.5, color = "grey20") +
  geom_line(aes(x = date, y = mean.temp.C), color = "#F49639", linewidth = 1) +
  ylab("Mean\nTemperature (C)") + xlab(NULL) +
  scale_x_date(date_breaks = "1 month", limits = as.Date(c('2021-06-25','2023-05-05')), expand =c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank())
p3B.temp

p3B.mal <- df.mal.ts %>%
  ggplot() +
  geom_tile(aes(x = date, y = 1, fill = prob.inf), height = 1) +
  geom_vline(xintercept = df.cycs.of_note$date, linetype = "dashed", alpha = 0.5, color = "grey20") +
  scale_fill_paletteer_c("grDevices::Reds", direction = -1, name = "Cumulative probability\nof malaria infection\n(per 31 days)", limits = c(0, 0.47)) +
  scale_x_date(date_breaks = "1 month", limits = as.Date(c('2021-06-25','2023-05-05')), expand =c(0,0)) +
  xlab(NULL) + 
  ylab("Malaria\nSeasonality") +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12),
        panel.grid = element_blank())
p3B.mal

p3B.cyc.tile / p3B.precip / p3B.temp / p3B.mal + plot_layout(heights = c(1, 4, 4, 1))



#########################################################################################################################
# Figure S10: Seasonality for all sites
#########################################################################################################################

df.foi.all <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data/data_foi/foi_bookends 20231023.csv") %>%
  pivot_longer(!date, names_to = "foi.site", values_to = "prob.inf") %>%
  mutate(site_code = substr(foi.site, 1, 2)) %>% 
  mutate(age = as.integer(substr(foi.site, 4, 5))) %>%
  #updating site codes
  mutate(code.new = case_when(
    site_code == "N5" ~ "MNJ.01",
    site_code == "N4" ~ "MNJ.02",
    site_code == "N3" ~ "MNJ.03",
    site_code == "N2" ~ "MNJ.04",
    site_code == "N1" ~ "MNJ.05",
    site_code == "S1" ~ "MNJ.06",
    site_code == "S2" ~ "MNJ.07",
    site_code == "S3" ~ "MNJ.08",
    site_code == "S5" ~ "MNJ.09",
    site_code == "S6" ~ "MNJ.10")) %>%
  #Ordering by average infection rate
  mutate(code.new = factor(code.new, levels = c("MNJ.06",
                                                "MNJ.04",
                                                "MNJ.03",
                                                "MNJ.05",
                                                "MNJ.02",
                                                "MNJ.07",
                                                "MNJ.08",
                                                "MNJ.09",
                                                "MNJ.01",
                                                "MNJ.10"))) %>%
  mutate(month = month(date)) %>%
  mutate(month.label = month(date, label = TRUE, abbr = TRUE))

#Making a vector of the date halfway through a month to facilitate labeling
v.mid.months.2023 <- ymd(c("2023-01-15",
                           "2023-02-14",
                           "2023-03-15",
                           "2023-04-15",
                           "2023-05-15",
                           "2023-06-15",
                           "2023-07-15",
                           "2023-08-15",
                           "2023-09-15",
                           "2023-10-15",
                           "2023-11-15",
                           "2023-12-15"))

#Making a vector of the date halfway through a month to facilitate labeling
v.months.bounds.2023 <- ymd(c("2023-01-01",
                              "2023-02-01",
                              "2023-03-01",
                              "2023-04-01",
                              "2023-05-01",
                              "2023-06-01",
                              "2023-07-01",
                              "2023-08-01",
                              "2023-09-01",
                              "2023-10-01",
                              "2023-11-01",
                              "2023-12-01"))
#Donut 1: Young children
p.d.05 <- df.foi.all %>% filter(age == 5) %>%
  ggplot(aes(x = date, y = 20, fill = prob.inf)) +
  #Site 1
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.06") %>% filter(age == 5),  width = 1.2, height = 1) +
  #Sites 2-10
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.04") %>% filter(age == 5), aes(x = date, y = 19), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.03") %>% filter(age == 5), aes(x = date, y = 18), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.05") %>% filter(age == 5), aes(x = date, y = 17), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.02") %>% filter(age == 5), aes(x = date, y = 16), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.07") %>% filter(age == 5), aes(x = date, y = 15), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.08") %>% filter(age == 5), aes(x = date, y = 14), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.09") %>% filter(age == 5), aes(x = date, y = 13), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.01") %>% filter(age == 5), aes(x = date, y = 12), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.10") %>% filter(age == 5), aes(x = date, y = 11), width = 1.2, height = 1) +
  
  #Color scale
  scale_fill_paletteer_c("grDevices::Berlin", name = "Cumulative probability\nof malaria infection\n(per 31 days)", limits = c(0, max(df.foi.all$prob.inf))) +
  
  #Inner labels:
  new_scale_fill() +
  geom_text(data = df.foi.all %>% filter(age == 5) %>% filter(date == ymd("2023-01-01")),
            aes(x = date, 
                y = 20:11, 
                label = c("MNJ.06","MNJ.04","MNJ.03","MNJ.05","MNJ.02","MNJ.07","MNJ.08","MNJ.09","MNJ.01","MNJ.10")),
            size = 2.5, color = "white") +
  
  #Outer labels:
  new_scale_fill() +
  #Ticks for each day along 'x' axis
  geom_segment(data = df.foi.all %>% filter(code.new == unique(df.foi.all$code.new)[1])  %>% filter(age == 5),
               aes(x = date, xend = date, y = 20.6, yend = 20.9),
               color = "black", linewidth = 0.25, alpha = 1, linetype = "solid") +
  #Month boundaries
  geom_segment(data = df.foi.all %>% filter(date %in% v.months.bounds.2023) %>% filter(code.new == unique(df.foi.all$code.new)[1]) %>% filter(age == 5),
               aes(x = date, xend = date, y = 21.2, yend = 22),
               color = "black", linewidth = 0.3, alpha = 0.5, linetype = "solid") +
  #Month labels
  geom_text(data = df.foi.all %>% filter(date %in% v.mid.months.2023) %>% filter(code.new == unique(df.foi.all$code.new)[1]) %>% filter(age == 5),
            aes(x = date, y = 21.8, label = month),
            color = "black") +
  #Theme and coord params
  ylim(0, 22) +
  coord_polar() +
  theme_void() +
  theme(legend.position = "bottom")
p.d.05

#Donut 2: School aged children
p.d.14 <- df.foi.all %>% filter(age == 14) %>%
  ggplot(aes(x = date, y = 20, fill = prob.inf)) +
  #Site 1
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.06") %>% filter(age == 14),  width = 1.2, height = 1) +
  #Sites 2-10
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.04") %>% filter(age == 14), aes(x = date, y = 19), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.03") %>% filter(age == 14), aes(x = date, y = 18), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.05") %>% filter(age == 14), aes(x = date, y = 17), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.02") %>% filter(age == 14), aes(x = date, y = 16), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.07") %>% filter(age == 14), aes(x = date, y = 15), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.08") %>% filter(age == 14), aes(x = date, y = 14), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.09") %>% filter(age == 14), aes(x = date, y = 13), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.01") %>% filter(age == 14), aes(x = date, y = 12), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.10") %>% filter(age == 14), aes(x = date, y = 11), width = 1.2, height = 1) +
  
  #Color scale
  scale_fill_paletteer_c("grDevices::Berlin", name = "Cumulative probability\nof malaria infection\n(per 31 days)", limits = c(0, max(df.foi.all$prob.inf))) +
  
  #Outer labels:
  new_scale_fill() +
  #Ticks for each day along 'x' axis
  geom_segment(data = df.foi.all %>% filter(code.new == unique(df.foi.all$code.new)[1])  %>% filter(age == 14),
               aes(x = date, xend = date, y = 20.6, yend = 20.9),
               color = "black", linewidth = 0.25, alpha = 1, linetype = "solid") +
  #Month boundaries
  geom_segment(data = df.foi.all %>% filter(date %in% v.months.bounds.2023) %>% filter(code.new == unique(df.foi.all$code.new)[1]) %>% filter(age == 14),
               aes(x = date, xend = date, y = 21.2, yend = 22),
               color = "black", linewidth = 0.3, alpha = 0.5, linetype = "solid") +
  #Month labels
  geom_text(data = df.foi.all %>% filter(date %in% v.mid.months.2023) %>% filter(code.new == unique(df.foi.all$code.new)[1]) %>% filter(age == 14),
            aes(x = date, y = 21.8, label = month),
            color = "black") +
  #Theme and coord params
  ylim(0, 22) +
  coord_polar() +
  theme_void() +
  theme(legend.position = "bottom")
p.d.14

#Donut 3: Adults
p.d.30 <- df.foi.all %>% filter(age == 30) %>%
  ggplot(aes(x = date, y = 20, fill = prob.inf)) +
  #Site 1
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.06") %>% filter(age == 30),  width = 1.2, height = 1) +
  #Sites 2-10
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.04") %>% filter(age == 30), aes(x = date, y = 19), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.03") %>% filter(age == 30), aes(x = date, y = 18), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.05") %>% filter(age == 30), aes(x = date, y = 17), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.02") %>% filter(age == 30), aes(x = date, y = 16), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.07") %>% filter(age == 30), aes(x = date, y = 15), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.08") %>% filter(age == 30), aes(x = date, y = 14), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.09") %>% filter(age == 30), aes(x = date, y = 13), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.01") %>% filter(age == 30), aes(x = date, y = 12), width = 1.2, height = 1) +
  geom_tile(data = df.foi.all %>% filter(code.new == "MNJ.10") %>% filter(age == 30), aes(x = date, y = 11), width = 1.2, height = 1) +
  
  #Color scale
  scale_fill_paletteer_c("grDevices::Berlin", name = "Cumulative probability\nof malaria infection\n(per 31 days)", limits = c(0, max(df.foi.all$prob.inf))) +
  
  #Outer labels:
  new_scale_fill() +
  #Ticks for each day along 'x' axis
  geom_segment(data = df.foi.all %>% filter(code.new == unique(df.foi.all$code.new)[1])  %>% filter(age == 30),
               aes(x = date, xend = date, y = 20.6, yend = 20.9),
               color = "black", linewidth = 0.25, alpha = 1, linetype = "solid") +
  #Month boundaries
  geom_segment(data = df.foi.all %>% filter(date %in% v.months.bounds.2023) %>% filter(code.new == unique(df.foi.all$code.new)[1]) %>% filter(age == 30),
               aes(x = date, xend = date, y = 21.2, yend = 22),
               color = "black", linewidth = 0.3, alpha = 0.5, linetype = "solid") +
  #Month labels
  geom_text(data = df.foi.all %>% filter(date %in% v.mid.months.2023) %>% filter(code.new == unique(df.foi.all$code.new)[1]) %>% filter(age == 30),
            aes(x = date, y = 21.8, label = month),
            color = "black") +
  #Theme and coord params
  ylim(0, 22) +
  coord_polar() +
  theme_void() +
  theme(legend.position = "bottom")
p.d.30

p.d.05 / p.d.14 / p.d.30






#########################################################################################################################
# Figure: Linear Precip and Cyclone Time Line
#########################################################################################################################

#Cyclones to hit Madagascar 2020-2023
df.cycs.2020to2023 <- df.cycs %>% filter(date > ymd("2019-01-01")) #%>% filter(date < ymd("2024-01-01")) # %>%
  #Subsetting to those that hit the SE Coast near Mananjary district
  # filter(name.yr %in% c("2020_2020022S17043_DIANE", 
  #                       "2020_2020034S13063_FRANCISCO", 
  #                       "2021_2020355S11065_CHALANE", 
  #                       "2022_2022020S13059_ANA", 
  #                       "2022_2022025S11091_BATSIRAI", 
  #                       "2022_2022047S15073_EMNATI", 
  #                       "2023_2023018S14058_CHENESO", 
  #                       "2023_2023036S12118_FREDDY"))

p.cycs.precip <- df.mean.precip %>% full_join(df.cycs.2020to2023, by = join_by(date)) %>%
  mutate(year = year(date))

df.precip.ts <- p.cycs.precip %>%
  ggplot(aes(x = date, y = mean.precip.mm)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = c(ymd("2019-01-01"), ymd("2020-01-01"), ymd("2021-01-01"), ymd("2022-01-01"), ymd("2023-01-01"), ymd("2024-01-01"), ymd("2025-01-01")), color = "grey50", linetype = "dotted", alpha = 0.8, linewidth = 0.4) +
  geom_line(color = "#49A4B9") +
  geom_point(data = df.cycs.2020to2023, aes(x = date, y = -50), color = "black", shape = 18, size = 4) +
  geom_text_repel(data = df.cycs.2020to2023, 
                   aes(x = date, y = -50, label = NAME), 
                   color = "black",
                   #force_pull   = 0, # do not pull toward data points
                   nudge_y      = -250,
                   direction    = "y",
                   angle        = 90,
                   hjust        = 1,
                   segment.size = 0.2,
                   max.iter = 1e4, max.time = 1
  ) +
  scale_x_date(date_breaks = "3 month") +
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000), limits = c(-900, 2500)) +
  #ylim(-850, 2500) +
  xlab(NULL) +
  ylab("Daily total precipitation estimates from ERA5 (mm)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
df.precip.ts






