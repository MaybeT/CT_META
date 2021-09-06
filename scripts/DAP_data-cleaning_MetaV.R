##Converting Chris Nunn data cleaning to a metadata additive file. 

# Collect Data

# Chris Nunn 

# Created:
# 2019-04-05
# Edited:
# 2019-08-29

# This is a script to load the raw .csv files for canopy temperatures and weather

# Packages:
library(lubridate) # for control of dates
library(tidyverse)

CT_data_location <- "G:/WaterWise/WaterWise/WaterWiseAnalyses/CanopyTempData_DAP/canopy_temperature"
RDS_data_files_location <- "G:/WaterWise/WaterWise/WaterWiseAnalyses/CanopyTempData_DAP/RDS_data_files"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CANOPY TEMPERATURES ---------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read the file names of all canopy data files in data folder
canopy_file_names <- list.files(path = CT_data_location,
                         recursive = T,
                         full.names = T,
                         pattern = "*.csv")

# Load the data into one nested dataframe
df <- data_frame(filename = canopy_file_names) %>%
  mutate(file_contents = map(filename, ~ read_csv(file.path(.),
                                                  skip= 0,
                                                  n_max = 50000,
                                                  col_names = TRUE,
                                                  col_types = "cn"))) 
# Unnest data
CT_data_raw <- unnest(df)

# Save to RDS for use in repeat analysis or shiny app. This is raw data
saveRDS(CT_data_raw, file.path(RDS_data_files_location,"canopy-temperatures-raw.rds"))

# Cleaning process:
#  all CT data single column name "canopy_temp"
#  all date times single column name "date_time"
#  Sensor UID from filename
#  Remove all rows with NAs
CT_data_clean <- CT_data_raw %>% 
  mutate(canopy_temp = coalesce(Canopytemp, `IRT 2`, `IRT 3`, `IRT 4`,`IRT 1`, CanopyTemp, AV_TempObj, Value),
         date_time = coalesce(Datetime, `DATE TIME`, DATETIME, TimeDate, DateTime, Date, X1, Timestamp)) %>% 
  mutate(UID = basename(filename)) %>% 
  mutate(UID = str_remove(UID,".csv"))%>% 
  select(UID, date_time, canopy_temp) %>% 
  filter(!is.na(date_time)) %>% 
  filter(!is.na(canopy_temp))

# Some projects have manually edited date_time values in different (AUS and USA) date formats
# This will find the most likely date format but for files with mixed formats a manual check must be made
# time_gap checks for the period between time points, over large gaps indicate an error
CT_data_clean_dates <- CT_data_clean %>%
  filter(!is.na(date_time)) %>% 
  group_by(UID) %>% 
  mutate(row = row_number(),
         date_time_2 = parse_date_time(date_time, orders = c("dmY HM", "Ymd HMS")),
         date_time_3 = parse_date_time(date_time, orders = c("mdY HM", "Ymd HMS")),
         date_time_4 = coalesce(date_time_2, date_time_3),
         date_time_5 = if_else(str_detect(UID, "2011_ACRI_W") & row >= 2000, date_time_3, date_time_4),
         time_gap     = as.numeric(date_time_5 - lag(date_time_5)) / 900) %>% 
  select(UID, row, date_time, time_gap, date_time_5, date_time_4, date_time_2, date_time_3, canopy_temp) 

CT_sensors_cleaned <- CT_data_clean_dates %>% 
  select(UID, date_time = date_time_5, canopy_temp)

saveRDS(CT_sensors_cleaned, file.path(RDS_data_files_location,"canopy-temperatures-cleaned_V2.rds"))

# WEATHER --------------------------------

# Read the file names of all canopy data files in data folder
weather_file_names <- list.files(path = "G:/WaterWise/WaterWise/WaterWiseAnalyses/CanopyTempData_DAP/weather_data",
                                recursive = T,
                                full.names = T,
                                pattern = "*.csv")

# Load the data into one nested dataframe
df_weather <- data_frame(filename = weather_file_names) %>%
  mutate(file_contents = map(filename, ~ read.csv(file.path(.),
                                                  colClasses = "character"))) 
  
weather_data_raw <- unnest(df_weather)

?as.Date.character()

saveRDS(weather_data_raw, file.path(RDS_data_files_location, "weather-unnested_V1.rds"))

# Cleaning process:
#  all date times single column name "date_time"
#  ACRI datetimes between 2008-2009 are wrong by 2 hours
#  all other ACRI datetimes are wrong by 1 hour
#  Only keep weather data after 2006 and remove any future (error) values
#  create unified columns for AT_max, AT_min, AT_mean
#  create unified columns for RH_max, RH_min, RH_mean
#  generate mean for AT and RH from min and max
#  Convert solar radiation units (MANUAL PROCESS) and save to "solar_rad"
#  convert rainfall to non cumulative save to "rain_mm"
#  create unified columns for AT_max, AT_min, AT_mean
#  create unified columns for AT_max, AT_min, AT_meanRemove all rows with NAs

weather_data_clean <- weather_data_raw %>% 
  mutate(location = basename(filename)) %>% 
  mutate(location = str_extract(location,"^[:alpha:]+")) %>% 
  mutate(DateTime = if_else(location == "StGeorge", dmy_hm(DateTime), dmy_hms(DateTime)),
         Time.EDST = dmy_hm(`Time.EDST`),
         Time     = dmy_hm(Time),
         DATETIME = dmy_hm(DATETIME),
         Date     = dmy_hm(Date),
         date_time = coalesce(DateTime, Date, DATETIME, Time, Time.EDST),
         date_time = force_tz(date_time, tz = timezone)) %>% 
  mutate(date_time = case_when(location == "ACRI" & Year %in% c(2008, 2009) ~ date_time - (2*60*60),
                               location == "ACRI"                           ~ date_time - (60*60),
                               TRUE                                         ~ date_time)) %>%
  filter(year(date_time) > 2006 & date_time <= Sys.time()) %>% 
  mutate_at(vars(-filename, - location, -date_time), as.numeric) %>% 
  mutate(AT_max = as.numeric(coalesce(Max.Air.Temp...C., Air.Temp.Max..deg.C.)),
         AT_min = as.numeric(coalesce(Min.Air.Temp...C., Air.Temp.Min..deg.C.)),
         AT_mean = as.numeric(coalesce(Temperature..C., Air.Temp.Avg..deg.C., AmbientTemp, `AVERAGE.Air.Temperature.Deg.C`, TempC)),
         RH_max = as.numeric(coalesce(Max.Rel.Humidity...., RH.Max....)),
         RH_min = as.numeric(coalesce(Min.Rel.Humidity...., RH.Min....)),
         RH_mean = as.numeric(coalesce(Rel.Humidity...., RelativeHumidity, RH.Avg...., `AVERAGE.Relative.Humidity..`, Humidity))) %>% 
  select(location, date_time,
         `Radiation..MJ.m².`, `Solar.Rad..kWh.m.2.`, SR_W_m2, `Solar..MJ.m.2.`, `AVERAGE.Solar.Radiation.Watts.m2`,
         `Rain..mm.`, `Rain.Since.9am..mm.`,`TOTAL.Rain.Gauge.mm`,
         AT_max:RH_mean) %>% 
  mutate(AT_mean = if_else(is.na(AT_mean), (AT_max + AT_min) / 2, AT_mean),
         RH_mean = if_else(is.na(RH_mean), (RH_max + RH_min) / 2, RH_mean)) %>% 
  mutate(`Solar.Rad..kWh.m.2.` = as.numeric(`Solar.Rad..kWh.m.2.`) - as.numeric(lag(`Solar.Rad..kWh.m.2.`)),
         `Solar.Rad..kWh.m.2.` = if_else(`Solar.Rad..kWh.m.2.` < 0, 0, `Solar.Rad..kWh.m.2.`)) %>% 
  mutate(`Radiation..MJ.m².`   = as.numeric(`Radiation..MJ.m².`) * 10^6 / (60 * 60),
         `Solar.Rad..kWh.m.2.` = as.numeric(`Solar.Rad..kWh.m.2.`)* 3.6 * 10^6 / (15 * 60),
         `Solar..MJ.m.2.`      = as.numeric(`Solar..MJ.m.2.`) * 10^6 / (60 * 60),
         `AVERAGE.Solar.Radiation.Watts.m2` = as.numeric(`AVERAGE.Solar.Radiation.Watts.m2`),
         SR_W_m2 = as.numeric(SR_W_m2),
         solar_rad = coalesce(`Radiation..MJ.m².`, `Solar.Rad..kWh.m.2.`, SR_W_m2, `Solar..MJ.m.2.`, `AVERAGE.Solar.Radiation.Watts.m2`)) %>% 
  select(-`Radiation..MJ.m².`, -`Solar.Rad..kWh.m.2.`, -SR_W_m2, -`Solar..MJ.m.2.`, -`AVERAGE.Solar.Radiation.Watts.m2`) %>% 
  group_by(location) %>% 
  mutate(rain = as.numeric(`Rain.Since.9am..mm.`) - as.numeric(lag(`Rain.Since.9am..mm.`)),
         rain = if_else(rain < 0, 0, rain),
         rain_mm = coalesce(as.numeric(`Rain..mm.`), rain, `TOTAL.Rain.Gauge.mm`)) %>% 
  select(-`Rain..mm.`, -rain, -`Rain.Since.9am..mm.`, -`TOTAL.Rain.Gauge.mm`) 

saveRDS(weather_data_clean, file.path(RDS_data_files_location, "weather-cleaned_V2.rds"))

