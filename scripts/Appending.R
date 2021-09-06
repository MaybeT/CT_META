##Append rows to the definitive canopy temperature spreadsheet. 

library(tidyverse)
library(lubridate)
library(readxl)

#read in files; excel file name and sheet name or number - this is the "original" file
Base_dtc <- read_xlsx("data/definitive canopy temperature.xlsx", "Sensor_Database") %>%
  mutate_if(is.character,as.factor) %>%  #mutate the character fields to factor
  rename(Irrigation_type = Irrigation_Type, sensor_type = Sensor_type, experiment_name = Experiment_Name, 
         planting_year = Planting_Year, treatment = Treatment, location = Location, plot_no = Plot_Number, 
         row_configuration = Row_Configuration, Planting_day_of_year =  "Planting_Date_DOY",
         lint_yield_kg_per_ha = Lint_Yield_kg_ha, fibre_length_mm = Fibre_Length_mm, micronaire = Micronaire, 
         fibre_strength_g_tex = "Fibre_Strength_g/tex")
dtc <- select(Base_dtc, experiment_name, planting_year) %>% 
  unique()



#this is the files to be appended. 
append_file <- read.csv("data/metadata_files_for_consolidation/1718_CanopyTemperatureSensor_MetadataFile.csv") %>% 
  mutate_if(is.character,as.factor) %>%  #mutate the character fields to factor
  rename(Irrigation_type = Irrigation_Type, sensor_type = Sensor_type, experiment_name = Experiment_Name, 
         planting_year = Planting_Year, treatment = Treatment, location = Location, plot_no = Plot_Number, 
         row_configuration = Row_Configuration, Planting_day_of_year =  "Planting_Date_DOY")
append_list <- select(append_file, experiment_name, planting_year) %>% 
  unique()

###APPEND using the rbind function
append_rows <-  rbind(Base_dtc, append_file)
