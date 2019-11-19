#Joins for quality data

library(tidyverse)
library(readxl)
library (lubridate)
library(janitor)

#read in file
#Metadata File
meta1 <- read_xlsx("M:/AusBIOTIC database/Definative_cT_Raw/definitive canopy temperature.xlsx", "Sensor_Database") %>%
  mutate_if(is.character,as.factor) %>%  #mutate the character fields to factor
  rename(Irrigation_type = Irrigation_Type, sensor_type = Sensor_type, experiment_name = Experiment_Name, 
         planting_year = Planting_Year, treatment = Treatment, location = Location, plot_no = Plot_Number, 
         row_configuration = Row_Configuration, Planting_day_of_year =  "Planting_Date_DOY",
         lint_yield_kg_per_ha = Lint_Yield_kg_ha, fibre_length_mm = Fibre_Length_mm, micronaire = Micronaire, 
         fibre_strength_g.tex = "Fibre_Strength_g/tex")
meta1Filter <- filter(meta1, planting_year == 2012 & experiment_name == "LimitedWater") 
  mutate(meta1Filter, str_remove(treatment, ", 1 in 1 out"))


#2012/13 Quanlity and Yield data from Limited Water Maturity pick Data

Quality12 <- read_xlsx("//nexus.csiro.au/CSIRO/Agriculture/Operations Agriculture/Myall Vale/Groups/COTPhys/2012-2013/Trials/Limited Water B2/Raw Data/Fibre Quality/Limited Water Maturity Pick HVI Data 1213.xlsx") %>% 
  
Yield12 <-  read_csv("//nexus.csiro.au/CSIRO/Agriculture/Operations Agriculture/Myall Vale/Groups/COTPhys/2012-2013/Trials/Limited Water B2/Working/Yield/Yield.csv")

