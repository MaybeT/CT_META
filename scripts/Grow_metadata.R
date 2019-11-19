##Growers Metadata
##Sorting out the growers metadata and files from 2014_15



# Created 15/11/2019
#modified #

library(tidyverse)
library(lubridate)
library(readxl)

##read in the file we have.
gd_list <- read_xlsx("data/Summary_growerTC_14-15.xlsx", "locations") %>%
  mutate_if(is.character,as.factor) #mutate the character fields to factor

gd_list <- read_xlsx("data/Summary_growerTC_14-15.xlsx", "locations") %>%
  mutate_if(is.character,as.factor) #mutate the character fields to factor
  


#these are the files already on the DAP
meta2 <- read.csv("data/cotton_canopy_sensor_agronomic_data.csv")

meta3 <- read_xlsx("data/Definitive_metadata.xlsx", "Sensor_Database") %>% 
  mutate_if(is.character,as.factor) %>%  #mutate the character fields to factor
  rename(Irrigation_type = Irrigation_Type, sensor_type = Sensor_type, experiment_name = Experiment_Name, 
         planting_year = planting_year, treatment = Treatment, location = Location, plot_no = Plot_Number, 
         row_configuration = Row_Configuration, Planting_day_of_year =  "Planting_Date_(DOY)",
         lint_yield_kg_per_ha = Lint_Yield_kg_ha, fibre_length_mm = Fibre_Length_mm, micronaire = Micronaire, 
         fibre_strength_g.tex = "Fibre_Strength_(g/tex)")

meta4 <- read_csv("data/add_to_DAP.csv")

#get column names for comparitive tables. 
colnames(meta1)
colnames(meta2)
colnames(meta4)


#look at header data or glimpse, which is better with many columns
head(meta1, 5)
head(meta2, 5)
glimpse(meta1)
glimpse(meta2)

#summary table
summary(meta1)
summary(meta2)