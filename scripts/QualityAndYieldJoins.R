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
         fibre_strength_g.tex = "Fibre_Strength_g/tex") %>% 
  mutate(plot_no = as.character(plot_no)) %>% 
  mutate(experiment_name = as.character(experiment_name))

  
# meta1Filter <- filter(meta1, planting_year == 2012 & experiment_name == "LimitedWater") %>% 
#   select(lint_yield_kg_per_ha, everything())


#2012/13 Yield data from Limited Water Maturity pick Data

Yield12 <-  read_csv("//nexus.csiro.au/CSIRO/Agriculture/Operations Agriculture/Myall Vale/Groups/COTPhys/2012-2013/Trials/Limited Water B2/Working/Yield/Yield.csv") %>% 
    rename(lint_m2 = "Lint/m2", temp_rep_no = Rep) %>% #rename columns is another way to join different column names (vectors)
    mutate(temp_rep_no = as.factor(temp_rep_no)) %>% 
    mutate(planting_year = 2012) %>%
    mutate(experiment_name = as.character("LimitedWater")) %>% 
    mutate(plot_no = as.character(plot_no)) %>% 
    mutate(lint_yield_kg_per_ha = lint_m2*10) %>% 
  select(planting_year, experiment_name, plot_no, lint_yield_kg_per_ha)
 
#use janitor package to compare matching columns, anticipating what the joins will do!
compare_df_cols(Yield12,meta1)

#Create the Yield12 join and update the metadata sheet using full_join and case_when
join_Yield12_meta1 <- full_join(meta1,Yield12, by = c("planting_year", "plot_no", "experiment_name")) %>% 
    mutate(lint_yield_kg_per_ha = case_when(!is.na(lint_yield_kg_per_ha.y)~lint_yield_kg_per_ha.y,!is.na(lint_yield_kg_per_ha.x)~lint_yield_kg_per_ha.x,TRUE~NA_real_)) %>% 
 select(lint_yield_kg_per_ha, lint_yield_kg_per_ha.x, lint_yield_kg_per_ha.y, everything())
 
#2012/13 fibre quality data from Limited water maturity pick Data

Quality12 <- read_xlsx("//nexus.csiro.au/CSIRO/Agriculture/Operations Agriculture/Myall Vale/Groups/COTPhys/2012-2013/Trials/Limited Water B2/Raw Data/Fibre Quality/Limited Water Maturity Pick HVI Data 1213.xlsx") %>%  
    rename(plot_no = "Plot") %>% 
    rename(fibre_length_mm = len,fibre_strength_g.tex = "str", micronaire = mic) %>% 
    mutate(planting_year = 2012) %>%
    mutate(experiment_name = as.character("LimitedWater")) %>% 
    mutate(plot_no = as.character(plot_no)) %>% 
    select(planting_year, experiment_name, plot_no, fibre_length_mm, fibre_strength_g.tex, micronaire)

compare_df_cols(Quality12,meta1)    
## force join by different column names
#To join by different variables on x and y use a named vector. For example, by = c("a" = "b") will match x.a to y.b

join_Quality12_meta1 <-full_join(meta1, Quality12, by = c("planting_year","plot_no", "experiment_name")) %>% 
    mutate(fibre_strength_g.tex =case_when(!is.na(fibre_strength_g.tex.y)~fibre_strength_g.tex.y, !is.na(fibre_strength_g.tex.x)~fibre_strength_g.tex.x, TRUE~NA_real_)) %>% 
    mutate(fibre_length_mm = case_when(!is.na(fibre_length_mm.y)~fibre_length_mm.y, !is.na(fibre_length_mm.x)~fibre_length_mm.x, TRUE~NA_real_)) %>% 
    mutate(micronaire = case_when(!is.na(micronaire.y)~micronaire.y, !is.na(micronaire.x)~micronaire.x, TRUE~NA_real_)) %>% 
  select(planting_year, experiment_name, plot_no, lint_yield_kg_per_ha, fibre_length_mm, fibre_strength_g.tex, micronaire, everything())

