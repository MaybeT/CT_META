#Oh Compare mon cher
#Tracey May
# Last Modified 2019/08/23

library(tidyverse)
library(lubridate)
library(readxl)

#read in files; excel file name and sheet name or number
meta1 <- read_xlsx("data/definitive canopy temperature.xlsx", "Sensor_Database") %>%
  mutate_if(is.character,as.factor) %>%  #mutate the character fields to factor
  rename(Irrigation_type = Irrigation_Type, sensor_type = Sensor_type, experiment_name = Experiment_Name, 
         planting_year = Planting_Year, treatment = Treatment, location = Location, plot_no = Plot_Number, 
         row_configuration = Row_Configuration, Planting_day_of_year =  "Planting_Date_(DOY)",
         lint_yield_kg_per_ha = Lint_Yield_kg_ha, fibre_length_mm = Fibre_Length_mm, micronaire = Micronaire, 
         fibre_strength_g.tex = "Fibre_Strength_(g/tex)")

 meta2 <- read.csv("data/cotton_canopy_sensor_agronomic_data.csv")

 meta3 <- read_xlsx("data/Definitive_metadata.xlsx", "Sensor_Database") %>% 
   mutate_if(is.character,as.factor) %>%  #mutate the character fields to factor
   rename(Irrigation_type = Irrigation_Type, sensor_type = Sensor_type, experiment_name = Experiment_Name, 
          planting_year = planting_year, treatment = Treatment, location = Location, plot_no = Plot_Number, 
          row_configuration = Row_Configuration, Planting_day_of_year =  "Planting_Date_(DOY)",
          lint_yield_kg_per_ha = Lint_Yield_kg_ha, fibre_length_mm = Fibre_Length_mm, micronaire = Micronaire, 
          fibre_strength_g.tex = "Fibre_Strength_(g/tex)")
 
#get column names for comparitive tables. 
colnames(meta1)
colnames(meta2)


#look at header data or glimpse, which is better with many columns
head(meta1, 5)
head(meta2, 5)
glimpse(meta1)
glimpse(meta2)

#summary table
summary(meta1)
summary(meta2)

#Summarise the table to give a count of a grouped value "experiement name", "planting year"
meta1Sum <- meta1 %>% group_by(experiment_name, planting_year)
meta1_by <- meta1Sum %>% summarise(n1 = n())

meta2Sum <- meta2 %>% group_by(experiment_name, planting_year)
meta2_by <- meta2Sum %>% summarise(n2 = n())

join_meta_cnt <- full_join(meta1_by,meta2_by)
write_csv(join_meta_cnt, "data/join_meta_count.csv")
           

#list the categories in a field - "levels"

anti <- anti_join(distinct(meta1Sum, experiment_name), distinct(meta2Sum, experiment_name)) #what is in meta1 but not meta2
pro <- anti_join(distinct(meta2, experiment_name), distinct(meta1, experiment_name))
unique(anti$planting_year)

#compare categories from grouped values
anti <- anti_join(distinct(meta1_by, experiment_name,planting_year), distinct(meta2_by, experiment_name,planting_year))
levels(anti$experiment_name)
levels(anti$planting_year)

pro <- anti_join(distinct(meta2, experiment_name, planting_year), distinct(meta1, experiment_name, planting_year))
levels(anti$experiment_name)
levels(anti$planting_year)


#match rows in columns
#
compare <- meta1 %>% semi_join(meta2, by = "Sensor_Unique_Identifier")
anti_compare <- meta1 %>% anti_join(meta2, by = "Sensor_Unique_Identifier")


band_members %>% inner_join(band_instruments)
band_members %>% left_join(band_instruments)
band_members %>% right_join(band_instruments)
band_members %>% full_join(band_instruments)

# "Filtering" joins keep cases from the LHS
band_members %>% semi_join(band_instruments)
band_members %>% ?anti_join(band_instruments)

# "Nesting" joins keep cases from the LHS and nests the RHS
band_members %>% nest_join(band_instruments)

# To suppress the message, supply by
band_members %>% inner_join(band_instruments, by = "name")
# This is good practice in production code

# Use a named `by` if the join variables have different names
band_members %>% full_join(band_instruments2, by = c("name" = "artist"))
# Note that only the key from the LHS is kept

#select columns to go into the dataframe


