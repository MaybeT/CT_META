#Oh Compare mon cher
#Tracey May
# Created 1/09/2019
#modified #12/11/2019

library(tidyverse)
library(lubridate)
library(readxl)

#read in files; excel file name and sheet name or number - this is the "original" file
meta1 <- read_xlsx("M:/AusBIOTIC database/Definative_cT_Raw/definitive canopy temperature.xlsx", "Sensor_Database") %>%
  mutate_if(is.character,as.factor) %>%  #mutate the character fields to factor
  rename(Irrigation_type = Irrigation_Type, sensor_type = Sensor_type, experiment_name = Experiment_Name, 
         planting_year = Planting_Year, treatment = Treatment, location = Location, plot_no = Plot_Number, 
         row_configuration = Row_Configuration, Planting_day_of_year =  "Planting_Date_DOY",
         lint_yield_kg_per_ha = Lint_Yield_kg_ha, fibre_length_mm = Fibre_Length_mm, micronaire = Micronaire, 
         fibre_strength_g.tex = "Fibre_Strength_g/tex")
levels(meta1$location)


#these are the files already on the DAP
 meta2 <- read.csv("data/cotton_canopy_sensor_agronomic_data.csv")
levels(meta2$location)
 meta3 <- read_xlsx("data/Definitive_metadata.xlsx", "Sensor_Database") %>% 
   mutate_if(is.character,as.factor) %>%  #mutate the character fields to factor
   rename(Irrigation_type = Irrigation_Type, sensor_type = Sensor_type, experiment_name = Experiment_Name, 
          planting_year = planting_year, treatment = Treatment, location = Location, plot_no = Plot_Number, 
          row_configuration = Row_Configuration, Planting_day_of_year =  "Planting_Date_(DOY)",
          lint_yield_kg_per_ha = Lint_Yield_kg_ha, fibre_length_mm = Fibre_Length_mm, micronaire = Micronaire, 
          fibre_strength_g.tex = "Fibre_Strength_(g/tex)")
 
 meta4 <- read_csv("data/add_to_DAP.csv") %>% 
      mutate_if(is.character,as.factor)
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

#Summarise the table to give a count of a grouped value "experiement name", "planting year"
meta1Sum <- meta1 %>% group_by(experiment_name, planting_year, location)
meta1_by <- meta1Sum %>% summarise(MetaData = n())

meta2Sum <- meta2 %>% group_by(experiment_name, planting_year, location)
meta2_by <- meta2Sum %>% summarise(inDAP = n())

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

#comparing internal data


###################################################################################################

# comparing and listing what is to be added to DAP file (filename = Add_to_DAP)

#Summarise the table to give a count of a grouped value "experiement name", "planting year"

meta4Sum <- meta4 %>% group_by(experiment_name, planting_year, location)
meta4_by <- meta4Sum %>% summarise(nAdd = n())

files_to_be_added_to_DAP <- full_join(meta1_by,meta2_by) %>% 
                           full_join(.,meta4_by)
                           
 sort(files_to_be_added_to_DAP, by= planting_year) %>% write_csv(files_to_be_added_to_DAP, "data/FilesToBeAdded_DAP_TM.csv")


anti <- anti_join(distinct(meta1Sum, experiment_name), distinct(meta2Sum, experiment_name)) #what is in meta1 but not meta2
pro <- anti_join(distinct(meta2, experiment_name), distinct(meta1, experiment_name))
unique(anti$planting_year)

#compare categories from grouped values
anti <- anti_join(distinct(meta1_by, experiment_name,planting_year), distinct(meta2_by, experiment_name,planting_year))
levels(anti$experiment_name)
levels(anti$planting_year)

pro <- anti_join(distinct(meta4, experiment_name, planting_year), distinct(meta1, experiment_name, planting_year))
levels(anti$experiment_name)
levels(anti$planting_year)




