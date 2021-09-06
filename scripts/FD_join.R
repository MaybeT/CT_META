#Just a join

library(tidyverse)
library(lubridate)
library(readxl)

#read in files; excel file name and sheet name or number
readin <- read_xlsx("M:/AusBIOTIC database/2014-15/GrowerTc_2014-15/May-be-important_files/Mean time between irrigations.xlsx") %>%
  colnames(readin)
  #mutate_if(is.character,as.factor) #mutate the character fields to factor
  # rename(Irrigation_type = Irrigation_Type, sensor_type = Sensor_type, experiment_name = Experiment_Name, 
  #        planting_year = Planting_Year, treatment = Treatment, location = Location, plot_no = Plot_Number, 
  #        row_configuration = Row_Configuration, planting_day_of_year = "Planting_Date_(DOY)", variety = Variety,
  #        lint_yield_kg_per_ha = Lint_Yield_kg_ha, fibre_length_mm = Fibre_Length_mm, micronaire = Micronaire, 
  #        fibre_strength_g.tex = "Fibre_Strength_(g/tex)")
  
Join1 <- filter(readin, planting_year == 2012) %>% 
  mutate(lint_yield_kg_per_ha = (as.numeric(lint_yield_kg_per_ha))) %>%
  mutate(temp_exp_no = (as.factor(temp_exp_no))) %>% 
  mutate(planting_day_of_year = (as.numeric(planting_day_of_year))) %>% 
  mutate(row_configuration = (as.factor(row_configuration))) %>% 
  mutate(temp_rep_no = (as.numeric(temp_rep_no))) %>% ##104 obs 
  mutate_at(vars(starts_with("Irrigation_Date")), .funs = dmy)

summary(Join1)

glimpse(Join1)
levels(Join1$experiment_name) 


Join2 <- read.csv("data/add_to_DAP.csv") %>% 
  filter(planting_year == 2012) %>% 
  mutate_if(is.character,as.factor) %>%   #mutate the character fields to factor
  mutate(temp_rep_no = (as.numeric(temp_rep_no))) %>%
  mutate(temp_exp_no = (as.factor(temp_exp_no))) %>%
  mutate(row_configuration = (as.factor(row_configuration))) %>% 
  mutate(lint_yield_kg_per_ha = (as.numeric(lint_yield_kg_per_ha))) %>% 
  mutate(planting_day_of_year = (as.numeric(planting_day_of_year))) %>% 
  mutate_at(vars(starts_with("Irrigation_Date")), .funs = dmy)


 
join_wc_cnt <- full_join(Join1, by = c("sensor_type", "planting_year", "location", "plot_no", "temp_exp_no", "temp_trt_no", "temp_rep_no", "Irrigation_type", "lint_yield_kg_per_ha", "fibre_length_mm", "micronaire", "fibre_strength_g.tex"),Join2)

f1 <- filter(join_wc_cnt, !is.na(join_id.y))
f2 <- filter(join_wc_cnt, is.na(join_id.y))
write_csv(join_wc_cnt, "data/join_wc2.csv")
write_csv(f2, "data/misjoin_wc.csv")
anti_join_wc_cnt <- anti_join(Join1, distinct = c("sensor_type", "planting_year", "location", "plot_no", "temp_exp_no", "temp_trt_no", "temp_rep_no", "variety", "Irrigation_type", "lint_yield_kg_per_ha", "fibre_length_mm", "micronaire", "fibre_strength_g.tex"),Join2)

#get column names for comparitive tables.
levels(Join1$treatment)
levels(Join2$treatment)
levels(Join1$experiment_name)

colnames(Join1)
colnames(Join2)

#glimpse, which is better with many columns
glimpse(Join1)
glimpse(Join2)

#summary table
summary(Join1)
summary(Join2)


#list the categories in a field - "levels"

anti <- anti_join(distinct(Join1Sum, experiment_name), distinct(Join2Sum, experiment_name)) #what is in Join1 but not Join2
pro <- anti_join(distinct(Join2, experiment_name), distinct(Join1, experiment_name))
unique(anti$planting_year)

#compare categories from grouped values
anti <- anti_join(distinct(Join1_by, experiment_name), distinct(Join2_by, experiment_name))
pro <- anti_join(distinct(Join2, experiment_name), distinct(Join1, experiment_name))
levels(anti$experiment_name)
levels(anti$planting_year)


#match rows in columns
#
compare <- Join1 %>% semi_join(join_wc_cnt)
anti_compare <- Join1 %>% anti_join(join_wc_cnt)

compare2 <- Join2 %>% semi_join(join_wc_cnt)
anti_compare2 <- Join2 %>% anti_join(join_wc_cnt)

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
