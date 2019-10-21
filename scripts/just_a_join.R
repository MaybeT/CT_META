#Just a join

library(tidyverse)
library(lubridate)
library(readxl)

#read in files; excel file name and sheet name or number
readin <- read_csv("data/warren_dct.csv") %>%
  mutate_if(is.character,as.factor) %>%  #mutate the character fields to factor
  rename(Irrigation_type = Irrigation_Type, sensor_type = Sensor_type, experiment_name = Experiment_Name, 
         planting_year = Planting_Year, treatment = Treatment, location = Location, plot_no = Plot_Number, 
         row_configuration = Row_Configuration, Planting_day_of_year =  "Planting_Date_(DOY)", variety = Variety,
         lint_yield_kg_per_ha = Lint_Yield_kg_ha, fibre_length_mm = Fibre_Length_mm, micronaire = Micronaire, 
         fibre_strength_g.tex = "Fibre_Strength_(g/tex)")
  
Join <- filter(readin, planting_year == 2012 & str_detect(experiment_name,"^\\d....")) %>% 
  mutate(lint_yield_kg_per_ha = (as.numeric(lint_yield_kg_per_ha))) %>% ##104 obs 
  mutate_at(vars(starts_with("Irrigation_Date")), .funs = dmy)
  
  gather(key = "irrigation", value = "irrig_date", starts_with("Irrigation_date")) %>% 
  mutate(irrig_date_2 = dmy(irrig_date))

Join <-   spread(JoinG, key = "irrigation", value = "irrig_date_2")

# have a look - has it worked?#filter(!is.na(irrig_date))

# planting_year %in% c(2012,2014) filter syntax


 
  mutate_at(vars(starts_with("Irrigation_date")), .funs = dmy)

  glimpse(Join1)

  Join2 <- read.csv("data/add_to_DAP.csv") %>% 
  mutate_if(is.character,as.factor) %>%   #mutate the character fields to factor
  mutate(temp_rep_no = (as.numeric(temp_rep_no))) %>% 
  mutate(lint_yield_kg_per_ha = (as.numeric(lint_yield_kg_per_ha)))
glimpse(Join2)
 
#get column names for comparitive tables. 
colnames(Join1)
colnames(Join2)

#glimpse, which is better with many columns
glimpse(Join1)
glimpse(Join2)

#summary table
summary(Join1)
summary(Join2)


join_wc_cnt <- full_join(Join1,Join2)
write_csv(join_meta_cnt, "data/join_wc.csv")


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
compare <- Join1 %>% semi_join(Join2, by = "Sensor_Unique_Identifier")
anti_compare <- Join1 %>% anti_join(Join2, by = "Sensor_Unique_Identifier")


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
