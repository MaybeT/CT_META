
library(tidyverse)
library(lubridate)

senors <- read_csv("G:/COTPhys/AusBIOTIC database/Definative_cT_Raw/2012/ACRI/2012_RefinedScheduling/PHEN348.csv") %>% 
    mutate(Datetime = dmy_hm(Datetime))

ggplot(sensors, aes(x=date_time, y=canopy_temp, colour = UID))+
  geom_point(alpha=0.3, size= 0.5, colour = "black")+
  geom_point(data = filter(CT_sensors_cleaned, !between(canopy_temp, 5, 50)),
             aes(x=date_time, y=canopy_temp), colour = "red")+
  facet_wrap(~UID)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# ggplot(senors, aes(Datetime, Canopytemp))+
#   geom_point()

CT_data_location <- "//fsact.nexus.csiro.au/ANF-Share1/MyallVale/Groups/COTPhys/AusBIOTIC database/Definative_cT_Raw/2012/ACRI/2012_RefinedScheduling"
RDS_data_files_location <- "data/RDS_data_files"

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
#  all CT data single column name "Canopytemp"
#  all date times single column name "Datetime"
#  Sensor UID from filename
#  Remove all rows with NAs
CT_data_clean <- CT_data_raw %>% 
  mutate(canopy_temp = coalesce(Canopytemp),
         date_time = coalesce(Datetime)) %>% 
  mutate(UID = basename(filename)) %>% 
  mutate(UID = basename(filename)) %>% 
  mutate(UID = str_remove(UID,".csv"))%>% 
  select(UID, date_time, canopy_temp) %>% 
  filter(!is.na(date_time)) %>% 
  filter(!is.na(canopy_temp))

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

counts_tab <- CT_sensors_cleaned %>% 
  group_by(UID) %>% 
  count()


summary_tab <- CT_sensors_cleaned %>% 
  group_by(UID) %>% 
  summarise(min_date = (min(date_time)),
         max_date = max(date_time), reads = n()) %>% 
  mutate(min_date = as.Date(min_date)) %>% 
  mutate(max_date = as.Date(max_date))

write_csv(summary_tab, "data/summary_sensors.csv")

#unfiltered records plot with facet wrap
ggplot(CT_sensors_cleaned, aes(x=date_time, y=canopy_temp, colour = UID))+
  geom_point(alpha=0.3, size= 0.5, colour = "black")+
  geom_point(data = filter(CT_sensors_cleaned, !between(canopy_temp, 5, 50)),
             aes(x=date_time, y=canopy_temp), colour = "red")+
  facet_wrap(~UID)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#select individual sensor to view
filter_sensor <- filter(CT_sensors_cleaned, UID== "PHEN337")

ggplot(filter_sensor, aes(x=date_time, y=canopy_temp, colour = UID))+
  geom_point(alpha=0.3, size= 0.5, colour = "black")+
  geom_point(data = filter(filter_sensor, !between(canopy_temp, 5, 50)),
             aes(x=date_time, y=canopy_temp), colour = "red")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


  
saveRDS(CT_sensors_cleaned, file.path(RDS_data_files_location,"canopy-temperatures-cleaned_V2.rds"))


