#sensor list then summarise data in the files, from Phenonet. 
library(tidyverse)
library(lubridate)
library(readxl)

#Read files in a folder

#2016-17 SD Card reads
sensor_SD <- read_csv("//nexus.csiro.au/Home/M/may159/1617_Arducrop_SDCards/ExtractedFiles/BS2000.csv")
sensor_SD <- read_csv("//mvpc76-mv/C$/Users/may159/Downloads/1819_Analysis/1100/1100Output160119.csv")
sensor_SD_file <- rename(sensor_SD, UID = "ID", Timestamp = "time", Value = "AV Object temperature (c)", SDev = "SD Object temperature (c)", device_temp = "AV Object temperature (c)_1", SDev_device = "SD Object temperature (c)_1", num_obs = "num observations", batt_level =  "batt level")            
#2017-18 SD card reads
sensor_SD <- read_csv("//mvpc76-mv/C$/Users/may159/Downloads/1718_Analysis/SDCardDL/2000SD/2000_endDate.csv")

sensor_SD_file <- rename(sensor_SD, UID = "ID", Timestamp = "time", Value = "AV Object temperature (c)", SDev = "SD Object temperature (c)", device_temp = "AV Object temperature (c)_1", SDev_device = "SD Object temperature (c)_1", num_obs = "num observations", batt_level =  "batt level")            

              
#check column names
colnames(sensor_SD_file)   
str(sensor_SD_file)
#Summarise with UID from SD card file to see the date range present for the SD card

SD_min_max <- group_by(sensor_SD_file, UID) %>% 
  summarise(min_date = min(Timestamp, na.rm = TRUE),
            max_date = max(Timestamp, na.rm = TRUE),
            max = max(Value, na.rm = TRUE),
            min = min(Value, na.rm = TRUE),
            count = n())         


#create a tibble to work with. Select out the date range and required vectors
SensorExtract <-   sensor_SD_file %>% 
mutate(Value = as.numeric(Value),
       Timestamp = ymd_hms(Timestamp, tz = "Australia/Sydney")) %>%
  filter(Timestamp >= "2016-11-23" & Timestamp <= "2017-06-01") %>% 
  filter(between(Value, 5,50)) %>% 
  select(UID, Value, Timestamp)

#Average min and max 
#groupby (SensorExtract, UID, day(Timestamp)
#summarise(min(min = min(Value, na.rm = TRUE)))


#summarise filtered date values

Mod_min_max <- group_by(SensorExtract, UID) %>% 
  summarise(min_date = min(Timestamp, na.rm = TRUE),
            max_date = max(Timestamp, na.rm = TRUE),
            min = min(Value, na.rm = TRUE),
            max = max(Value, na.rm = TRUE),
            count = n())         
write_csv(Mod_min_max,"data/summary_CCF_2016_17.csv")
#------------

#check table structure
str(SensorExtract)

#Visualise all sensors in list
ggplot(SensorExtract, aes(x = Timestamp, y = Value))+
  geom_point()+
  facet_wrap(~UID)

#Visualise all sensors in list
#filter data between 11 and 50 C

SensorExtract_filter <- 
ggplot(SensorExtract, aes(x = Timestamp, y = Value))+
  geom_line()+
  facet_wrap(~UID)

#view each sensor in a "for" loop for a closer look at each individual sensor. 

sensors <- unique(SensorExtract$UID)

plotUID <- sensors[i]

i = 2

#loops all sensors to create a plot for all sensors in loop
for (i in 1:length(sensors)){
  
  plotUID <- sensors[i]
  
  g1 <- ggplot(filter(SensorExtract, UID == plotUID), aes(x = Timestamp, y = Value))+
    geom_line()+
    labs(
      x = "Time",              # x axis title
      y = "CT",   # y axis title
      title = plotUID)      # main title of figure
  # theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
  
  print(g1)
  
}
ggplot(filter(SensorExtract, UID == plotUID), aes(x = Timestamp, y = Value))+
 geom_line()+
  labs(
    x = "Time",              # x axis title
    y = "CT",   # y axis title
    title = plotUID)      # main title of figure
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
  


# create a table of all the SensorIDs in the data file
sensortable <- SensorExtract %>% distinct(UID) %>% 

SensorExtract <- filter(SensorExtract, str_detect(UID, "^...$")) %>% 
  filter(Timestamp < Sys.time()) %>% 
  #filter(Datetime >= "2017/12/01"& Datetime <= "2018/12/01") %>% #Use this to filter date time by selecting date.
  filter(between(Value, 5, 50)) %>% 
  


# create a table of all the SensorIDs in the data file

# use a for loop to go filter the data by each sensor ID 
for (i in 1:nrow(sensortable)) {
  
  SensorID <- sensortable$UID[i] %>% 
  
  BySensor <- SensorExtract %>% 
    filter(UID == SensorID) %>%
    select(-UID) %>% # remove ID column as the output csv will contain this informaiton
    mutate(Timestamp = floor_date(Timestamp,unit = "minutes")) %>%  #round to nearest minute
  
    
    #clean up sensorDat, aggregate to minutes
    #filter for unique values
    
    group_by(Datetime) %>%
    summarise(CanopyTemp = mean(CanopyTemp))
  
  
  SaveFileName <- paste(savePath,"/s",SensorID,".csv",sep = "")
  
  try(write_csv(mutate(BySensor,Datetime=format(Datetime, "%F %H:%M:%S")),
                SaveFileName))
}

