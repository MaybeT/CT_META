#sensor list then summarise data in the files, from Phenonet. 
library(tidyverse)
library(lubridate)
library(readxl)

#Read files in a folder

#PhenoNET_cT <- read_csv("//mvpc76-mv/C$/Users/may159/Downloads/1819_Analysis/1819_Data/2019-02-18_canopy-temperatures-cleaned.csv") %>% 
 # rename(UID = SensorSerial, Timestamp = LocalTime, Value = CanopyTemp)

PhenoNET_cT <- read_csv("//fsact.nexus.csiro.au/ANF-Share1/MyallVale/Groups/COTPhys/2018-2019/CanopyTemperature/1819_Data/PhenoNet/phenoNET_20190321.csv") %>% 
      rename(UID = "Sensor Serial", Timestamp = "Local Time", Value ="Canopy Temp (°C)")


colnames(PhenoNET_cT)   

#Summarise with UID from phenonet file

min_max <- group_by(PhenoNET_cT, UID) %>% 
  summarise(min_date = min(Timestamp, na.rm = TRUE),
            max_date = max(Timestamp, na.rm = TRUE),
            min = min(Value, na.rm = TRUE),
            max = max(Value, na.rm = TRUE),
            count = n())

#create a tibble to work with. 

SensorExtract <-   PhenoNET_cT %>% 
mutate(Value = as.numeric(Value),
       Timestamp = ymd_hms(Timestamp, tz = "Australia/Sydney")) %>%
  select(UID, Value, Timestamp)
#------------


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

