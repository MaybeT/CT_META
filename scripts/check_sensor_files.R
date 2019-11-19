#sensor list then summarise data in the files, from Sensor DB. 
library(tidyverse)
library(lubridate)
library(readxl)

#Read files in a folder

canopy_file_names <- list.files(path = "data/warren",
                                recursive = T,
                                full.names = T,
                                pattern = ".csv")



#create a dataframe reading in the files. 

df <- data_frame(filename = canopy_file_names) %>% 
  mutate(file_contents = map(filename, ~ read_csv(file.path(.),
                                                   skip = 6,
                                                   n_max = 50000,
                                                   col_names = TRUE)))

df_unnested <- unnest(df) %>% 
  mutate(UID = basename(filename)) %>% 
  mutate(UID = str_extract(UID, "PHEN\\d{3}"))

#Summarise with UID from file

min_max <- group_by(df_unnested, UID) %>% 
  summarise(max_date = max(Timestamp, na.rm = TRUE),
            min_date = min(Timestamp, na.rm = TRUE),
            max = max(Value, na.rm = TRUE),
            min = min(Value, na.rm = TRUE))

#create a tibble to work with. 

SensorExtract <-   as_tibble(df_unnested) %>% 
mutate(Value = as.numeric(Value),
       Timestamp = ymd_hms(Timestamp, tz = "Australia/Sydney")) %>%
  select(UID, Value, Timestamp)
#------------
str(SensorExtract)

ggplot(SensorExtract, aes(x = Timestamp, y = Value))+
  geom_line()+
  facet_wrap(~UID)


#view each sensor in a "for" loop for a closer look at each individual sensor. 

sensors <- unique(SensorExtract$UID)

plotUID <- sensors[i]

i = 2
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
