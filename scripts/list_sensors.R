#sensor list 
library(tidyverse)
library(lubridate)
library(readxl)


canopy_file_names <- list.files(path = "//fsact.nexus.csiro.au/ANF-Share1/MyallVale/Groups/COTPhys/AusBIOTIC database/2016-17/Emerald",
                                recursive = T,
                                full.names = T,
                                pattern = "s\\d{3}.csv")




df <- data_frame(filename = canopy_file_names) %>% 
  mutate(file_contents = map(filename, ~ read_csv(file.path(.),
                                                   skip = 0,
                                                   n_max = 50000,
                                                   col_names = TRUE,
                                                   col_types = "cn")))

df_unnested <- unnest(df) %>% 
  mutate(UID = basename(filename)) %>% 
  mutate(UID = str_remove(UID, ".csv"))


min_max <- group_by()



  as.tibble() %>% 
mutate(CanopyTemp = as.numeric(CanopyTemp),
       Datetime = ymd_hms(Datetime, tz = "Australia/Sydney")) %>%
  select(ID, CanopyTemp, Datetime)
#------------
str(SensorExtract)


# create a table of all the SensorIDs in the data file
Sensors <- SensorExtract %>% distinct(ID) 

SensorExtract <- filter(SensorExtract, str_detect(ID, "^20..$")) %>% 
  filter(Datetime < Sys.time()) %>% 
  #filter(Datetime >= "2017/12/01"& Datetime <= "2018/12/01") %>% 
  filter(between(CanopyTemp, 5, 50))


# create a table of all the SensorIDs in the data file
Sensors <- SensorExtract %>% distinct(ID) 


ggplot(SensorExtract, aes(x = Datetime, y = CanopyTemp))+
  geom_line()+
  facet_wrap(~ID)

# use a for loop to go filter the data by each sensor ID 
for (i in 1:nrow(Sensors)) {
  
  SensorID <- Sensors$ID[i]
  
  BySensor <- SensorExtract %>% 
    filter(ID == SensorID) %>%
    select(-ID) %>% # remove ID column as the output csv will contain this informaiton
    #filter(Datetime >= "2017/12/01"& Datetime <= "2018/12/01") %>% 
    #filter(CanopyTemp >= 5 & CanopyTemp <= 50) %>% 
    mutate(Datetime = floor_date(Datetime,unit = "minutes")) %>%  #round to nearest minute
    
    
    #clean up sensorDat, aggregate to minutes
    #filter for unique values
    
    group_by(Datetime) %>%
    summarise(CanopyTemp = mean(CanopyTemp))
  
  
  SaveFileName <- paste(savePath,"/s",SensorID,".csv",sep = "")
  
  try(write_csv(mutate(BySensor,Datetime=format(Datetime, "%F %H:%M:%S")),
                SaveFileName))
