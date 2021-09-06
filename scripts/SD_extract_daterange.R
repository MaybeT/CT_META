#Read in different file types. 

library(tidyverse)
library(lubridate)
library(readxl)

BS1200 <- read_csv("//nexus.csiro.au/Home/M/may159/1617_Arducrop_SDCards/ARDUCROP_SD_Python/ARDUCROP_SD/1200_min_output.csv")
BS1500 <- read_csv("//nexus.csiro.au/Home/M/may159/1617_Arducrop_SDCards/ARDUCROP_SD_Python/ARDUCROP_SD/1500_min_output.csv")
  
start_end_date <- group_by(BS1500, ID) %>% 
  summarise(min_date = min(time, na.rm = TRUE),
            max_date = max(time, na.rm = TRUE))#,

clip <- head(BS1500)

write_csv(clip, "//nexus.csiro.au/Home/M/may159/1617_Arducrop_SDCards/ARDUCROP_SD_Python/ARDUCROP_SD/clip_SD_output.csv")  