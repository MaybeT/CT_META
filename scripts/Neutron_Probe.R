#Neuton Probe processing script

library(tidyverse)
library(lubridate)
library(readxl)


#read in the raw data file

#neutron probe raw data
nmm_raw <- read_delim("G:\\COTPhys_Current\\2019-2020\\Trials\\1920_Limited_Water_A2\\raw_data\\NMM_raw_data\\nmm.txt", delim = " ", trim_ws = TRUE, col_names = FALSE) 

#limited water plot description  
plotMeta <- read_csv("G:\\COTPhys_Current\\2019-2020\\Trials\\1920_Limited_Water_A2\\raw_data\\NMM_raw_data\\LW_plot_list.csv")  

#name columns
nmm_df <- rename(nmm_raw, index = X1, plot = X2, date = X4, time = X5, r20 = X6, r30 = X7, r40 = X8, r50 = X9, r60 = X10, r80 = X11, r100 = X12, r120 = X13)

#take out X3 and time columns

nmm_df2 <- nmm_df %>% 
  mutate(date = (as.Date(date,"%m/%d/%y"))) %>% 
  select(-X3, -time )

#gather the data
gather_nmm <- gather(nmm_df2, key = "depth", value = count, "r20":"r120")

#sort the data 
nmm <- gather_nmm %>% 
  group_by(date, plot) %>% 
  mutate(skip = ifelse(index == min(index), "skip", "plant"))

#Join the metadata of the plot descriptions

join_meta <- left_join(nmm, plotMeta,by = c("plot"="Plot"))

write_csv(join_meta, )






