#2018/19 Arducrop allocations.
#This script is to align dates and numbers in several sheet to make sense of the arducrop allocations which 
#were improperly recorded and are somewhat unsubstantiated. 

library(tidyverse)
library(readxl)
library(lubridate)

#read in the spreadsheet for 2018/19

Allocations_1819 <- read_xlsx("//fsact.nexus.csiro.au/ANF-Share1/MyallVale/Groups/COTPhys/2018-2019/Protocols/1819_CanopyTemperatureSensorAllocation.xlsx", "1819_ArduAllocations") %>% 
  arrange(sensor_no)


allocation_1819_unknown <- read_xlsx("//fsact.nexus.csiro.au/ANF-Share1/MyallVale/Groups/COTPhys/2018-2019/Protocols/1819_CanopyTemperatureSensorAllocation.xlsx", "ArduChecklistNot sure date") %>% 
                arrange(F, Plot)
  
allocation_1819_futurefarm <- read_xlsx("//fsact.nexus.csiro.au/ANF-Share1/MyallVale/Groups/COTPhys/2018-2019/Protocols/1819_CanopyTemperatureSensorAllocation.xlsx", "A3 future farm") %>% 
                arrange(sensor_no)

ArduA2_ChangeList300119 <- read_xlsx("//fsact.nexus.csiro.au/ANF-Share1/MyallVale/Groups/COTPhys/2018-2019/Protocols/1819_CanopyTemperatureSensorAllocation.xlsx", "ArduA2_ChangeList300119") %>% 
                  arrange(sensor_no)
Final_sensor_A2 <- read_xlsx("//fsact.nexus.csiro.au/ANF-Share1/MyallVale/Groups/COTPhys/2018-2019/Protocols/1819_CanopyTemperatureSensorAllocation.xlsx", "A2finalsensorlist100419") %>% 
                  arrange(sensor_no)
phenonetHHJ <- read_csv("//fsact.nexus.csiro.au/ANF-Share1/MyallVale/Groups/COTPhys/2018-2019/CanopyTemperature/1819_Data/PhenoNet/phenoNET_20190321.csv")
