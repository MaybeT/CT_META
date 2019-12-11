library(tidyverse)
library(readxl)
library (lubridate)

#read in data
FF <- read_excel("M:/2012-2013/Trials/Limited Water B2/Raw Data/B2 1st Flower/B2 LW 1ST Flower 1213.xls") %>% 
     mutate(Treatment = as.factor(FF$Treatment))

#visualise with plot      
ggplot(FF, aes(x=Plot, y=DAS, colour = Treatment))+
           geom_point()

#filter data frame and filter out na values
FF_df <- select(FF,Treatment, Plot, DAS) %>%
          filter(!is.na(DAS)) %>% 
          group_by(Treatment)

FFx <- FF_df %>% summarise(FFx1 = mean(DAS))



 


