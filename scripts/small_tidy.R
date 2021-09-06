library(tidyverse)

df <- data_frame(species = c("A", "B"),
                 male = c(5, 8),
                 female = c(2, 6))

df_tidy <- df %>% 
  gather(key = sex, value = count, -species)
df_tidy

df_wide <- df_tidy %>% 
  spread(key = sex, value = count)

df_tidy

#gather is going from wide to long
#spread is going from long to wide
df_tidy %>% 
  spread(key = species, value = count)


gather(key = "irrigation", value = "irrig_date", starts_with("Irrigation_date")) %>% 
  mutate(irrig_date_2 = dmy(irrig_date))

Join <-   spread(JoinG, key = "irrigation", value = "irrig_date_2")

# have a look - has it worked?#filter(!is.na(irrig_date))

# planting_year %in% c(2012,2014) filter syntax