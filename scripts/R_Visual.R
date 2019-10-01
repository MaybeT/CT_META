# clear R of all objects
rm(list=ls())

# Packages:
library(tidyverse)
library(readxl)

# Specify and load the data file
#------------

gapminder <- read_csv("data/gapminder.csv")


ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

# to show life expectancy over time

ggplot(data = gapminder, aes(x = year, y = lifeExp, color = continent)) +
  geom_point()

#now as lines
ggplot(data = gapminder, aes(x=year, y=lifeExp, by=country, color=continent)) +
  geom_line()

# now lines and points

ggplot(data = gapminder, aes(x=year, y=lifeExp, by=country, color=continent)) +
  geom_line() + 
  geom_point()

#layer order mapping to data needs to be inside the aesthetic aes() function
ggplot(data = gapminder, aes(x=year, y=lifeExp, by=country)) +
  geom_line(aes(color=continent)) + 
  geom_point()
# points at the back, layer order
ggplot(data = gapminder, aes(x=year, y=lifeExp, by=country)) +
  geom_point() + geom_line(aes(color=continent))

#visual mapping to the aes functions. 
##example of how we do this

ggplot(data = gapminder, aes(x=year, y=lifeExp, colour = continent, by=country)) +
  geom_line() + 
  geom_point(colour = "black")


#Transformations

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color=continent)) +
  geom_point()

#change scale of x axis

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, colour = continent)) +
  geom_point() + 
  scale_x_log10()

#viewing trends in your data
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_smooth (method = "lm", colour = "black")+
  geom_point (aes(colour = continent)) + 
  scale_x_log10()
  
#inside the aes R maps to data, outside it becomes a constant eg; always make this line colour black


ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(size=1.5, color="green") + scale_x_log10() +
  geom_smooth(method="lm", size=1.5)


#change the shape and colour of different points
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point(size=3, shape=14) + scale_x_log10() +
  geom_smooth(method="lm", size=1.5)


##multi panel figures, uses facet-wrap. define how it is broken up 

gapminder_sub <- gapminder %>% 
  filter(continent == "Americas")

ggplot(gapminder_sub, aes(x = year, y = lifeExp, by = country)) +
    geom_line()+
  facet_wrap(~country)

#######Come back to this - modifying text skipped ########

plot(data = az.countries, aes(x = year, y = lifeExp, color=continent)) +
  geom_line() + facet_wrap( ~ country) +
  labs(
    x = "Year",              # x axis title
    y = "Life expectancy",   # y axis title
    title = "Figure 1",      # main title of figure
    color = "Continent"      # title of legend
  ) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())


##Exporting the plot

lifeExp_plot <- ggplot(gapminder_sub, aes(x = year, y = lifeExp, by = country)) +
  geom_line()+
  facet_wrap(~country)

ggsave("figures/lifeExp.png", lifeExp_plot) ### this was saving 

###KnitR knits code and english for a chunk of code. 





### There are lots of stuff to change. Following how to change axis texts. 

### Do the challenges. 








