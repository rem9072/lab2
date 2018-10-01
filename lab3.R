#Reed Magleby
#Lab Assignemnt 3

library(tidyverse)
library(gapminder)

#Problem 1
europe <- filter(gapminder,continent =="Europe"& gdpPercap>=40000 & year==2007)
 
filter(gapminder,year==2007) %>%
ggplot(aes(x=gdpPercap, y=lifeExp)) +
  geom_point(size=2) +
  geom_text(data=europe, mapping=aes(x=gdpPercap, y=lifeExp, label=country), color="blue",vjust= 1.5) +
  labs(x="GDP per capita",y="life expectancy")

#Problem 2
filter(gapminder,year==2007) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp)) +
  geom_point(shape=21,color="black",fill="white",size=1.5,stroke=1.5) +
  geom_rect(aes(xmin=39000,xmax=51000,ymin=75,ymax=85), fill="red",alpha=0.002) +
  geom_text(aes(x=39000,y=75,label="Countries with highest GDP"),hjust=-0.01,vjust=1)+
  labs(x="GDP per capita",y="life expectancy")           
