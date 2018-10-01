library(tidyverse)
library(gapminder)


#Problem 1
x <- (seq(-4, 4, by=0.01))
y <- dnorm(x)
norm <- data.frame(x=x, y=y)
 ggplot(norm,aes(x,y)) +
  geom_line() +
  geom_ribbon(data = norm[norm$x < -1,], aes(x,ymax=y,ymin=0),fill="red",color="black",alpha=1) +
   theme_classic()

#Problem 2
filter(gapminder,year==2007) %>%
ggplot(aes(x=gdpPercap, y=lifeExp)) +
  geom_point() +
  geom_text((aes(x=gapminder[country=="Europe"],label=country)))