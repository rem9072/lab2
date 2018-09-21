library(tidyverse)


#Tidy Data

lab2 <- read.csv("C:/Users/rfmag/Downloads/lab2.csv")
lab2mod<-lab2 %>%
  gather("base_sport","base_qol","base_pain","first_sport","first_qol","first_pain","second_sport", "second_qol", "second_pain", key="time_category", value="score")%>% 
  separate("time_category", c("time","category"), sep= "_") 

#Recreate Graph

lab2mod %>%
  group_by(category,time)%>%
  summarize(avg=mean(score),stdev=sd(score)) %>%
  ggplot(mapping=aes(x=time, y=avg, group=category, color=category)) + 
    geom_line(position=position_dodge(0.1)) + 
  geom_point(position=position_dodge(0.1)) +
  geom_errorbar(aes(ymin=avg-stdev,ymax=avg+stdev),width=0.05,position=position_dodge(0.1)) +
  labs(x ="Time of report",y ="Sample mean and standard deviation") +
  scale_x_discrete(labels= c("baseline","one year","two years")) +
  ylim(0,100) 
  

#Tidy Data part 2

coverage <- read.csv("C:/Users/rfmag/Downloads/coverage.csv",skip=2)
expend <- read.csv("C:/Users/rfmag/Downloads/expenditures.csv",skip=2)

coveragemod<-coverage[-c(68:53),] %>%
  gather(2:29, key="year_coverage", value="number")%>% 
  separate("year_coverage", c("year","coverage type"), sep= "__")

expendmod <- expend[-c(61:53),]%>%
  select(matches("location|2013|2014")) %>%
  gather(2:3, key="year", value="cost") %>% 
  separate("year", c("year","total health spending"), sep= "__") %>%
  select(one_of(c("Location","year","cost")))

expend_coverage <- full_join(coveragemod,expendmod) %>%
  separate("year", c("x","year"), sep= "X") %>%
  select(-x)
        
