# loading libraries
library(tidyverse)
library(dplyr)
library(countrycode)
library(ggtext)
library(patchwork)

# loading data

water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')
View(water)

water$regionint = countrycode(water$country_name, origin = "country.name",destination ="un.regionintermediate.name")

#plot

water$regionint <- factor(water$regionint , levels=c("Eastern Africa", "Middle Africa", "Western Africa", "Southern Africa","South America"))

water %>% filter(!is.na(water_source)) %>%
  filter(!is.na((regionint)))%>%
  group_by(regionint,water_source)%>% tally() %>%
  mutate(ws= ifelse(water_source=='Borehole','Borehole','Other'))%>%
  group_by(regionint,ws)%>%tally(n)%>%mutate(prop=n/sum(n))%>%
  ggplot(aes(x=regionint, y=prop,fill=ws))+
  geom_col(alpha = 0.85)+
  scale_fill_manual(values=c("plum4","darkcyan"))+
  scale_y_continuous(label=scales::percent_format())+
  labs(x="Regions",y="Percentage", title ="Water sources in Africa and South America",fill="Water Sources")+
  theme_minimal()



