library(ggplot2)
library(dplyr)

hh_data_2023<-read.csv('Data/hh_data23.csv', stringsAsFactors=FALSE)

summary(hh_data_2023)

#creating a category for ranges of federal poverty level:
hh_data_2023$poverty_cat = cut(hh_data_2023$fed_poverty_level_first_visit,
                              c(0,100,150,200,250,300,350,999))

hh_data_2023$first_visit_2023 <- as.factor(hh_data_2023$first_visit_2023)

#ggplot to compare 
ggplot(hh_data_2023, aes(x=poverty_cat)) + 
  geom_bar(aes(fill=first_visit_2023),position = "dodge") + theme(legend.position = "top")