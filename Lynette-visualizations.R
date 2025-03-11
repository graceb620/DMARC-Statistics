rm(list=ls())
library(ggplot2)
library(dplyr)


hh_data_2023<-read.csv('Data/hh_data23.csv', stringsAsFactors=FALSE)

summary(hh_data_2023)




#Scatter plot of income vs poverty level
ggplot(hh_data_2023, aes(x = income_2023, y = fed_poverty_level_2023)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Income vs. Poverty Level",
       x = "Income in 2023",
       y = "Federal Poverty Level (%)") +
  theme_minimal()
