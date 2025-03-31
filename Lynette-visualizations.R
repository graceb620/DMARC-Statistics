rm(list=ls())
library(ggplot2)
library(dplyr)


hh_data_2023<-read.csv('Data/hh_data23.csv', stringsAsFactors=FALSE)

summary(hh_data_2023)

#Household size distribution for first time visitors of 2023 vs other years
hh_data_2023 %>%
  mutate(first_visit_2023 = ifelse(year(first_visit) == 2023, "2023", "Returning visitors")) %>%
  filter(n_people_in_household <= 12) %>%
  ggplot(aes(x = as.factor(n_people_in_household), fill = first_visit_2023)) +
  geom_bar(position = "stack") +
  labs(
    title = "Household Size Distribution",
    x = "Number of People in Household",
    y = "Count of households",
    fill = "Year of first visit"
  ) +
  scale_fill_manual(values = c("2023" = "#CE1256", "Returning visitors" = "#0072B2")) + 
  theme_minimal()

#stacked bar chart with the count would do better 
#Change the wording to first visitors to returning visitors 
#change the heading too 
#reduce the number on the chart



