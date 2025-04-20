library(ggplot2)
library(dplyr)

hh_data_2023<-read.csv('Data/hh_data23.csv', stringsAsFactors=FALSE)

summary(hh_data_2023)

# median income of households------
ggplot(hh_data_2023, aes(x = first_visit_2023, y = household_income_median, group=first_visit_2023)) + 
  geom_boxplot() 

# There are a few outliers with median income above 200000 - remove them
hh_23out <- hh_data_2023 %>% 
  filter(household_income_median < 200000) %>% 
  mutate(first_visit_2023= ifelse(first_visit_2023==1, "first visit in 2023", "first visit before 2023"))
# redoing the box plot
ggplot(hh_23out, aes(x = first_visit_2023, y = household_income_median, group=first_visit_2023)) + 
  geom_boxplot() + xlab("First Visit") + ylab("Median Household Income") +
  labs(title="Comparison of median household income of visitors in 2023 below $200,000",
       subtitle="between households who first visited in 2023 and who first visited earlier")

# let's remove the households that do not have any income
hh_23out <- hh_data_2023 %>% 
  filter(household_income_median < 200000&household_income_median != 0) %>% 
  mutate(first_visit_2023= ifelse(first_visit_2023==1, "first visit in 2023", "first visit before 2023"))

ggplot(hh_23out, aes(x = first_visit_2023, y = household_income_median, group=first_visit_2023)) + 
  geom_boxplot() + xlab("First Visit") + ylab("Median Household Income") +
  labs(title="Comparison of median household income of visitors in 2023 above $0 and below $200,000",
       subtitle="between households who first visited in 2023 and who first visited earlier")


# creating a category for ranges of federal poverty level ---------
hh_data_2023<-read.csv('Data/hh_data23.csv', stringsAsFactors=FALSE)
hh_data_2023$poverty_cat = cut(hh_data_2023$fed_poverty_level_first_visit,
                              c(0,100,150,200,999))

hh_data_2023$first_visit_2023 <- as.factor(hh_data_2023$first_visit_2023)

hh_data_2023 <- filter(poverty_cat=="NA")

# ggplot to compare 
ggplot(hh_data_2023, aes(x=poverty_cat)) + 
  geom_bar(aes(fill=first_visit_2023),position = "dodge") + xlab("First Visit") + 
  ylab("Median Household Income") +
  labs(title="Comparison of median household income of visitors in 2023 above $0 and below $200,000",
       subtitle="between households who first visited in 2023 and who first visited earlier")
