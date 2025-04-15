library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(TeachingDemos)

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
rm(list=ls()) 

hh_data_2023<-read.csv('Data/hh_data23.csv', stringsAsFactors=FALSE)

hh_data_2023$poverty_cat <- cut(hh_data_2023$fed_poverty_level_first_visit,
                                c(0,160,999))

hh_data_2023 <- hh_data_2023 %>% filter(poverty_cat!="NA") 


hh_data_2023$snap_first_2023 <- factor(hh_data_2023$snap_first_2023, 
                                        levels = c(0, 1), 
                                        labels = c("Do NOT receive SNAP", "Do receive SNAP"))


hh_data_2023$first_visit_2023 <- factor(hh_data_2023$first_visit_2023, 
                                        levels = c(0, 1), 
                                        labels = c("Visited before 2023", "First Timer in 2023"))

hh_data_2023$poverty_cat <- factor(hh_data_2023$poverty_cat, 
                                        levels = c("(0,160]", "(160,999]"), 
                                        labels = c("below 160% Federal Povery Level\n- might qualify for SNAP benefits", 
                                                   "above 160% Federal Povery Level\n- will not qualify for SNAP benefits*"))


ggplot(hh_data_2023, aes(x = poverty_cat, fill = first_visit_2023)) +
  geom_bar(color = "black") +  
  geom_text(color= "white", stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5)) +  
  xlab("Threshold Categories") + 
  ylab("Count of Households Within the Category") +
  labs(
    title = "Difference in Count between Households that Meet Threshold to Qualify for SNAP Benefits",
    subtitle = "For Households That Visited DMARC Pantries in 2023",
    fill = "Household Type",
    caption="*There are some exceptions. To learn more, go to 'Iowa Department of Health and Human Services - Facts About SNAP'"
  ) + theme_light() + theme(axis.text.x = element_text(size = 11)) + 
  scale_y_continuous(breaks = seq(0, max(table(hh_data_2023$poverty_cat)), by = 1000)) +
  scale_fill_brewer(palette = "Accent") 

#whether they took out snap benefits rather than return vs first time
ggplot(hh_data_2023, aes(x = poverty_cat, fill = snap_first_2023)) +
  geom_bar(color = "black") +  
  xlab("Threshold Categories") + 
  ylab("Count of Households Within the Category") +
  labs(
    title = "Difference in Count between Households that Meet Threshold to Qualify for SNAP Benefits",
    subtitle = "For Households That Visited DMARC Pantries in 2023",
    fill = " ",
    caption="*There are some exceptions. To learn more, go to 'Iowa Department of Health and Human Services - Facts About SNAP'"
  ) + theme_light() + theme(axis.text.x = element_text(size = 11)) +
  scale_y_continuous(breaks = seq(0, max(table(hh_data_2023$poverty_cat)), by = 1000)) +
  scale_fill_brewer(palette = "Spectral") 


#closer to original
ggplot(hh_data_2023, aes(x=poverty_cat)) + 
  geom_bar(aes(fill=first_visit_2023)) + xlab("Federal Poverty Level Categories") + 
  ylab("Count of Households") +
  labs(title="Counts of visitors that first visited in 2023 versus other years",
       subtitle="comparison between categories of those that visited DMARC Pantries in 2023")

# proportions in case I want that
ggplot(hh_data_percent, aes(x = poverty_cat, y=percent, fill = percent)) + 
  geom_bar(stat = "identity", aes(fill=first_visit_2023)) + xlab("Categories") + 
  ylab("percentage of households within the category") +
  labs(title="Proportions of first-time visitors and returners within federal poverty level categories",
       subtitle="of those that visited DMARC pantries in 2023")
