library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(TeachingDemos)
library(tidyverse)

rm(list=ls())

hh_data_2023orig<-read.csv('Data/hh_data23.csv', stringsAsFactors=FALSE)
hh_data_2023<-read.csv('Data/hh_data23.csv', stringsAsFactors=FALSE)
monthly_count<-read.csv('Data/monthly_count2.csv', stringsAsFactors=FALSE)
quarter_count <- read.csv('Data/quarter_count2.csv', stringsAsFactors=FALSE)

monthly_count<-monthly_count%>%filter(year(round_month)>=2020&year(round_month)<2025)
quarter_count<-quarter_count%>%filter(year(round_quarter)>=2020&year(round_quarter)<2025)
monthly_count$round_month <- as.Date(monthly_count$round_month) # I have no clue why round_month is a character 
quarter_count$round_quarter <- as.Date(quarter_count$round_quarter) # I have no clue why round_quarter is a character 

summary(quarter_count)

# # median income of households------
# ggplot(hh_data_2023, aes(x = first_visit_2023, y = household_income_median, group=first_visit_2023)) + 
#   geom_boxplot() 
# 
# # There are a few outliers with median income above 200000 - remove them
# hh_23out <- hh_data_2023 %>% 
#   filter(household_income_median < 200000) %>% 
#   mutate(first_visit_2023= ifelse(first_visit_2023==1, "first visit in 2023", "first visit before 2023"))
# # redoing the box plot
# ggplot(hh_23out, aes(x = first_visit_2023, y = household_income_median, group=first_visit_2023)) + 
#   geom_boxplot() + xlab("First Visit") + ylab("Median Household Income") +
#   labs(title="Comparison of median household income of visitors in 2023 below $200,000",
#        subtitle="between households who first visited in 2023 and who first visited earlier")
# 
# # let's remove the households that do not have any income
# hh_23out <- hh_data_2023 %>% 
#   filter(household_income_median < 200000&household_income_median != 0) %>% 
#   mutate(first_visit_2023= ifelse(first_visit_2023==1, "first visit in 2023", "first visit before 2023"))
# 
# ggplot(hh_23out, aes(x = first_visit_2023, y = household_income_median, group=first_visit_2023)) + 
#   geom_boxplot() + xlab("First Visit") + ylab("Median Household Income") +
#   labs(title="Comparison of median household income of visitors in 2023 above $0 and below $200,000",
#        subtitle="between households who first visited in 2023 and who first visited earlier")


# comparing counts of people on snap in first visit of 2023 vs last visit ------

snap_diff<-sum(hh_data_2023$snap_last_2023)-sum(hh_data_2023$snap_first_2023)
snap_diff

# creating a category for ranges of federal poverty level --------

hh_data_2023$poverty_cat <- cut(hh_data_2023$fed_poverty_level_first_visit,
                                c(0,160,999))

hh_data_2023$snap <- factor(hh_data_2023$snap, 
                            levels = c(0, 1), 
                            labels = c("Have NEVER received SNAP", "Have received SNAP at some point"))

hh_data_2023$snap_last_2023 <- factor(hh_data_2023$snap_last_2023, 
                            levels = c(0, 1), 
                            labels = c("Did NOT receive SNAP", "Did receive SNAP"))

hh_data_2023$snap_first_2023 <- factor(hh_data_2023$snap_first_2023, 
                                      levels = c(0, 1), 
                                      labels = c("Did NOT receive SNAP", "Did receive SNAP"))

hh_data_2023$first_visit_2023 <- factor(hh_data_2023$first_visit_2023, 
                                        levels = c(0, 1), 
                                        labels = c("Visited before 2023", "First Timer in 2023"))

hh_data_2023$poverty_cat <- factor(hh_data_2023$poverty_cat, 
                                   levels = c("(0,160]", "(160,999]"), 
                                   labels = c("below 160% Federal Poverty Level\n- might qualify for SNAP benefits", 
                                              "above 160% Federal Poverty Level\n- will not qualify for SNAP benefits*"))

hh_data_2023 <- hh_data_2023 %>% 
  mutate(poverty_cat=replace_na(as.character(poverty_cat),"Unknown Federal Poverty level"))

# graphs for 2023 --------
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
    fill = "During time of first 2023 visit",
    caption="*There are some exceptions. To learn more, go to 'Iowa Department of Health and Human Services - Facts About SNAP'"
  ) + theme_light() + theme(axis.text.x = element_text(size = 11)) +
  scale_y_continuous(breaks = seq(0, max(table(hh_data_2023$poverty_cat)), by = 1000)) +
  scale_fill_brewer(palette = "Spectral") 

#whether they took out snap benefits EVER
ggplot(hh_data_2023, aes(x = poverty_cat, fill = snap)) +
  geom_bar(color = "black") +  
  xlab("Threshold Categories") + 
  ylab("Count of Households Within the Category") +
  labs(
    title = "Difference in Count between Households that Meet Threshold to Qualify for SNAP Benefits",
    subtitle = "For Households That Visited DMARC Pantries in 2023",
    fill = "Have taken SNAP benefits at some point between 2018-2023",
    caption="*There are some exceptions. To learn more, go to 'Iowa Department of Health and Human Services - Facts About SNAP'"
  ) + theme_light() + theme(axis.text.x = element_text(size = 11)) +
  scale_y_continuous(breaks = seq(0, max(table(hh_data_2023$poverty_cat)), by = 1000)) +
  scale_fill_brewer(palette = "Spectral") 

#whether they took out snap benefits THEIR MOST RECENT visit
ggplot(hh_data_2023, aes(x = poverty_cat, fill = snap_last_2023)) +
  geom_bar(color = "black") +  
  xlab("Threshold Categories") + 
  ylab("Count of Households Within the Category") +
  labs(
    title = "Difference in Count between Households that Meet Threshold to Qualify for SNAP Benefits",
    subtitle = "For Households That Visited DMARC Pantries in 2023",
    fill = "During time of LAST 2023 visit",
    caption="*There are some exceptions. To learn more, go to 'Iowa Department of Health and Human Services - Facts About SNAP'"
  ) + theme_light() + theme(axis.text.x = element_text(size = 11)) +
  scale_y_continuous(breaks = seq(0, max(table(hh_data_2023$poverty_cat)), by = 1000)) +
  scale_fill_brewer(palette = "Spectral") 

#comparison of first vs last visit

max_y <- hh_data_2023 %>%
  count(snap_first_2023) %>%
  pull(n) %>%
  max()

ggplot(hh_data_2023, aes(x = snap_last_2023)) +
  geom_bar() +  
  geom_text(stat = "count", aes(label = after_stat(count)), 
            position = position_stack(vjust = 0.5), size = 4, color = "white") +
  xlab("SNAP benefit categories") + 
  ylab("Count of Households") +
  labs(
    title = "Count of households with SNAP benefits at time of LAST visit",
    subtitle = "For Households That Visited DMARC Pantries in 2023"
  ) + theme_light() + theme(axis.text.x = element_text(size = 11)) +
  scale_y_continuous(limits = c(0, max_y),breaks = seq(0, max_y, by = 1000))

#comparison of first vs last visit
ggplot(hh_data_2023, aes(x = snap_first_2023)) +
  geom_bar(color = "black") +  
  geom_text(stat = "count", aes(label = after_stat(count)), 
            position = position_stack(vjust = 0.5), size = 4, color = "white") +
  xlab("SNAP benefit categories") + 
  ylab("Count of Households") +
  labs(
    title = "Count of households with SNAP benefits at time of FIRST visit",
    subtitle = "For Households That Visited DMARC Pantries in 2023"
  ) + theme_light() + theme(axis.text.x = element_text(size = 11)) +
  scale_y_continuous(breaks = seq(0, max_y, by = 1000)) 

# graphs for over-time under threshold but no snap -------
scale_factor <- max(quarter_count$num_nosnap_threshold, na.rm = TRUE) / 
  max(quarter_count$percent_nosnap_threshold, na.rm = TRUE)

ggplot(quarter_count, aes(x = round_quarter)) +
  geom_col(aes(y = num_nosnap_threshold, fill = "Count" )) +
  geom_line(aes(y = percent_nosnap_threshold * scale_factor, color = "Percentage \nout of all income eligible visitors"), size = 1.2, group = 1) +
  scale_y_continuous(
    name = "Count",
    sec.axis = sec_axis(~./scale_factor, name = "Percentage (in %) ")
  ) + 
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(title = "Income eligible visitors that do NOT take out foodstamps (SNAP)",
       subtitle = "using the maximum threshold of 160% federal poverty level",
       x = "Quarters",
       y = "Count",
       fill="Legend",
       color="Legend") +
  scale_fill_manual(values = c("Count" = "darkgrey")) +
  scale_color_manual(values = c("Percentage \nout of all income eligible visitors" = "black")) +
  theme_minimal()

# #closer to original
# ggplot(hh_data_2023, aes(x=poverty_cat)) + 
#   geom_bar(aes(fill=first_visit_2023)) + xlab("Federal Poverty Level Categories") + 
#   ylab("Count of Households") +
#   labs(title="Counts of visitors that first visited in 2023 versus other years",
#        subtitle="comparison between categories of those that visited DMARC Pantries in 2023")
# 
# # proportions in case I want that
# ggplot(hh_data_percent, aes(x = poverty_cat, y=percent, fill = percent)) + 
#   geom_bar(stat = "identity", aes(fill=first_visit_2023)) + xlab("Categories") + 
#   ylab("percentage of households within the category") +
#   labs(title="Proportions of first-time visitors and returners within federal poverty level categories",
#        subtitle="of those that visited DMARC pantries in 2023")
