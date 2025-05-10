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

# counts of people on snap in first visit of 2023 vs last visit ------

snap_diff<-sum(hh_data_2023$snap_last_2023)-sum(hh_data_2023$snap_first_2023)
snap_diff # at least over 5 thousand people gained snap between their first and last visit

# Data cleaning: creating a category for ranges of federal poverty level --------

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

# graphs for 2023: federal poverty level threshold analysis --------

# Visited before 2023 vs first timer in 2023 --------
# not easy to read and interpret meaningfully
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

# Whether they took out snap benefits at all -------

# At the time of their FIRST visit:
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

#whether they took out snap benefits EVER:
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

#whether they took out snap benefits their MOST RECENT visit:
ggplot(hh_data_2023, aes(x = poverty_cat, fill = snap_last_2023)) +
  geom_bar(color = "black") +  
  xlab("Threshold Categories") + 
  ylab("Count of Households Within the Category") +
  labs(
    title = "Difference in Count between Households that Meet Threshold to Qualify for SNAP Benefits",
    subtitle = "For Households That Visited DMARC Pantries in 2023",
    fill = "During time of MOST RECENT 2023 visit",
    caption="*There are some exceptions. To learn more, go to 'Iowa Department of Health and Human Services - Facts About SNAP'"
  ) + theme_light() + theme(axis.text.x = element_text(size = 11)) +
  scale_y_continuous(breaks = seq(0, max(table(hh_data_2023$poverty_cat)), by = 1000)) +
  scale_fill_brewer(palette = "Spectral") 

# comparison of if they received SNAP at first time vs last -------------

# to keep the y axis the same for easier comparison:
max_y <- hh_data_2023 %>%
  count(snap_first_2023) %>%
  pull(n) %>%
  max()

# First graph: last visit
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

# Second graph: first visit
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
  scale_fill_manual(values = c("Yes" = "#1f77b4", "No" = "#ff7f0e"))
  scale_y_continuous(breaks = seq(0, max_y, by = 1000)) 

# over-time bar graph for when under federal poverty level threshold and without snap -------
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