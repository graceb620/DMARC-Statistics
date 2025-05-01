

# Load libraries
library(dplyr)
library(ggplot2)

# Create income brackets
hh_data_24$income_bracket <- cut(hh_data_24$annualIncome,
                                 breaks = c(-Inf, 10000, 20000, 40000, Inf),
                                 labels = c("<$10k", "$10k–20k", "$20k–40k", "$40k+"))

# Calculate prevalence
prevalence_by_bracket <- hh_data_24 %>%
  group_by(income_bracket) %>%
  summarise(
    total = n(),
    with_dietary_issue = sum(dietary_issue == 1, na.rm = TRUE),
    prevalence = with_dietary_issue / total
  )

# Plot
ggplot(prevalence_by_bracket, aes(x = income_bracket, y = prevalence)) +
  geom_col(fill = "darkorange") +
  labs(title = "Prevalence of Dietary Issues by Income Bracket",
       x = "Income Bracket", y = "Proportion with Dietary Issues") +
  theme_minimal()



hh_data_24 %>%
  group_by(foodstamps, dietary_issue) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = factor(foodstamps), y = count, fill = factor(dietary_issue))) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of Dietary Issues by Food Stamp Usage",
       x = "Received Food Stamps (0 = No, 1 = Yes)",
       y = "Proportion", fill = "Dietary Issue") +
  theme_minimal()



# Filter for each year
diet_2023 <- hh_data_24 %>% filter(first_visit_2023 == 1)
diet_2024 <- hh_data_24 %>% filter(first_visit_2024 == 1)

# Combine into one frame with labels
diet_trend <- bind_rows(
  diet_2023 %>% mutate(year = "2023"),
  diet_2024 %>% mutate(year = "2024")
)

# Calculate proportions
diet_over_time <- diet_trend %>%
  group_by(year) %>%
  summarise(
    total = n(),
    with_dietary_issue = sum(dietary_issue == 1, na.rm = TRUE),
    prevalence = with_dietary_issue / total
  )

# Plot
ggplot(diet_over_time, aes(x = year, y = prevalence)) +
  geom_col(fill = "seagreen") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Change in Dietary Issue Prevalence (2023 vs 2024)",
       x = "Year", y = "Prevalence") +
  theme_minimal()



