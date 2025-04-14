rm(list=ls())
library(tidyverse)
library(RColorBrewer)
library(reshape2)
library(randomForest)
library(caret) # For model evaluation
library(pROC)
library(pdp)

# Read in data -----------------------------------------------------------------
hh_23 <- read.csv('Data/hh_data23.csv',stringsAsFactors=FALSE)

# Convert target variable to factor --------------------------------------------
hh_23$first_visit_2023 <- as.factor(hh_23$first_visit_2023)

# Split Data into Train and Test -----------------------------------------------
RNGkind(sample.kind = "default")
set.seed(13032025)
train.idx <- sample(x = 1:nrow(hh_23), size = 0.8*nrow(hh_23))
train.df <- hh_23[train.idx, ]
test.df <- hh_23[-train.idx, ]

# Select relevant predictor variables (avoiding future-dependent ones)----------
train.df <- train.df %>% select(first_visit_2023, n_people_in_household, snap, snap_first_visit, snap_change_2023, household_income_median, fed_poverty_level_first, last_homeless_state, first_housing_type, last_housing_type, own_or_buying, more_than_one_change_location, elderly, child, working_age, college_education, single_parent)
test.df <- test.df %>% select(first_visit_2023, n_people_in_household, snap, snap_first_visit, snap_change_2023, household_income_median, fed_poverty_level_first, last_homeless_state, first_housing_type, last_housing_type, own_or_buying, more_than_one_change_location, elderly, child, working_age, college_education, single_parent)

# There were missing values in the elderly, child, and working_age columns
train.df <- na.omit(train.df)

# Fit Random Forest Model ------------------------------------------------------
rforest <- randomForest(first_visit_2023 ~ ., data = train.df, ntree = 1000, mtry = 3, importance = TRUE)

# Print model summary
print(rforest)

# Plot variable importance
varImpPlot(rforest, type = 1)

# Predictions on Test Set ------------------------------------------------------
pred <- predict(rforest, test.df)

# Evaluate Model Performance ---------------------------------------------------
conf_matrix <- confusionMatrix(pred, test.df$first_visit_2023)
print(conf_matrix)

# Extract Accuracy
accuracy <- conf_matrix$overall["Accuracy"]
cat("Model Accuracy:", accuracy, "\n")

# Tune Model by Adjusting mtry -----------------------------------------------
tuned_rf <- tuneRF(train.df[, -1], train.df$first_visit_2023, stepFactor = 1.5, improve = 0.01, ntreeTry = 500)

# Test Again with Adjusted mtry ----------------------------------------------
best_mtry <- tuned_rf[which.min(tuned_rf[,2]), 1] # Find best mtry
rforest_tuned <- randomForest(first_visit_2023 ~ ., data = train.df, ntree = 1000, mtry = best_mtry, importance = TRUE, classwt = c(1, 1.5)) # Give more weight to minority class (?)

# Predictions on Test Set (Tuned Model) ----------------------------------------
pred_tuned <- predict(rforest_tuned, test.df)
conf_matrix_tuned <- confusionMatrix(pred_tuned, test.df$first_visit_2023)
print(conf_matrix_tuned)

# Convert predictions to probability scores
prob_tuned <- predict(rforest_tuned, test.df, type = "prob")

# Compute ROC curve and AUC for the positive class (assuming "1" is the positive class)
roc_curve <- roc(test.df$first_visit_2023, prob_tuned[, 2], levels = rev(levels(test.df$first_visit_2023)))

# Plot ROC Curve
plot(roc_curve, col = "blue", main = "ROC Curve for Tuned Random Forest Model")
abline(a = 0, b = 1, lty = 2, col = "red")

# Print AUC
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

# Extract and Compare Accuracy
accuracy_tuned <- conf_matrix_tuned$overall["Accuracy"]
cat("Tuned Model Accuracy:", accuracy_tuned, "\n")

# The improvement is noticeable, though not huge, which suggests the original model was already well-configured

# Get importance from the tuned random forest model
importance_data <- data.frame(
  Factors = rownames(importance(rforest_tuned)),
  Importance = importance(rforest_tuned, type = 1)[,1]
)

# Plot feature importance
ggplot(importance_data, aes(x = reorder(Factors, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Factor Importance in Predicting First Visit in 2023", x = "Factor", y = "Importance (Mean Decrease in Accuracy)") +
  theme_minimal()

# THIS IS SOME BS

# Partial Dependence Plots to further understand relationships
# Example for 'household_income_median'
pdp_income <- partial(rforest_tuned, pred.var = "household_income_median", prob = TRUE)
plot_pdp_income <- plotPartial(pdp_income, main = "Partial Dependence of Household Income")
print(plot_pdp_income)

# Example for 'snap_change_2023'
pdp_snap_change <- partial(rforest_tuned, pred.var = "snap_change_2023", prob = TRUE)
plot_pdp_snap_change <- plotPartial(pdp_snap_change, main = "Partial Dependence of SNAP Change")
print(plot_pdp_snap_change)

# Add a section to analyze the distribution of important features
# Example for household income
ggplot(train.df, aes(x = household_income_median)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Household Income", x = "Household Income Median") +
  theme_minimal()

# Example for snap change (categorical)
ggplot(train.df, aes(x = as.factor(snap_change_2023))) + # Treat as factor
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Distribution of SNAP Change 2023", x = "SNAP Change 2023 (0/1)") +
  theme_minimal()

# Look at people who have moved more than once 
multi_movers <- hh_23 %>% 
  filter(more_than_one_change_location == 1)

# Plot 1: Household size distribution (up to 10 people)
ggplot(multi_movers %>% filter(n_people_in_household <= 10), 
       aes(x = n_people_in_household)) +
  geom_histogram(binwidth = 1, fill = "#69b3a2", color = "black") +
  labs(title = "Household Size (Up to 10 People) for Multi-Movers",
       x = "Number of People in Household",
       y = "Count")

# Plot 2: SNAP usage
ggplot(multi_movers, aes(x = factor(snap))) +
  geom_bar(fill = "#f28500") +
  labs(title = "SNAP Usage for Those Who Moved More Than Once",
       x = "Received SNAP (0 = No, 1 = Yes)",
       y = "Count")

# Plot 3: First Homeless State
ggplot(multi_movers, aes(x = first_homeless_state)) +
  geom_bar(fill = "#b07aa1") +
  labs(title = "First Homeless State Distribution",
       x = "First Homeless State",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 4: Last Homeless State
ggplot(multi_movers, aes(x = last_homeless_state)) +
  geom_bar(fill = "#b07aa1") +
  labs(title = "FLast Homeless State Distribution",
       x = "Last Homeless State",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 5: Income over time
ggplot(multi_movers, aes(x = income_first, y = income_last)) +
  geom_point(alpha = 0.7, color = "#0072b2") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Income First vs. Last Visit (Multi-Movers)",
       x = "Income at First Visit",
       y = "Income at Last Visit")

# Save all plots to a PDF
#ggsave("multi_mover_plots_23.pdf", width = 10, height = 8)
