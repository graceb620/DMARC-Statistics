rm(list=ls()) 
library(tidyverse)
library(RColorBrewer)
library(reshape2)
library(randomForest)
library(caret)  # For model evaluation

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
train.df <- train.df %>%
  select(first_visit_2023, n_people_in_household, elderly, child, working_age, 
         single_parent, income_first, income_avg, fed_poverty_level_first, 
         snap, snap_first_visit, one_change_location, more_than_one_change_location, 
         first_housing_type, own_or_buying)

test.df <- test.df %>%
  select(first_visit_2023, n_people_in_household, elderly, child, working_age, 
         single_parent, income_first, income_avg, fed_poverty_level_first, 
         snap, snap_first_visit, one_change_location, more_than_one_change_location, 
         first_housing_type, own_or_buying)

# There were missing valuesin the elderly, child, and working_age columns
train.df <- na.omit(train.df)


# Fit Random Forest Model ------------------------------------------------------
rforest <- randomForest(first_visit_2023 ~ ., 
                        data = train.df, 
                        ntree = 1000, 
                        mtry = 3, 
                        importance = TRUE)


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

# Tune Model by Adjusting `mtry` -----------------------------------------------
tuned_rf <- tuneRF(train.df[, -1], train.df$first_visit_2023, stepFactor = 1.5, improve = 0.01, ntreeTry = 500)

# Test Again with Adjusted `mtry` ----------------------------------------------
best_mtry <- tuned_rf[which.min(tuned_rf[,2]), 1]  # Find best mtry
rforest_tuned <- randomForest(first_visit_2023 ~ ., 
                              data = train.df, 
                              ntree = 1000, 
                              mtry = best_mtry, 
                              importance = TRUE,
                              classwt = c(1, 1.5)) # Give more weight to minority class (?)

# Predictions on Test Set (Tuned Model) ----------------------------------------
pred_tuned <- predict(rforest_tuned, test.df)
conf_matrix_tuned <- confusionMatrix(pred_tuned, test.df$first_visit_2023)
print(conf_matrix_tuned)

# Extract and Compare Accuracy
accuracy_tuned <- conf_matrix_tuned$overall["Accuracy"]
cat("Tuned Model Accuracy:", accuracy_tuned, "\n")

# The improvement is noticeable, though not huge, which suggests the original model was already well-configured

