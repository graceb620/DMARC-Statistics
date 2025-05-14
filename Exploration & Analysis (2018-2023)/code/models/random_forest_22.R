# Main contributor: Zofia Landowska 
rm(list=ls())
library(tidyverse)
library(RColorBrewer)
library(reshape2)
library(randomForest)
library(caret) # For model evaluation
library(pROC)
library(pdp)

# Read in data -----------------------------------------------------------------
hh_22 <- read.csv('Data/hh_data22.csv',stringsAsFactors=FALSE)

# Convert target variable to factor --------------------------------------------
hh_22$first_visit_2022 <- as.factor(hh_22$first_visit_2022)

# Split Data into Train and Test -----------------------------------------------
RNGkind(sample.kind = "default")
set.seed(13032025)
train.idx <- sample(x = 1:nrow(hh_22), size = 0.8*nrow(hh_22))
train.df <- hh_22[train.idx, ]
test.df <- hh_22[-train.idx, ]

# Select relevant predictor variables (avoiding future-dependent ones)----------
train.df <- train.df %>% select(first_visit_2022, n_people_in_household, snap, snap_first_visit, snap_change_2022, household_income_median, fed_poverty_level_first, last_homeless_state, first_housing_type, last_housing_type, own_or_buying, more_than_one_change_location, elderly, child, working_age, college_education, single_parent)
test.df <- test.df %>% select(first_visit_2022, n_people_in_household, snap, snap_first_visit, snap_change_2022, household_income_median, fed_poverty_level_first, last_homeless_state, first_housing_type, last_housing_type, own_or_buying, more_than_one_change_location, elderly, child, working_age, college_education, single_parent)

# There were missing values in the elderly, child, and working_age columns
train.df <- na.omit(train.df)

# Fit Random Forest Model ------------------------------------------------------
rforest <- randomForest(first_visit_2022 ~ ., data = train.df, ntree = 1000, mtry = 3, importance = TRUE)

# Print model summary
print(rforest)

# Plot variable importance
varImpPlot(rforest, type = 1)

# Predictions on Test Set ------------------------------------------------------
pred <- predict(rforest, test.df)

# Evaluate Model Performance ---------------------------------------------------
conf_matrix <- confusionMatrix(pred, test.df$first_visit_2022)
print(conf_matrix)

# Extract Accuracy
accuracy <- conf_matrix$overall["Accuracy"]
cat("Model Accuracy:", accuracy, "\n")

# Tune Model by Adjusting mtry -----------------------------------------------
tuned_rf <- tuneRF(train.df[, -1], train.df$first_visit_2022, stepFactor = 1.5, improve = 0.01, ntreeTry = 500)

# Test Again with Adjusted mtry ----------------------------------------------
best_mtry <- tuned_rf[which.min(tuned_rf[,2]), 1] # Find best mtry
rforest_tuned <- randomForest(first_visit_2022 ~ ., data = train.df, ntree = 1000, mtry = best_mtry, importance = TRUE, classwt = c(1, 1.5)) # Give more weight to minority class (?)

# Predictions on Test Set (Tuned Model) ----------------------------------------
pred_tuned <- predict(rforest_tuned, test.df)
conf_matrix_tuned <- confusionMatrix(pred_tuned, test.df$first_visit_2022)
print(conf_matrix_tuned)

# Convert predictions to probability scores
prob_tuned <- predict(rforest_tuned, test.df, type = "prob")

# Compute ROC curve and AUC for the positive class (assuming "1" is the positive class)
roc_curve <- roc(test.df$first_visit_2022, prob_tuned[, 2], levels = rev(levels(test.df$first_visit_2022)))

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
  labs(title = "Factor Importance in Predicting First Visit in 2022", x = "Factor", y = "Importance (Mean Decrease in Accuracy)") +
  theme_minimal()

