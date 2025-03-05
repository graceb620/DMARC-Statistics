rm(list=ls()) 
library(tidyverse)
library(caret)  # For model evaluation
library(glmnet)  # For Ridge and Lasso models
library(pROC)   

# Read in data -----------------------------------------------------------------
hh_23 <- read.csv('Data/hh_data23.csv', stringsAsFactors = FALSE)

# Convert target variable to factor --------------------------------------------
hh_23$first_visit_2023 <- as.factor(hh_23$first_visit_2023)

# Split Data into Train and Test -----------------------------------------------
set.seed(13032025)
train.idx <- sample(x = 1:nrow(hh_23), size = 0.8 * nrow(hh_23))
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

# Remove rows with missing values in the train and test sets
train.df <- train.df[complete.cases(train.df), ]
test.df <- test.df[complete.cases(test.df), ]

# Recreate x.train and y.train after removing NAs
x.train <- model.matrix(first_visit_2023 ~ n_people_in_household + elderly + child + working_age +
                          single_parent + income_first + income_avg + fed_poverty_level_first +
                          snap + snap_first_visit + one_change_location + more_than_one_change_location +
                          first_housing_type + own_or_buying, data = train.df)[,-1]
x.test <- model.matrix(first_visit_2023 ~ n_people_in_household + elderly + child + working_age +
                         single_parent + income_first + income_avg + fed_poverty_level_first +
                         snap + snap_first_visit + one_change_location + more_than_one_change_location +
                         first_housing_type + own_or_buying, data = test.df)[,-1]

# Define the target variable for regression
y.train <- as.vector(train.df$first_visit_2023)
y.test <- as.vector(test.df$first_visit_2023)

# Check if the number of rows in x.train matches the length of y.train
if (nrow(x.train) != length(y.train)) {
  stop("The number of rows in x.train does not match the length of y.train.")
}

# Fit models -------------------------------------------------------------------
# Fit the Lasso and Ridge models using cross-validation to select lambda
lr_lasso_cv <- cv.glmnet(x.train, y.train, family = "binomial", alpha = 1)
lr_ridge_cv <- cv.glmnet(x.train, y.train, family = "binomial", alpha = 0)

# Plot the results for cross-validation
plot(lr_lasso_cv)
plot(lr_ridge_cv)

# Extract optimal lambda values
best_lasso_lambda <- lr_lasso_cv$lambda.min
best_ridge_lambda <- lr_ridge_cv$lambda.min

# Fit final Lasso and Ridge models with the best lambda
final_lasso <- glmnet(x.train, y.train, family = "binomial", alpha = 1, lambda = best_lasso_lambda)
final_ridge <- glmnet(x.train, y.train, family = "binomial", alpha = 0, lambda = best_ridge_lambda)

# Predict on the test data
test.df.preds <- test.df %>%
  mutate(
    lasso_pred = predict(final_lasso, x.test, type = "response")[,1],
    ridge_pred = predict(final_ridge, x.test, type = "response")[,1]
  )

# Calculate ROC curves for Lasso and Ridge models
lasso_rocCurve <- roc(response = as.factor(test.df.preds$first_visit_2023),
                      predictor = test.df.preds$lasso_pred, levels = c("0", "1"))

ridge_rocCurve <- roc(response = as.factor(test.df.preds$first_visit_2023),
                      predictor = test.df.preds$ridge_pred, levels = c("0", "1"))

# Plot ROC curves for both models
plot(lasso_rocCurve, print.thres = TRUE, print.auc = TRUE)
plot(ridge_rocCurve, print.thres = TRUE, print.auc = TRUE)

# Create data frames for ROC curve information
lasso_data <- data.frame(
  Model = "Lasso",
  Specificity = lasso_rocCurve$specificities,
  Sensitivity = lasso_rocCurve$sensitivities,
  AUC = as.numeric(lasso_rocCurve$auc)
)

ridge_data <- data.frame(
  Model = "Ridge",
  Specificity = ridge_rocCurve$specificities,
  Sensitivity = ridge_rocCurve$sensitivities,
  AUC = as.numeric(ridge_rocCurve$auc)
)

# Combine all the ROC data
roc_data <- rbind(lasso_data, ridge_data)

# Plot ROC curve for both models
ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model), data = roc_data) +
  geom_text(data = roc_data %>% group_by(Model) %>% slice(1),
            aes(x = 0.75, y = c(0.75, 0.65), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 3)))) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()
