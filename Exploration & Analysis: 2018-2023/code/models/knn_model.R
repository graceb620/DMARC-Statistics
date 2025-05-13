

#clear the environment
rm(list=ls())


# Load necessary libraries
library(dplyr)  
library(ggplot2)
library(tidyverse)
library(class)  
library(caret)  
library(tibble)

# Load dataset
hh_23 <- read.csv('Data/hh_data23.csv', stringsAsFactors=FALSE)

# Convert categorical variables to factors
hh_23_clean <- hh_23 %>%
  mutate(across(where(is.character), as.factor))

summary(hh_23_clean)

# Normalize numeric columns (except target variable)
num_cols <- sapply(hh_23_clean, is.numeric)
hh_23_clean[num_cols] <- scale(hh_23_clean[num_cols])

# Define predictors (X) and target variable (Y)
X <- hh_23_clean %>% select(-first_visit_2023)  # Remove target column
Y <- as.factor(hh_23_clean$first_visit_2023)  # Ensure target is a factor

# Split into training and testing sets
set.seed(123)  
train_index <- createDataPartition(Y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
Y_train <- Y[train_index]
Y_test <- Y[-train_index]


# Impute missing values
preProc <- preProcess(X_train, method = "medianImpute")  
X_train <- predict(preProc, X_train)  
X_test <- predict(preProc, X_test)  

# Verify no missing values
sum(is.na(X_train))  
sum(is.na(X_test))

# Convert to data frame
X_train <- as.data.frame(X_train)
X_test <- as.data.frame(X_test)

# Remove non-numeric columns
X_train <- X_train %>% select(where(is.numeric))
X_test <- X_test %>% select(where(is.numeric))

# Convert back to matrices (for KNN)
X_train <- as.matrix(X_train)
X_test <- as.matrix(X_test)

# Ensure Y_train and Y_test are factors
Y_train <- as.factor(Y_train)
Y_test <- as.factor(Y_test)

# Train KNN model with k = 9 (choose optimal k later)
knn_pred <- knn(train = X_train, test = X_test, cl = Y_train, k = 9)
conf_matrix <- confusionMatrix(knn_pred, Y_test)
print(conf_matrix)

accuracy_results <- data.frame(k = integer(), accuracy = numeric())

for (k in 1:20) {
  knn_pred <- knn(train = X_train, test = X_test, cl = Y_train, k = k)
  acc <- sum(knn_pred == Y_test) / length(Y_test)
  accuracy_results <- rbind(accuracy_results, data.frame(k = k, accuracy = acc))
}

# Plot accuracy vs k
ggplot(accuracy_results, aes(x = k, y = accuracy)) +
  geom_line() + geom_point() +
  ggtitle("KNN Accuracy for Different k Values") +
  xlab("k") + ylab("Accuracy")



###########trying to analyze the model###########
Y_train_numeric <- as.numeric(Y_train) 
# Compute the correlation for each feature in X_train with the numeric target Y_train_numeric
cor_target <- cor(X_train, Y_train_numeric, use = "complete.obs")

# Convert correlation results into a data frame
cor_target_df <- as.data.frame(cor_target) %>%
  rownames_to_column("Variable") %>%
  filter(!is.na(cor_target)) %>%  # Keep only non-NA correlations
  mutate(abs_corr = abs(cor_target)) %>%  # Compute absolute correlation values
  arrange(desc(abs_corr))  # Sort by absolute correlation

# plotting the correlation values
ggplot(cor_target_df, aes(x = reorder(Variable, -abs_corr), y = abs_corr)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Correlation with First-Time Visit (2023)",
       x = "Variable", y = "Absolute Correlation") +
  theme_minimal()
