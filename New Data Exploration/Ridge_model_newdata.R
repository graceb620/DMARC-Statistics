library(tidyverse)
library(pROC)
library(glmnet)
library(lubridate)
library(randomForest)
library(ggplot2)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(logistf)

### --- DATA PREP ---------------------------------------------------------------
# Create the dataset
hh_data2 = read.csv("Data/hh_data2.csv")

hh_data_models <- hh_data2 %>% 
  select(c(first_visit_2024, householdMembers, IncomeSource, fedPovertyLevel,
           IncomeSource, primary_visit_location, primary_service, foodstamps,
           dietary_issue, veteran))
# Remove NA values from the visit_count_2024 column, just in case
hh_data_models <- hh_data_models %>% na.omit(visit_count_2024)

# Splitting data into train and testing data sets
RNGkind(sample.kind="default")
set.seed(12345)
train.idx <- sample(x=1:nrow(hh_data_models), size=.7*nrow(hh_data_models))
train.df <- hh_data_models[train.idx,]
test.df <- hh_data_models[-train.idx,]

# Making test/train matrices
x.train <- model.matrix(first_visit_2024 ~ householdMembers+IncomeSource+
                          fedPovertyLevel+IncomeSource+primary_visit_location+
                          primary_service+foodstamps+dietary_issue+veteran, 
                        data=train.df)[,-1]
x.test <- model.matrix(first_visit_2024 ~ householdMembers+IncomeSource+
                          fedPovertyLevel+IncomeSource+primary_visit_location+
                          primary_service+foodstamps+dietary_issue+veteran, 
                        data=test.df)[,-1]

# Create vectors for y variable 
y.train <- as.vector(train.df$first_visit_2024)
y.test <- as.vector(test.df$first_visit_2024)

### --- lasso model ------------------------------------------------------------
## Fitting the model ----

## 1. Use cross validation to fit (LOTS OF) lasso regressions
lr_lasso_cv <- cv.glmnet(x.train, 
                         y.train, 
                         weights = train.df$weight,
                         family=binomial(link="logit"), 
                         alpha=1)

# 2. Find the best lambda value
# - Plot the sample error for each lambda value
plot(lr_lasso_cv)
# - Pick out the best optimal lambda value
best_lasso_lambda <- lr_lasso_cv$lambda.min

# 3. Fit the final Model
lasso <- final_lasso <- glmnet(x.train,
                               y.train,
                               family = binomial(link = "logit"),
                               weights = train.df$weight,
                               alpha = 1,
                               lambda = best_lasso_lambda)

## --- Quantify Prediction Performance ----------------------------------------
test.df.preds <- test.df %>% 
  mutate(
    lasso_pred = predict(lasso, x.test, type="response")[,1],
  )

lasso_rocCurve <- roc(response = as.factor(test.df.preds$first_visit_2024),
                               predictor = test.df.preds$lasso_pred, 
                               levels = c("0", "1")) 



### --- Ridge Model ------------------------------------------------------------
## --- Fitting the model -----
# ---- Use cross validation to fit ridge regressions -----
lr_ridge_cv <- cv.glmnet(x.train, 
                         y.train, 
                         weights = train.df$weight,
                         family=binomial(link="logit"), 
                         alpha=0)

# ---- Finding the best lambda value -----
# - Plotting the sample error for each lambda value
plot(lr_ridge_cv)
# - Pick out the best optimal lambda value
best_ridge_lambda <- lr_ridge_cv$lambda.min

# ---- Fitting the final Model ----
ridge <- final_ridge <- glmnet(x.train,
                               y.train,
                               family = binomial(link = "logit"),
                               weights = train.df$weight,
                               alpha = 0,
                               lambda = best_ridge_lambda)

## --- Quantify Prediction Performance ----
test.df.preds <- test.df %>% 
  mutate(
    ridge_pred = predict(ridge, x.test, type="response")[,1],
  )

ridge_rocCurve <- roc(response = as.factor(test.df.preds$first_visit_2024),
                               predictor = test.df.preds$ridge_pred, 
                               levels = c("0", "1")) 

### --- Random Forest 
tempforest <- randomForest(as.factor(first_visit_2024) ~ householdMembers+IncomeSource+
                             fedPovertyLevel+IncomeSource+primary_visit_location+
                             primary_service+foodstamps+dietary_issue+veteran, 
                           data=train.df,
                           ntree=100, 
                           mtry=4,
                           weights=train.df$weight)
dim(train.df)
mtry <- seq(from=1, to=9, by=3)
keeps <- data.frame(m=rep(NA, length(mtry)),
                   OOB_err_rate = rep(NA, length(mtry)))

for(idx in 1:length(mtry)) {
  print(paste0("Trying m= ", mtry[idx]))
  
  tempforest <- randomForest(as.factor(first_visit_2024) ~ householdMembers+IncomeSource+
                               fedPovertyLevel+IncomeSource+primary_visit_location+
                               primary_service+foodstamps+dietary_issue+veteran, 
                             data=train.df,
                             ntree=1000, 
                             mtry=mtry[idx],
                             weights=train.df$weight)
  
  keeps[idx, "m"] <- mtry[idx]
  
  keeps[idx, "OOB_err_rate"] <- mean(predict(tempforest) != train.df$first_visit_2024)
}

ggplot(data = keeps) +
  geom_line(aes(x = m, y = OOB_err_rate))+
  theme_bw() + labs(x = "m (mtry value)", y = "OOB Error Rate(minimize)") +
  scale_x_continuous(breaks = c(1:19))

# 4 is the optimal number that minimized the OOB error rate
finalforest <- randomForest(as.factor(first_visit_2024) ~ householdMembers + IncomeSource +
                            fedPovertyLevel + primary_visit_location +
                            primary_service + foodstamps + dietary_issue + veteran, 
                            data = train.df,
                            ntree = 100, 
                            mtry = 4)

pi_hat <- predict(finalforest, test.df, type="prob")[,"1"]

forest_rocCurve <- roc(response=test.df$first_visit_2024,
                       predictor=pi_hat,
                       levels=c("0","1"))

### --- ROC Curves -------------------------------------------------------------
par(mfrow=c(2,2))
plot(ridge_rocCurve, main="ROC Curve for Ridge Model",print.thres = TRUE, print.auc = TRUE)
plot(lasso_rocCurve, main="ROC curve for Lasso Model", print.thres = TRUE, print.auc = TRUE)
plot(forest_rocCurve, main="ROC Curve for Random Forest Model", print.thres=TRUE, print.auc=TRUE)
### --- Making a variable Importance Plot -------------------------------------
par(mfrow=c(1,1))
varImpPlot(finalforest, type=2, main="Variable Importance Plot for 
           if a person is a first time visitor in 2024")







