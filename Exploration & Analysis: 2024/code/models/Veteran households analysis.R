# Main contributor: Amelia Burnell

# Dataset used: household information for only 2024 visits 
# Our y variable is "anyveteran": whether a household has any veteran members

# This file creates a random forest, ridge, and lasso models 
# Goal: for DMARC can understand this group better and give them better resources

# Loading libraries and datasets ---------------

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(TeachingDemos)
library(tidyverse)
library(pROC)
library(randomForest)
library(rpart)
library(rpart.plot)
library(logistf)
library(glmnet)

rm(list=ls())

hh24<-read.csv('Data/hh2_only2024calc.csv', stringsAsFactors=FALSE)

# Random Forest Model --------------------
# Random Forest will allow us to create a "variable importance plot" that allows 
# us see what variables help best to classify veteran households. 

### Prepare data for fitting predictive models 
#make veteran a factor
#remove ID and specific dates
hh24forest<-hh24%>%select(-houseHoldIdAfn,-first_visit,-last_visit)
hh24forest$anyveteran<-as.factor(hh24forest$anyveteran)

RNGkind(sample.kind="default")
set.seed(4561)
train.idx<-sample(x=1:nrow(hh24forest),size=.7*nrow(hh24forest))

#make training data set
train.df<-hh24forest[train.idx,]
test.df<-hh24forest[-train.idx,]

### Creating & tuning the forest
##Let's check what the OOB rate is for mtry=[1,18]
# 
# #creating a dataframe that will keep all 18 values for our loop
# keeps2 <- data.frame(m=rep(NA,length(18)),
#                      OOB_err_rate=rep(NA,length(18)))
# #
# for (idx in 1:18){
#   print(paste0("Trying to m=", idx))
#   
#   tempforest<- randomForest(anyveteran ~., 
#                             data=train.df, 
#                             ntree=1000,
#                             mtry=idx,
#                             importance=TRUE,
#                             proximity = TRUE)
#   #collecting the data in idx'th row
#   keeps2[idx,"m"]<-idx
#   #record the corresponding oob rate
#   keeps2[idx,"OOB_err_rate"]<-mean(predict(tempforest)!=train.df$anyveteran)
#   
# }
# keeps2
# 
# ggplot(data=keeps2)+
#   geom_line(aes(x=m,y=OOB_err_rate))+
#   theme_bw()+labs(x="m (mtry) value",y="OOB error rate")
# # mtry=2 has the lowest OOB error rate

# Our final forest
set.seed(112233)
finalforest<-randomForest(anyveteran~.,
                          data=train.df,
                          ntree=1000,
                          mtry=2, #sqrt(18)=4.2, round down to 4
                          importance=TRUE)
finalforest

#Assuming positive event is anyveteran
pi_hat<-predict(finalforest,test.df,type="prob")[,"1"]
rocCurve<-roc(response=test.df$anyveteran,
              predictor=pi_hat,
              levels=c("0","1"))

plot(rocCurve,print.thres=TRUE,print.auc=TRUE)

#If we set pi* to 0.147 
#(we predict that a visitor is a veteran household if the probability of it being luxury is 0.14)
#Our specificity is 0.959
#Our sensitivity is 0.89
#We'll predict that a visitor is a veteran household 95.9% of the time when it actually is  
#We'll predict that a visitor is NOT a veteran household 89% of the time when it is NOT


# Variable Importance Plot
par(mfrow=c(1,1))
varImpPlot(finalforest, type=1)
vi <- as.data.frame(varImpPlot(finalforest, type=1))
vi$Variable <- rownames(vi)

ggplot(data = vi) +
  geom_bar(aes(x=reorder(Variable,MeanDecreaseAccuracy),
               weight=MeanDecreaseAccuracy), position="identity") +
  coord_flip() + 
  labs(x="Variable Name", y="Mean Decrease Accuracy") + 
  ggtitle("Variable Importance Plot for Veteran Housholds")

#Ridge/Lasso models -------------------
hh24<-read.csv('Data/amelia_hh2_2024.csv', stringsAsFactors=FALSE)

### Prepare data for fitting predictive models 
#make anyveteran a vector
#remove ID and specific dates
hh24rl<-hh24%>%select(-houseHoldIdAfn,-first_visit,-last_visit)
hh24rl$anyveteran<-as.vector(hh24rl$anyveteran)

RNGkind(sample.kind="default")
set.seed(4561)
# Create full design matrix FIRST, to ensure matching columns
X <- model.matrix(anyveteran ~ ., data = hh24rl)[, -1]  # Remove intercept column
y <- hh24rl$anyveteran

# Split train/test using indices
set.seed(4561)
train.idx <- sample(1:nrow(X), size = 0.7 * nrow(X))
x.train <- X[train.idx, ]
x.test <- X[-train.idx, ]
y.train <- y[train.idx]
y.test <- y[-train.idx]

lr_lasso_cv<-cv.glmnet(x.train,
                       y.train,
                       family=binomial(link="logit"),
                       alpha=1) #is for lasso

lr_ridge_cv<-cv.glmnet(x.train, #this is the x matrix
                       y.train, #this is the y vector
                       family=binomial(link="logit"),
                       alpha=0) #is for ridge

#(predictability) pick out the optimal lambda values
best_lasso_lambda <- lr_lasso_cv$lambda.min
best_ridge_lambda <- lr_lasso_cv$lambda.min

#final models
final_lasso <- glmnet(x.train,
                      y.train,
                      family=binomial(link="logit"),
                      alpha=1,
                      lambda=best_lasso_lambda) 
final_ridge <- glmnet(x.train,
                      y.train,
                      family=binomial(link="logit"),
                      alpha=0,
                      lambda=best_ridge_lambda)

lr_lasso_coefs <- coef(lr_lasso_cv,s="lambda.min") %>% as.matrix
lr_ridge_coefs <- coef(lr_ridge_cv,s="lambda.min") %>% as.matrix

test.df.preds<-test.df %>% 
  mutate(
    lasso_pred=predict(final_lasso, x.test,type="response")[,1], 
    ridge_pred=predict(final_ridge, x.test,type="response")[,1], 
  )

ridge_rocCurve <- roc(response=(test.df.preds$anyveteran),
                      predictor=test.df.preds$ridge_pred, 
                      levels=c("0","1"))
lasso_rocCurve<-roc(response=(test.df.preds$anyveteran),
                    predictor=test.df.preds$lasso_pred,
                    levels=c("0","1"))

plot(ridge_rocCurve, print.thres = TRUE, print.auc = TRUE) #0.753 AUC

plot(lasso_rocCurve, print.thres = TRUE, print.auc = TRUE) #0.754 AUC 

#Use lasso for interpretations, slightly higher, very small difference

# Only visitor household with veterans visualizations preperation -----------------
# Creating veteran only data set for visualizations
hhvet<-hh24%>%filter(anyveteran>0)