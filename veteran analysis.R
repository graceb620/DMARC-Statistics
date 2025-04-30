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

rm(list=ls())

hh24<-read.csv('Data/amelia_hh2_2024.csv', stringsAsFactors=FALSE)
monthly_count2<-read.csv('Data/monthly_count2.csv', stringsAsFactors=FALSE)
quarter_count2 <- read.csv('Data/quarter_count2.csv', stringsAsFactors=FALSE)

# Random Forest Model --------------------
# Our y variable is "anyveteran": whether a household has any veteran members
# Random Forest will allow us to create a "variable importance plot" that allows 
# us see what variables help best to classify veteran households. 
# This way, DMARC can understand this group better and give them better resources

### Prepare data for fitting predictive models 
#make veteran a factor
#remove ID and specific dates
hh24forest<-hh24%>%select(-houseHoldIdAfn,first_visit,last_visit)
hh24forest$anyveteran<-as.factor(hh24forest$anyveteran)

RNGkind(sample.kind="default")
set.seed(4561)
train.idx<-sample(x=1:nrow(hh24forest),size=.7*nrow(hh24forest))

#make training data set
train.df<-hh24forest[train.idx,]
test.df<-hh24forest[-train.idx,]

### Creating & tuning the forest --------
set.seed(112233)
myforest<-randomForest(anyveteran~.,
                       data=train.df,
                       ntree=5000,
                       mtry=4, #sqrt(18)=4.2, round down to 4
                       importance=TRUE,
                       proximity = TRUE)
myforest

##Let's check what the OOB rate is for mtry=[1,18]

#creating a dataframe that will keep all 18 values for our loop
keeps2 <- data.frame(m=rep(NA,length(18)),
                     OOB_err_rate=rep(NA,length(18)))
#
for (idx in 1:18){
  print(paste0("Trying to m=", idx))
  
  tempforest<- randomForest(anyveteran ~., 
                            data=train.df, 
                            ntree=5000,
                            mtry=idx,
                            importance=TRUE,
                            proximity = TRUE)
  #collecting the data in idx'th row
  keeps2[idx,"m"]<-idx
  #record the corresponding oob rate
  keeps2[idx,"OOB_err_rate"]<-mean(predict(tempforest)!=train.df$anyveteran)
  
}
keeps2

ggplot(data=keeps2)+
  geom_line(aes(x=m,y=OOB_err_rate))+
  theme_bw()+labs(x="m (mtry) value",y="OOB error rate")
# mtry=4 has the lowest OOB error rate

# Our final forest
set.seed(112233)
finalforest<-randomForest(anyveteran~.,
                          data=train.df,
                          ntree=5000,
                          mtry=4, #sqrt(18)=4.2, round down to 4
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
#We'll predict that a visitor is a veteran household 62.6% of the time when it is actually veteran
#We'll predict that a visitor is not a veteran household 71% of the time when it actually is 


# Variable Importance Plot ---------
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


# Veteran only visualizations -----------------
# Creating veteran only data set for visualizations
hhvet<-hh24%>%filter(anyveteran>0)