library(tidyverse)
library(pROC)
library(glmnet)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(logistf)

source("New Data Exploration/DataSets2024.R")
### --- DATA PREP ---------------------------------------------------------------
# Create the dataset
hh_2024_new = read.csv("Data/hh2_2024") # Will be making model off of only households that visited in 2024

hh_data_models <- hh_2024_new %>% 
  select(c(visit_count_2024, householdMembers, IncomeSource, fedPovertyLevel,
           annualIncome, foodstamps, primary_visit_location, primary_service,
           primary_visitor_occupation, dietary_issue, veteran))
# Remove NA values from the visit_count_2024 column, just in case
hh_data_models <- hh_data_models %>% na.omit(visit_count_2024)

# Splitting data into train and testing data sets
RNGkind(sample.kind="default")
set.seed(12345)
train.idx <- sample(x=1:nrow(hh_data_models), size=.7*nrow(hh_data_models))
train.df <- hh_data_models[train.idx,]
test.df <- hh_data_models[-train.idx,]

# Making test/train matrices
x.train <- model.matrix(visit_count_2024~householdMembers+IncomeSource+fedPovertyLevel+
                        annualIncome+foodstamps+primary_visit_location+primary_service+
                        primary_visitor_occupation+dietary_issue+oldest_member+
                        youngest_member+veteran)





















