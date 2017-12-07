# Libraries Used
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggthemr)
library(randomForest)

#======================================================================================================
# Read clean train and test datasets

train <- readRDS("~/Documents/Rstuff/House.Pricing/Data/wrangled/clean.train.rds")
test <- readRDS("Data/wrangled/clean.test.rds")

#======================================================================================================
# Create Functions for Calculating RMSE and RMSLE

rmse <- function(preds, actuals){
  score <- sqrt(mean((actuals - preds)^2))
  return(score)
}

rmsle <- function(preds, actuals){
  rmse(log(preds), log(actuals))
}

##################################################################################################################################
# Predictions

#======================================================================================================
# 1.0 Calculate One Sale Price Value for all Houses that will minimize the Root Mean Square Logarithmic Error

# Calculate optimum Sale Price to miniize Error
train[, LogSalePrice := log(SalePrice)]
exp(mean(train$LogSalePrice))

# Test to see what the error is using created functions
rmsle(166716.6, train$SalePrice)
rmse(log(166716.6), log(train$SalePrice))

# Write a submission with the solution
test[, SalePrice := exp(mean(train$LogSalePrice))]
fwrite(test[, list(Id, SalePrice)], "Submissions/mean.csv")

#======================================================================================================
# 2.0 Predict Sale Prices based on SQ Footage Bins 

# Create SQ Footage Bins
train[, totalSF := FirstFlrSF + SecondFlrSF + TotalBsmtSF]
max(train[, list(totalSF = FirstFlrSF + SecondFlrSF + TotalBsmtSF)])
train[, SFGroup := round(totalSF/500, 0) * 500]

test[, totalSF := FirstFlrSF + SecondFlrSF + TotalBsmtSF]
test[, SFGroup := round(totalSF/500, 0) * 500]

# Create prediction
preds <- train[, list(Pred = exp(mean(LogSalePrice))), keyby = SFGroup]

# Map Back to train data
train[preds, Pred := i.Pred, on = "SFGroup"]

# Calculate In-House Error of prediction
rmsle(train$Pred, train$SalePrice)

# Make Prediction for Test Data & Write Submission
test[preds, Pred := i.Pred, on = "SFGroup"]
test[is.na(Pred), Pred := exp(mean(train$LogSalePrice))]

fwrite(test[, list(Id, SalePrice = Pred)], "Submissions/footagebucket.csv")

#======================================================================================================
# 3.0 


















