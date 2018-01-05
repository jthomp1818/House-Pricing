# Linear Model #1
# This R script loosely follows the "Fun with Real Estate Data" Kernel from Kaggle, by Stehpanie Kirmer

##################################################################################################################################
# Libraries

library(data.table)
library(glmnet)
library(mltools)

source("scripts/helpers.R")

#======================================================================================================
# Settings

options(scipen = 20)

##################################################################################################################################
# Read clean train and test datasets

train <- readRDS("~/Documents/Rstuff/House.Pricing/Data/wrangled/clean.train.rds")
test <- readRDS("Data/wrangled/clean.test.rds")

##################################################################################################################################
# Create fold column 
train[, FoldId := folds(x = TotCoolSF, nfolds = 5, seed = 2018)]

# Features to be Used
feats <- train[, list(
  TotCoolSF, TotUnSF, YrSold, SaleType, SaleCondition, OverallQual, OverallCond, BedroomAbvGr, TotBathrooms, KitchenQual, 
  GarageCars, LotArea, Neighborhood, LogSalePrice, FoldId
  )]

# Check for completeness
feats[!complete.cases(feats)]

num.feats <- cbind(
  feats[, colnames(feats)[sapply(feats, is.ordered)], with = F], 
  feats[, colnames(feats)[sapply(feats, is.numeric)], with = F]
  )

cat.feats <- feats[, !colnames(num.feats), with = F]

# One Hot Encoding for cat.feats
hot.feats <- one_hot(feats, cols = c("SaleType", "SaleCondition", "OverallQual", "OverallCond", "KitchenQual", "Neighborhood"))

# Prepare Data - Remove dummy variables (will remove empty variables if applicable)
prep.feats <- hot.feats[, !c(
  "SaleType_VWD", 
  "SaleCondition_Normal", 
  "OverallQual_10", 
  "OverallCond_10", 
  "KitchenQual_Po", 
  "Neighborhood_Names"
  ), with = FALSE]

##################################################################################################################################
# Create linear model based on prepared data
# Cross Validation

lmCV <- list()
lmCV[["Params"]] <- CJ(alpha = seq(0, 1, 0.2), lambda = 10^seq(-4, -1, by = 1))

# p = 16; f = 1
set.seed(2018)
for (p in 1:nrow(lmCV$Params)) {
  params <- if("Score" %in% colnames(lmCV$Params)) lmCV$Params[p, !"Score", with = F] else lmCV$Params[p]
  print(paste0("Testing hyper parameters: ", paste(colnames(params), params, collapse = " | ")))
  
  # make an empty vector to store the scores
  scores <- numeric(0)
  
  for (f in 1:5) {
    print(paste0("fold: ", f))
    
    # get cvtrain/cvtest
    cvtrain <- prep.feats[!J(FoldId = f), on = "FoldId"]
    cvtest <- prep.feats[J(FoldId = f), on = "FoldId"]
    
    # Fit glmnet model
    model.glmnet <- glmnet(
      x = as.matrix(cvtrain[, !c("FoldId", "LogSalePrice")]),
      y = cvtrain$LogSalePrice,
      alpha = params$alpha,
      lambda = params$lambda
    )
    
    # Make Predictions on cvtest
    preds <- predict(model.glmnet, as.matrix(cvtest[, !c("FoldId", "LogSalePrice")]), s = params$lambda)[, 1]
    cvtest[, PredSalePrice := exp(preds)]
    
    # Calculate Score and append Scores
    score <- rmsle(preds = cvtest$PredSalePrice, actuals = exp(cvtest$LogSalePrice))
    scores <- c(scores, score)
  }
  
  # Average and save the scores
  score <- mean(scores)
  lmCV[["Params"]][p, Score := score][]
  
  # Print the score
  print(paste0("CV Score: ", score))
  print("-----------------------------------------------------------------")
}

lmCV[["Params"]][order(Score)]

# Record best score & hyperparameters associated
best.params <- lmCV[["Params"]][order(Score)][1,]
#    alpha lambda     Score
# 1:   0.8  0.001 0.1475681

