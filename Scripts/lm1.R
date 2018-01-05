# Linear Model #1
# This R script loosely follows the "Fun with Real Estate Data" Kernel from Kaggle, by Stehpanie Kirmer

##################################################################################################################################
# Libraries

library(data.table)

##################################################################################################################################
# Read clean train and test datasets

train <- readRDS("~/Documents/Rstuff/House.Pricing/Data/wrangled/clean.train.rds")
test <- readRDS("Data/wrangled/clean.test.rds")

##################################################################################################################################
# Create Functions for Calculating RMSE and RMSLE

rmse <- function(preds, actuals){
  score <- sqrt(mean((actuals - preds)^2))
  return(score)
}

rmsle <- function(preds, actuals){
  rmse(log(preds), log(actuals))
}

##################################################################################################################################
# Linear Model

# Remove LogSalePrice Column so that it is not used to predict Sale Price (would break model)
train$LogSalePrice <- NULL

lm.model.all <- lm(SalePrice ~ ., data = train)
summary(lm.model.all)

#--------------------------------------------------
# Remove variables that weren't found to be useful
lm.model.all <- lm(
  SalePrice ~ MSZoning + LotArea + Street + LandContour + Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + 
    Condition2 + HouseStyle + OverallQual + YearBuilt + RoofStyle + RoofMatl + MasVnrArea + Foundation + BsmtExposure + 
    BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF,
  data = train
  )

lm.model.all.predict <- predict(lm.model.all, train, type = "response")
model.output <- cbind(train, Pred = lm.model.all.predict)

rmsle(preds = model.output$Pred, actuals = train$SalePrice)   # Get 0.16 rmsle

#======================================================================================================
# Overwrite training prediction with test prediction
lm.model.all.predict <- predict(lm.model.all, test, type = "response")
model.output <- cbind(test, Pred = lm.model.all.predict)

model.output[Pred < 0]

#======================================================================================================
# Write Prediction to submittable file
lm1.solution <- data.table(Id = test$Id, SalePrice = model.output$Pred)

# Write Solution to file
write.csv(lm1.solution, "Submissions/lm1.solution.csv", row.names = FALSE)





