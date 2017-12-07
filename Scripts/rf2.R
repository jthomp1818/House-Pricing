# Random Forest Model #1
# Simple Random Forest model using variables expected to be the most influential on the SalePrice of a dwelling

##################################################################################################################################
# Libraries

library(data.table)
library(randomForest)

##################################################################################################################################
# Read clean train and test datasets

train <- readRDS("~/Documents/Rstuff/House.Pricing/Data/wrangled/clean.train.rds")
test <- readRDS("Data/wrangled/clean.test.rds")

##################################################################################################################################
# Random Forest Model utilizing Cross Validation

#--------------------------------------------------
# Build the model

# Optimization Inputs Required
num.trees <- 100  # Number of trees to use to optimize parameters. 20 - 30 recommended
variables.start <- train[, list(
  LotArea, Street, Alley, LotShape, LandContour, Utilities, LotConfig, LandSlope, Neighborhood,#MSSubClass, MSZoning, LotFrontage, 
  Condition1, Condition2, BldgType, HouseStyle, OverallQual, OverallCond, YearBuilt, YearRemodAdd, RoofStyle, RoofMatl,
  MasVnrType, MasVnrArea, ExterQual, ExterCond, BsmtCond, BsmtExposure, # BsmtQual, Exterior2nd, Foundation, Exterior1st, 
  BsmtFinType1, BsmtFinType2, BsmtUnfSF, TotalBsmtSF, CentralAir, Electrical, # , BsmtFinSF2, Heating, BsmtFinSF1, HeatingQC
  FirstFlrSF, LowQualFinSF, GrLivArea, BsmtFullBath, BsmtHalfBath, HalfBath, BedroomAbvGr, # SecondFlrSF, KitchenAbvGr, FullBath
  KitchenQual, TotRmsAbvGrd, Functional, FireplaceQu, GarageFinish, GarageCars, # GarageArea, Fireplaces, GarageType, GarageYrBlt
  GarageCond, PavedDrive, ScreenPorch, PoolArea, PoolQC,#OpenPorchSF, EnclosedPorch, ThreeSsnPorch, GarageQual, WoodDeckSF, Fence
  MiscVal, YrSold, SaleCondition, TotCoolSF, TotUnSF, TotBathrooms # MoSold, MiscFeature, SaleType, 
  )] 
# List all variables that you would like cross validation to start with
test.variable <- train$LogSalePrice  # List variable that you would like the model to train for

# Designate Objects to be used
set.seed(123)
variables <- copy(variables.start)
mtry0.error <- data.table(mtry0 = c(2, 4, 6, 8))
nodesize.error<- data.table(NodeSize = c(2, 3, 4, 8, 16))
variable.array <- data.table(rep(1, times = ncol(variables)), diag(x = -1, ncol = ncol(variables), nrow = ncol(variables)) + 
                               matrix(1, nrow = ncol(variables), ncol = ncol(variables))) * 1:ncol(variables)
output.list <- list()

#--------------------------------------------------
# Optimize Model to remove unproductive variables
# Add hyperparameter optimization

# Remove Unnecessary Variables if applicable
for (num.variables.index in 1:ncol(variables.start)) {
  
  variable.analysis.matrix <- data.table(WhichVariableRemoved = 0:(ncol(variables)))
  
  # Determine Usefulness of Variables
  for (variable.index in 1:(ncol(variables) + 1)) {
    
    variables.used <- variable.array[, variable.index, with = FALSE]
    
    # Optimize Model for the mtry0 Hyperparameter
    for (mtry0.index in mtry0.error$mtry0) {
      
      # Optimize Model for the nodesize Hyperparameter
      for (nodesize.index in nodesize.error$NodeSize) {
        
        rf.model <- randomForest(
          x = variables[, variables.used[[1]], with = FALSE],
          y = test.variable,
          data = train,
          na.action = na.pass,
          mtry0 = mtry0.index,
          ntree = num.trees,
          nodesize = nodesize.index,
          type = regression
        )
        
        nodesize.error[NodeSize == nodesize.index, OOBError := (last(rf.model$mse))][]
      }
      
      optimum.nodesize <- nodesize.error[which.min(OOBError), list(NodeSize, OOBError)]
      print(nodesize.error)
      print(optimum.nodesize)
      nodesize.error <- data.table(NodeSize = c(2, 3, 4, 8, 16))#*** This line is used to debug program by resetting the table
      mtry0.error[mtry0 == mtry0.index, `:=`(
        NodeSize = optimum.nodesize$NodeSize, 
        OOBError = optimum.nodesize$OOBError
      )][]
    }
    
    optimum.mtry0 <- mtry0.error[which.min(OOBError), list(mtry0,NodeSize,OOBError)]
    print(mtry0.error)
    print(optimum.mtry0)
    mtry0.error <- data.table(mtry0 = c(2, 4, 6, 8))#*** This line is used to debug program by resetting the table
    variable.analysis.matrix[WhichVariableRemoved == variable.index - 1,  `:=`(
      mtry0 = optimum.mtry0$mtry0, 
      NodeSize = optimum.mtry0$NodeSize,
      OOBError = optimum.mtry0$OOBError
    )][]
    print(variable.analysis.matrix)
  }
  
  # Create list of tables collecting the variable.analysis.matrix tables & list which variables were analyzed 
  output.list[[num.variables.index]] <- list(
    colnames(variables[, variable.array[[1]], with = FALSE]), 
    variable.analysis.matrix
  )
  
  # Test to see if removing any of the variables improved the error of the randomForest model
  if (min(variable.analysis.matrix$OOBError) == variable.analysis.matrix$OOBError[1]) {
    # If the error was NOT improved by removing a variable then:
    # Record Optimum Variables 
    
    min.error.variables <- variable.array[, 1, with = FALSE]
    optimum.variables <- variables[, min.error.variables[[1]], with = FALSE]
    
    # Break out of while loop
    break
    
  } else {
    # If the error was improved by removing a variable, then:
    # Remove appropriate variable
    varToRemove <- which.min(variable.analysis.matrix$OOBError) - 1
    print(paste("Removing variable", colnames(variables)[varToRemove]))
    variables <- variables[, !varToRemove, with = FALSE]
    
    # Create new, smaller, matrix
    variable.array <- data.table(rep(1, times = ncol(variables)), diag(x = -1, ncol = ncol(variables), nrow = ncol(variables)) + 
                                   matrix(1, nrow = ncol(variables), ncol = ncol(variables))) * 1:ncol(variables)
  }
}

optimum.params <- variable.analysis.matrix[
  OOBError == min(OOBError), 
  list(mtry0 = mtry0, 
       NodeSize = NodeSize, 
       OOBError = OOBError
  )]

rf2.model <- randomForest(
  optimum.variables,
  test.variable,
  data = train,
  na.action = rm(),
  mtry0 = optimum.params$mtry0,
  nodesize = optimum.params$NodeSize,
  ntree = 500
)

#--------------------------------------------------
# Model Error
par(mar = rep(5,4))
plot(rf2.model, ylim = c(0,0.05))

#--------------------------------------------------
# Model Variable Importance
# Get Importance
importance <- importance(rf2.model)
var.importance <- data.table(Variables = row.names(importance), Importance = round(importance[ ,'IncNodePurity'] ,2))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rank.importance, aes(x=reorder(Variables, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Variables') +
  coord_flip() +
  theme_few()

##################################################################################################################################
# Predictions

#======================================================================================================
# Create Functions for Calculating RMSE and RMSLE

rmse <- function(preds, actuals){
  score <- sqrt(mean((actuals - preds)^2))
  return(score)
}

rmsle <- function(preds, actuals){
  rmse(log(preds), log(actuals))
}

#======================================================================================================
# Make Prediction on train data to predict error
rf2.train.solution <- data.table(
  Id = train$Id, 
  SalePrice = exp(predict(
    rf2.model
    #newdata = test[, list(Pclass, Sex, Age, Fare, Embarked, Title, SibSp, Parch, MotherFather)]
  )
))

# Calc Error
rmsle(rf2.train.solution$SalePrice, train$SalePrice)

#======================================================================================================
# Make Prediction on test data to submit prediction to Kaggle
rf2.solution <- data.table(
  Id = test$Id, 
  SalePrice = exp(predict(
    rf2.model,
    newdata = test[, list(
      LotArea, Street, Alley, LotShape, LandContour, Utilities, LotConfig, LandSlope, Neighborhood,#MSSubClass, MSZoning, LotFrontage, 
      Condition1, Condition2, BldgType, HouseStyle, OverallQual, OverallCond, YearBuilt, YearRemodAdd, RoofStyle, RoofMatl,
      MasVnrType, MasVnrArea, ExterQual, ExterCond, BsmtCond, BsmtExposure, # BsmtQual, Exterior2nd, Foundation, Exterior1st, 
      BsmtFinType1, BsmtFinType2, BsmtUnfSF, TotalBsmtSF, CentralAir, Electrical, # , BsmtFinSF2, Heating, BsmtFinSF1, HeatingQC
      FirstFlrSF, LowQualFinSF, GrLivArea, BsmtFullBath, BsmtHalfBath, HalfBath, BedroomAbvGr, # SecondFlrSF, KitchenAbvGr, FullBath
      KitchenQual, TotRmsAbvGrd, Functional, FireplaceQu, GarageFinish, GarageCars, # GarageArea, Fireplaces, GarageType, GarageYrBlt
      GarageCond, PavedDrive, ScreenPorch, PoolArea, PoolQC,#OpenPorchSF, EnclosedPorch, ThreeSsnPorch, GarageQual, WoodDeckSF, Fence
      MiscVal, YrSold, SaleCondition, TotCoolSF, TotUnSF, TotBathrooms # MoSold, MiscFeature, SaleType, 
      )]
  )
  ))

# Write Solution to file
write.csv(rf2.solution, "Submissions/rf2.solution.csv", row.names = FALSE)
























