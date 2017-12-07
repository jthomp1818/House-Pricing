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
  TotCoolSF, TotUnSF, YearRemodAdd, SaleType, SaleCondition, OverallQual, OverallCond, BedroomAbvGr, TotBathrooms, KitchenQual, 
  GarageCars, PoolQC, MiscVal, LotArea, Neighborhood
  )] 
# List all variables that you would like cross validation to start with
test.variable <- train$LogSalePrice  # List variable that you would like the model to train for

# Designate Objects to be used
set.seed(123)
variables <- copy(variables.start)
mtry0.error <- data.table(mtry0 = c(2, 4, 6, 8))
sampsize.error <- data.table(SampSize = round(seq(from = 0.1, to = 0.7, by = 0.3)*nrow(train), 0))
nodesize.error<- data.table(NodeSize = c(2, 3, 4, 8, 16))
variable.array <- data.table(rep(1, times = ncol(variables)), diag(x = -1, ncol = ncol(variables), nrow = ncol(variables)) + 
                               matrix(1, nrow = ncol(variables), ncol = ncol(variables))) * 1:ncol(variables)
output.list <- list()

#--------------------------------------------------
# Optimize Model to remove unproductive variables
# Add hyperparameter optimization for sampsize, replace?, and splitrule

# Remove Unnecessary Variables if applicable
for (num.variables.index in 1:ncol(variables.start)) {
  
  variable.analysis.matrix <- data.table(WhichVariableRemoved = 0:(ncol(variables)))
  
  # Determine Usefulness of Variables
  for (variable.index in 1:(ncol(variables) + 1)) {
    
    variables.used <- variable.array[, variable.index, with = FALSE]
    
    # Optimize Model for the mtry0 Hyperparameter
    for (mtry0.index in mtry0.error$mtry0) {
      
      # Optimize Model for the sampsize Hyperparameter
      for (sampsize.index in sampsize.error$SampSize) {
        
        # Optimize Model for the nodesize Hyperparameter
        for (nodesize.index in nodesize.error$NodeSize) {
          
          rf.model <- randomForest(
            x = variables[, variables.used[[1]], with = FALSE],
            y = test.variable,
            data = train,
            na.action = na.pass,
            mtry0 = mtry0.index,
            ntree = num.trees,
            sampsize = sampsize.index,
            nodesize = nodesize.index,
            type = regression
          )
          
          nodesize.error[NodeSize == nodesize.index, OOBError := (last(rf.model$mse))][]
        }
        
        optimum.nodesize <- nodesize.error[which.min(OOBError), list(NodeSize,OOBError)]
        print(nodesize.error)
        print(optimum.nodesize)
        nodesize.error <- data.table(NodeSize = c(2, 3, 4, 8, 16))#*** This line is used to debug program by resetting the table
        sampsize.error[SampSize == sampsize.index, `:=`(
          NodeSize = optimum.nodesize$NodeSize, 
          OOBError = optimum.nodesize$OOBError
        )][]
      }
      
      optimum.sampsize <- sampsize.error[which.min(OOBError), list(SampSize, NodeSize, OOBError)]
      print(sampsize.error)
      print(optimum.sampsize)
      sampsize.error <- data.table(SampSize = round(seq(from = 0.1, to = 0.7, by = 0.3)*nrow(train), 0))#*** This line is used to 
                                                                                            #debug program by resetting the table
      mtry0.error[mtry0 == mtry0.index, `:=`(
        SampSize = optimum.sampsize$SampSize, 
        NodeSize = optimum.sampsize$NodeSize, 
        OOBError = optimum.sampsize$OOBError
      )][]
    }
    
    optimum.mtry0 <- mtry0.error[which.min(OOBError), list(mtry0,SampSize,NodeSize,OOBError)]
    print(mtry0.error)
    print(optimum.mtry0)
    mtry0.error <- data.table(mtry0 = c(2, 4, 6, 8))#*** This line is used to debug program by resetting the table
    variable.analysis.matrix[WhichVariableRemoved == variable.index - 1,  `:=`(
      mtry0 = optimum.mtry0$mtry0, 
      SampSize = optimum.mtry0$SampSize, 
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
       SampSize = SampSize, 
       NodeSize = NodeSize, 
       OOBError = OOBError
  )]

rf1.model <- randomForest(
  optimum.variables,
  test.variable,
  data = train,
  na.action = rm(),
  mtry0 = optimum.params$mtry0,
  sampsize = optimum.params$SampSize,
  nodesize = optimum.params$NodeSize,
  ntree = 500
)

#--------------------------------------------------
# Model Error
par(mar = rep(5,4))
plot(rf1.model, ylim = c(0,0.05))

#--------------------------------------------------
# Model Variable Importance
# Get Importance
importance <- importance(rf1.model)
var.importance <- data.table(Variables = row.names(importance), Importance = round(importance[ ,'IncNodePurity'] ,2))

# Create a rank variable based on importance
rank.importance <- var.importance
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))

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
rf1.train.solution <- data.table(
  Id = train$Id, 
  SalePrice = exp(predict(
    rf1.model
    #newdata = test[, list(Pclass, Sex, Age, Fare, Embarked, Title, SibSp, Parch, MotherFather)]
  )
))

# Calc Error
calc.error <- rmsle(rf1.train.solution$SalePrice, train$SalePrice)

#======================================================================================================
# Make Prediction on test data to submit prediction to Kaggle
rf1.solution <- data.table(
  Id = test$Id, 
  SalePrice = exp(predict(
    rf1.model,
    newdata = test[, list(
      OverallQual, TotCoolSF, Neighborhood, TotBathrooms, YearRemodAdd, LotArea, TotUnSF, OverallCond, SaleCondition, MiscVal
      )]
  )
  ))

# Write Solution to file
write.csv(rf1.solution, "Submissions/rf1.solution.csv", row.names = FALSE)
























