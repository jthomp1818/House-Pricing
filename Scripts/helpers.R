# Create Functions for Calculating RMSE and RMSLE

rmse <- function(preds, actuals){
  score <- sqrt(mean((actuals - preds)^2))
  return(score)
}

rmsle <- function(preds, actuals){
  rmse(log(preds), log(actuals))
}
