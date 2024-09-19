library(xgboost)
library(caret)
# notes for myself
# preparing data for xgboost
prepare_data_for_xgboost <- function(data, target_column) {
  # one part goes to features, another one goes to target
  features <- data[, !colnames(data) %in% target_column, drop = FALSE]
  target <- data[[target_column]]
  
  feature_matrix <- as.matrix(features)
  
  # dmatrix = the format for xgboost
  dtrain <- xgb.DMatrix(data = feature_matrix, label = target)
  
  return(list(dtrain = dtrain, feature_names = colnames(features)))
}

# main function for xgboost model training
train_xgboost_model <- function(data, target_column, params = list(), nrounds = 100) {

  prepared_data <- prepare_data_for_xgboost(data, target_column)
  dtrain <- prepared_data$dtrain
  feature_names <- prepared_data$feature_names
  
  # default parameters here
  default_params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  # merge default and provided parameters
  final_params <- modifyList(default_params, params)
  
  # train the model
  model <- xgb.train(
    params = final_params,
    data = dtrain,
    nrounds = nrounds,
    watchlist = list(train = dtrain),
    early_stopping_rounds = 10,
    verbose = 1
  )
  
  model$feature_names <- feature_names
  
  return(model)
}

