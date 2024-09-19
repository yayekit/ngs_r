library(xgboost)
library(pROC)
library(caret)

# Function to make predictions
make_predictions <- function(model, data, target_column) {
  # Prepare data
  features <- data[, !colnames(data) %in% target_column, drop = FALSE]
  feature_matrix <- as.matrix(features)
  
  # Make predictions
  predictions <- predict(model, feature_matrix)
  
  return(predictions)
}

# Function to evaluate model performance
evaluate_model <- function(model, data, target_column) {
  # Make predictions
  predictions <- make_predictions(model, data, target_column)
  
  # Get true labels
  true_labels <- data[[target_column]]
  
  # Calculate AUC
  roc_obj <- roc(true_labels, predictions)
  auc_value <- auc(roc_obj)
  
  # Calculate confusion matrix
  conf_matrix <- confusionMatrix(factor(ifelse(predictions > 0.5, 1, 0)), 
                                 factor(true_labels))
  
  # Calculate additional metrics
  accuracy <- conf_matrix$overall["Accuracy"]
  precision <- conf_matrix$byClass["Precision"]
  recall <- conf_matrix$byClass["Recall"]
  f1_score <- conf_matrix$byClass["F1"]
  
  # Return evaluation results
  return(list(
    auc = auc_value,
    roc_curve = roc_obj,
    confusion_matrix = conf_matrix,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1_score = f1_score
  ))
}

# Function to plot ROC curve
plot_roc_curve <- function(roc_obj) {
  plot(roc_obj, main = "ROC Curve")
  abline(a = 0, b = 1, lty = 2, col = "gray")
}

# Function to plot feature importance
plot_feature_importance <- function(model, top_n = 20) {
  importance_matrix <- xgb.importance(model = model)
  xgb.plot.importance(importance_matrix[1:min(nrow(importance_matrix), top_n),])
}

# Example usage
if (interactive()) {
  # Load model and test data
  model <- xgb.load("results/xgboost_model.model")
  test_data <- read.csv("data/processed/test_data.csv")
  
  # Assume 'target' is the column name for our prediction target
  target_column <- "target"
  
  # Evaluate model
  eval_results <- evaluate_model(model, test_data, target_column)
  
  # Print evaluation results
  print(eval_results$confusion_matrix)
  cat("\nAUC:", eval_results$auc)
  cat("\nAccuracy:", eval_results$accuracy)
  cat("\nPrecision:", eval_results$precision)
  cat("\nRecall:", eval_results$recall)
  cat("\nF1 Score:", eval_results$f1_score)
  
  # Plot ROC curve
  plot_roc_curve(eval_results$roc_curve)
  
  # Plot feature importance
  plot_feature_importance(model)
}