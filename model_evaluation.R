library(xgboost)
library(pROC)
library(caret)
library(ggplot2)

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

# Function to plot ROC curve using ggplot2
plot_roc_curve <- function(roc_obj) {
  roc_data <- data.frame(
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities
  )
  
  ggplot(roc_data, aes(x = FPR, y = TPR)) +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
    theme_minimal() +
    annotate("text", x = 0.75, y = 0.25, 
             label = paste("AUC =", round(auc(roc_obj), 3)))
}

# Function to plot feature importance using ggplot2
plot_feature_importance <- function(model, top_n = 20) {
  importance_matrix <- xgb.importance(model = model)
  top_features <- importance_matrix[seq_len(min(nrow(importance_matrix), top_n)),]
  
  ggplot(top_features, aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Feature Importance", x = "Features", y = "Gain") +
    theme_minimal()
}

# Function to save evaluation results and plots
save_evaluation_results <- function(eval_results, roc_plot, importance_plot, output_dir) {
  # Create output directory if it doesn't exist
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Save evaluation metrics
  write.csv(data.frame(
    Metric = c("AUC", "Accuracy", "Precision", "Recall", "F1 Score"),
    Value = c(eval_results$auc, eval_results$accuracy, eval_results$precision, 
              eval_results$recall, eval_results$f1_score)
  ), file = file.path(output_dir, "evaluation_metrics.csv"), row.names = FALSE)
  
  # Save confusion matrix
  write.csv(as.data.frame(eval_results$confusion_matrix$table), 
            file = file.path(output_dir, "confusion_matrix.csv"))
  
  # Save plots
  ggsave(file.path(output_dir, "roc_curve.png"), plot = roc_plot, width = 8, height = 6)
  ggsave(file.path(output_dir, "feature_importance.png"), plot = importance_plot, width = 10, height = 8)
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
  
  # Create plots
  roc_plot <- plot_roc_curve(eval_results$roc_curve)
  importance_plot <- plot_feature_importance(model)
  
  # Display plots
  print(roc_plot)
  print(importance_plot)
  
  # Save evaluation results and plots
  save_evaluation_results(eval_results, roc_plot, importance_plot, "results/model_evaluation")
}