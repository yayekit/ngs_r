library(plotly)
library(DT)
library(ggplot2)
library(xgboost)

# Preprocessing UI update
update_preprocessing_ui <- function(input, output, values) {
  output$preprocess_summary <- renderPrint({
    req(values$preprocessed_data)
    summary(values$preprocessed_data)
  })
  
  output$quality_plot <- renderPlotly({
    req(values$raw_data)
    p <- quality_check(values$raw_data)
    ggplotly(p)
  })
}

# Feature engineering UI update
update_feature_engineering_ui <- function(input, output, values) {
  output$feature_table <- renderDT({
    req(values$featured_data)
    datatable(values$featured_data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$download_features <- downloadHandler(
    filename = function() {
      paste("featured_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$featured_data, file, row.names = FALSE)
    }
  )
}

# Model training UI update
update_model_training_ui <- function(input, output, values) {
  output$train_summary <- renderPrint({
    req(values$model)
    print(values$model)
  })
}

# Model evaluation UI update
update_model_evaluation_ui <- function(input, output, values) {
  # Evaluation summary
  output$eval_summary <- renderPrint({
    req(values$evaluation)
    print_evaluation_summary(values$evaluation)
  })
  
  # ROC plot
  output$roc_plot <- renderPlotly({
    req(values$evaluation)
    plot_roc_curve(values$evaluation$roc_curve)
  })
  
  # Feature importance plot
  output$feature_importance_plot <- renderPlotly({
    req(values$model)
    plot_feature_importance(values$model)
  })
}

# Helper functions
print_evaluation_summary <- function(evaluation) {
  print(evaluation$confusion_matrix)
  cat("\nAUC:", evaluation$auc)
  cat("\nAccuracy:", evaluation$accuracy)
  cat("\nPrecision:", evaluation$precision)
  cat("\nRecall:", evaluation$recall)
  cat("\nF1 Score:", evaluation$f1_score)
}

plot_roc_curve <- function(roc_curve) {
  plot_data <- data.frame(
    FPR = roc_curve$specificities,
    TPR = roc_curve$sensitivities
  )
  p <- ggplot(plot_data, aes(x = FPR, y = TPR)) +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
    theme_minimal()
  ggplotly(p)
}

plot_feature_importance <- function(model) {
  importance_matrix <- xgb.importance(model = model)
  p <- xgb.plot.importance(importance_matrix[seq_len(min(nrow(importance_matrix), 20)),])
  ggplotly(p)
}

# Remove this line as it should be in a separate app.R file
# shinyApp(ui = ui, server = server)