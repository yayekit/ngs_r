library(plotly)
library(DT)

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

update_model_training_ui <- function(input, output, values) {
  output$train_summary <- renderPrint({
    req(values$model)
    print(values$model)
  })
}

update_model_evaluation_ui <- function(input, output, values) {
  output$eval_summary <- renderPrint({
    req(values$evaluation)
    print(values$evaluation$confusion_matrix)
    cat("\nAUC:", values$evaluation$auc)
    cat("\nAccuracy:", values$evaluation$accuracy)
    cat("\nPrecision:", values$evaluation$precision)
    cat("\nRecall:", values$evaluation$recall)
    cat("\nF1 Score:", values$evaluation$f1_score)
  })
  
  output$roc_plot <- renderPlotly({
    req(values$evaluation)
    plot_data <- data.frame(
      FPR = values$evaluation$roc_curve$specificities,
      TPR = values$evaluation$roc_curve$sensitivities
    )
    p <- ggplot(plot_data, aes(x = FPR, y = TPR)) +
      geom_line() +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
      labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$feature_importance_plot <- renderPlotly({
    req(values$model)
    importance_matrix <- xgb.importance(model = values$model)
    p <- xgb.plot.importance(importance_matrix[1:min(nrow(importance_matrix), 20),])
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)