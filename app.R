library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(xgboost)
library(Biostrings)
library(GenomicRanges)
library(pROC)

# Source our existing scripts
source("data_preprocessing.R")
source("feature_engineering.R")
source("model_training.R")
source("model_evaluation.R")

# ... [Previous UI code remains the same] ...

# Server
server <- function(input, output, session) {
  
  # Reactive values to store processed data and model
  values <- reactiveValues(
    raw_data = NULL,
    preprocessed_data = NULL,
    featured_data = NULL,
    model = NULL,
    evaluation = NULL
  )
  
  # ... [Previous server code remains the same up to Model Training] ...
  
  # Model Training
  observeEvent(input$train_btn, {
    req(values$featured_data)
    train_data <- values$featured_data[1:round(nrow(values$featured_data) * input$train_ratio), ]
    values$model <- train_xgboost_model(train_data, target_column = "target")
  })
  
  output$train_summary <- renderPrint({
    req(values$model)
    print(values$model)
  })
  
  # Model Evaluation
  observe({
    req(values$model, values$featured_data)
    test_data <- values$featured_data[(round(nrow(values$featured_data) * input$train_ratio) + 1):nrow(values$featured_data), ]
    values$evaluation <- evaluate_model(values$model, test_data, target_column = "target")
  })
  
  output$eval_summary <- renderPrint({
    req(values$evaluation)
    print(values$evaluation$confusion_matrix)
    cat("\nAUC:", values$evaluation$auc)
    cat("\nAccuracy:", values$evaluation$accuracy)
    cat("\nPrecision:", values$evaluation$precision)
    cat("\nRecall:", values$evaluation$recall)
    cat("\nF1 Score:", values$evaluation$f1_score)
  })
  
  output$roc_plot <- renderPlot({
    req(values$evaluation)
    plot_roc_curve(values$evaluation$roc_curve)
  })
  
  output$feature_importance_plot <- renderPlot({
    req(values$model)
    plot_feature_importance(values$model)
  })
}

# Run the app
shinyApp(ui = ui, server = server)