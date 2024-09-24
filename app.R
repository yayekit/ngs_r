library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(xgboost)
library(Biostrings)
library(GenomicRanges)
library(pROC)
library(plotly)

# sourcing the scripts
source("src/data_preprocessing.R")
source("src/feature_engineering.R")
source("src/model_training.R")
source("src/model_evaluation.R")

# custom css for the app
custom_css <- "
  .content-wrapper, .right-side {
    background-color: #f4f6f9;
  }
  .box {
    box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
    transition: all 0.3s cubic-bezier(.25,.8,.25,1);
  }
  .box:hover {
    box-shadow: 0 14px 28px rgba(0,0,0,0.25), 0 10px 10px rgba(0,0,0,0.22);
  }
  .custom-file-input::-webkit-file-upload-button {
    visibility: hidden;
  }
  .custom-file-input::before {
    content: 'Select FASTQ file';
    display: inline-block;
    background: linear-gradient(top, #f9f9f9, #e3e3e3);
    border: 1px solid #999;
    border-radius: 3px;
    padding: 5px 8px;
    outline: none;
    white-space: nowrap;
    -webkit-user-select: none;
    cursor: pointer;
    text-shadow: 1px 1px #fff;
    font-weight: 700;
    font-size: 10pt;
  }
  .custom-file-input:hover::before {
    border-color: black;
  }
  .custom-file-input:active::before {
    background: -webkit-linear-gradient(top, #e3e3e3, #f9f9f9);
  }
"

ui <- dashboardPagePlus(
  header = dashboardHeaderPlus(
    title = "NGS Data Analysis"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Preprocessing", tabName = "preprocess", icon = icon("cogs")),
      menuItem("Feature Engineering", tabName = "features", icon = icon("chart-bar")),
      menuItem("Model Training", tabName = "train", icon = icon("robot")),
      menuItem("Model Evaluation", tabName = "evaluate", icon = icon("chart-line"))
    )
  ),
  body = dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    tabItems(
      # Data Upload tab
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = "Upload FASTQ File", status = "primary", solidHeader = TRUE,
                  fileInput("file", "Choose FASTQ File", accept = c(".fastq", ".fq"), 
                            class = "custom-file-input"),
                  actionBttn("upload_btn", "Upload and Process", 
                             style = "gradient", color = "primary")
                )
              )
      ),
      
      # Preprocessing tab
      tabItem(tabName = "preprocess",
              fluidRow(
                box(
                  title = "Data Preprocessing Summary", status = "info", solidHeader = TRUE,
                  verbatimTextOutput("preprocess_summary")
                ),
                box(
                  title = "Quality Plot", status = "info", solidHeader = TRUE,
                  plotlyOutput("quality_plot")
                )
              )
      ),
      
      # Feature Engineering tab
      tabItem(tabName = "features",
              fluidRow(
                box(
                  title = "Featured Data", status = "success", solidHeader = TRUE, width = 12,
                  DTOutput("feature_table")
                )
              ),
              fluidRow(
                box(
                  downloadBttn("download_features", "Download Featured Data", 
                               style = "gradient", color = "success")
                )
              )
      ),
      
      # Model Training tab
      tabItem(tabName = "train",
              fluidRow(
                box(
                  title = "Model Training", status = "warning", solidHeader = TRUE,
                  sliderInput("train_ratio", "Training Data Ratio:", 
                              min = 0.5, max = 0.9, value = 0.8, step = 0.1),
                  actionBttn("train_btn", "Train Model", 
                             style = "gradient", color = "warning")
                ),
                box(
                  title = "Training Summary", status = "warning", solidHeader = TRUE,
                  verbatimTextOutput("train_summary")
                )
              )
      ),
      
      # Model Evaluation tab
      tabItem(tabName = "evaluate",
              fluidRow(
                box(
                  title = "Evaluation Metrics", status = "danger", solidHeader = TRUE,
                  verbatimTextOutput("eval_summary")
                ),
                box(
                  title = "ROC Curve", status = "danger", solidHeader = TRUE,
                  plotlyOutput("roc_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Feature Importance", status = "danger", solidHeader = TRUE, width = 12,
                  plotlyOutput("feature_importance_plot")
                )
              )
      )
    )
  )
)


server <- function(input, output, session) {
  values <- reactiveValues(
    raw_data = NULL,
    preprocessed_data = NULL,
    featured_data = NULL,
    model = NULL,
    evaluation = NULL
  )
  
  source("data_handlers.R")
  source("model_handlers.R")
  source("ui_handlers.R")
  
  observeEvent(input$upload_btn, {
    values$raw_data <- load_and_preprocess_data(input$file$datapath)
  })
  
  observe({
    req(values$preprocessed_data)
    values$featured_data <- engineer_features(values$preprocessed_data)
  })
  
  observeEvent(input$train_btn, {
    req(values$featured_data)
    values$model <- train_model(values$featured_data, input$train_ratio)
  })
  
  observe({
    req(values$model, values$featured_data)
    values$evaluation <- evaluate_model(values$model, values$featured_data, input$train_ratio)
  })
  
  observe({
    update_preprocessing_ui(input, output, values)
    update_feature_engineering_ui(input, output, values)
    update_model_training_ui(input, output, values)
    update_model_evaluation_ui(input, output, values)
  })
}