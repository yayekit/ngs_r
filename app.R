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

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "NGS Data Analysis"),
  dashboardSidebar(),
  dashboardBody()
)

