#' Entry Point for Running the Project Pipeline
#'
#' Accepts command line arguments to run individual parts of the pipeline:
#'   - "filter" to run the initial keyword-based filtering
#'   - "translate" to test translation of a sample row
#'   - "topic" to run the (currently stubbed) topic modeling step
#'
#' Author: Refactored orchestration logic for CLI use

args <- commandArgs(trailingOnly = TRUE)

# Run filtering
if ("filter" %in% args) {
  source("src/filter_speeches.R")
  filter_speeches(
    input_path = "data/raw/Corp_HouseOfCommons_V2.rds",
    output_path = "data/processed/filtered_UK.rds"
  )
}

# Run translation sample
if ("translate" %in% args) {
  source("src/translate.R")
  translate_first_line(
    input_path = "data/raw/Corp_Folketing_V2.rds",
    auth_path = "greenpolicyresponse-5877a60aeaf4.json"
  )
}

# Run topic modeling (stub)
if ("topic" %in% args) {
  source("src/topic_modelling.R")
  run_topic_modelling(
    filtered_path = "data/processed/filtered_UK.rds"
  )
}
