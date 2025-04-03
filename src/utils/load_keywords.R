#' Load Keywords from CSV
#' 
#' Reads a CSV file of keywords into a character vector.
#' The CSV is expected to have a single column header named `keyword`.
#' Used by filtering pipeline to detect environmental content.
#' 
#' @param path File path to the keyword CSV.
#' @return Character vector of keywords.
#'
#' Author: Refactored for modular use in structured project layout

library(data.table)

load_keywords <- function(path = "data/keywords.csv") {
  dt <- data.table::fread(path, header = TRUE, fill = TRUE)
  keywords <- dt[[1]]
  return(keywords)
}
