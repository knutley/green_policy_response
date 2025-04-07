#' Topic Modeling Pipeline
#'
#' Processes filtered speeches into topics via LDA:
#'   - Tokenization and cleaning (stopwords, numbers)
#'   - Document-Term Matrix (DTM) creation
#'   - LDA model fitting
#'   - Export of top terms per topic and topic assignments per document
#'
#' @param filtered_path Path to the filtered RDS file
#' @return NULL (results saved to CSV)
#'
#' Author: Aarushi Sharma, April 2025

run_topic_modelling <- function(filtered_path = "data/processed/filtered_UK.rds") {
  # Load required packages
  library(tidyverse)
  library(tidytext)
  library(stringr)
  library(topicmodels)
  library(reshape2)

  # Output paths
  lda_top_terms_csv <- "data/processed/lda_topics.csv"
  lda_doc_topics_csv <- "data/processed/lda_assignments.csv"

  message("[1/4] Loading filtered data...")
  if (!file.exists(filtered_path)) {
    stop("Filtered data not found at ", filtered_path, "\nRun: ./run.sh filter")
  }

  df <- readRDS(filtered_path)
  if (!"text" %in% names(df)) stop("Filtered data must contain a 'text' column.")
  if (nrow(df) == 0) stop("Filtered data is empty.")

  df <- df %>% mutate(doc_id = row_number())

  message("[2/4] Tokenizing and cleaning text...")
  tokenised <- df %>%
    select(doc_id, text) %>%
    mutate(text = str_to_lower(text)) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word") %>%
    filter(!str_detect(word, "^[0-9]+$"))

  message("[3/4] Building DTM and running LDA...")
  dtm_counts <- tokenised %>% count(doc_id, word, sort = TRUE)
  dtm <- cast_dtm(dtm_counts, document = doc_id, term = word, value = n)

  k <- 15  # Number of topics
  set.seed(42)
  lda_model <- LDA(dtm, k = k, control = list(seed = 42))

  message("   LDA model fit with ", k, " topics.")

  message("[4/4] Exporting results...")

  top_terms <- tidy(lda_model, matrix = "beta") %>%
    group_by(topic) %>%
    slice_max(beta, n = 10) %>%
    ungroup() %>%
    arrange(topic, -beta)
  write_csv(top_terms, lda_top_terms_csv)

  doc_topics <- tidy(lda_model, matrix = "gamma") %>%
    group_by(document) %>%
    slice_max(order_by = gamma, n = 1) %>%
    ungroup()
  write_csv(doc_topics, lda_doc_topics_csv)

  message("\n✅ Topic modeling complete.")
  message("Top terms saved to: ", lda_top_terms_csv)
  message("Topic assignments saved to: ", lda_doc_topics_csv)
}
