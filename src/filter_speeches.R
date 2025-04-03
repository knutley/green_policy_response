#' Filter Parliamentary Speeches for Environmental Content
#'
#' Filters an RDS file of parliamentary speeches by date and environmental keywords.
#' Saves the filtered speeches to a new RDS file.
#'
#' Adapted from: climatefiltering_sample.R by Elisa D'Amico, 19 MAR 2025

source("src/utils/load_keywords.R")
source("src/utils/download_if_missing.R")

library(data.table)
library(stringr)
library(progressr)

filter_speeches <- function(input_path = "data/raw/Corp_HouseOfCommons_V2.rds",
                            output_path = "data/processed/filtered_UK.rds",
                            keyword_path = "data/keywords.csv") {
  # Enable progressr globally
  options(progressr.enable = TRUE)
  
  message("[1/7] Loading keyword list from: ", keyword_path)
  keywords <- load_keywords(keyword_path)
  message("Loaded ", length(keywords), " keywords.")

  message("[2/7] Ensuring input file exists or downloading if needed...")
  input_path <- download_if_missing(
    filename = basename(input_path),
    dest_dir = dirname(input_path)
  )

  message("[3/7] Reading RDS file: ", input_path)
  t0 <- Sys.time()
  df <- readRDS(input_path)
  t1 <- Sys.time()
  message(sprintf("Finished reading RDS file in %.1f seconds.", as.numeric(difftime(t1, t0, units = "secs"))))

  message("[4/7] Filtering speeches by date (2000-2020)...")
  df$date <- as.Date(df$date)
  df <- df[df$date >= "2000-01-01" & df$date <= "2020-12-31", ]
  message("Filtered to ", nrow(df), " rows in date range.")

  message("[5/7] Setting up sequential processing...")
  
  # Set up progress handlers
  handlers(global = TRUE)
  handlers("txtprogressbar")

  message("[6/7] Compiling keyword regex...")
  # Create a simple pattern string
  pattern <- paste0(keywords, collapse = "|")
  
  message("[7/7] Checking for keyword matches with progress...")
  
  # Process in chunks to allow progress reporting
  chunk_size <- 50000
  total_rows <- nrow(df)
  n_chunks <- ceiling(total_rows / chunk_size)
  message("Processing ", n_chunks, " chunks...")
  
  # Create an empty vector to store match results
  match_results <- logical(total_rows)
  
  # Process chunks with progress reporting
  with_progress({
    # Initialize progressor with exact number of steps
    p <- progressor(steps = n_chunks)
    
    # Loop through chunks but ensure we don't update progress beyond the total
    for (i in 1:n_chunks) {
      # Define chunk indices
      start_idx <- (i-1) * chunk_size + 1
      end_idx <- min(i * chunk_size, total_rows)
      chunk_indices <- start_idx:end_idx
      
      # Get text for this chunk
      chunk_texts <- df$text[chunk_indices]
      
      # Process the chunk - do this as a vectorized operation
      chunk_results <- grepl(pattern, chunk_texts, ignore.case = TRUE)
      
      # Store results
      match_results[chunk_indices] <- chunk_results
      
      # Update progress (only if we haven't reached the total)
      if (i <= n_chunks) {
        p(amount = 1, message = sprintf("Processed chunk %d/%d", i, n_chunks))
      }
      
      # Force garbage collection between chunks
      rm(chunk_texts, chunk_results)
      gc(full = TRUE)
    }
  })

  # Filter the data frame using the match results
  df$match <- match_results
  filtered_df <- df[df$match == TRUE, ]
  message("Found ", nrow(filtered_df), " matching speeches.")
  filtered_df$match <- NULL

  message("Saving filtered speeches to: ", output_path)
  saveRDS(filtered_df, output_path)
  
  # Add timing information for the whole process
  message(sprintf("Total processing time: %.1f minutes", 
                 as.numeric(difftime(Sys.time(), t0, units = "mins"))))

  message("✅ Filtering complete.")
}