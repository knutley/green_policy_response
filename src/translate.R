#' Translate First Line of Folketing Data using Google Translate API
#' 
#' Detects the language of the first line of text in a given RDS file,
#' and translates it to English if necessary.
#'
#' Adapted from: GoogleTranslateAPI.R by Elisa D'Amico, 19 MAR 2025

source("src/utils/download_if_missing.R")

library(googleLanguageR)

translate_first_line <- function(input_path = "data/raw/Corp_Folketing_V2.rds", 
                                auth_path = "greenpolicyresponse-5877a60aeaf4.json") {
  message("[1/4] Ensuring input file exists or downloading if needed...")
  
  # Debug - read and check the required_files.csv directly
  filename <- basename(input_path)
  metadata_path <- "data/required_files.csv"

  if (file.exists(metadata_path)) {
    urls <- data.table::fread(metadata_path)
    # Ensure trimmed whitespace for comparison
    urls[, filename := trimws(filename)]
    filename <- trimws(filename)
    
    match <- urls[urls$filename == filename, ]
  }
  
  input_path <- download_if_missing(
    filename = filename,
    dest_dir = dirname(input_path)
  )
  
  message("[2/4] Authenticating with Google Cloud...")
  
  # Check if the auth file exists
  if (!file.exists(auth_path)) {
    message("Warning: Google Cloud authentication file not found at: ", auth_path)
    message("Skipping translation step. To run translation, you need to:")
    message("1. Create a Google Cloud project with Translation API enabled")
    message("2. Create a service account key and download as JSON")
    message("3. Place the JSON file at the path: ", auth_path)
    message("4. Run this script again")
    return(NULL)
  }
  
  # Try to authenticate with proper error handling
  tryCatch({
    gl_auth(auth_path)
    message("Successfully authenticated with Google Cloud")
  }, error = function(e) {
    message("Error authenticating with Google Cloud: ", e$message)
    message("Please check that your JSON key file is valid and has the necessary permissions.")
    return(NULL)
  })

  message("[3/4] Loading data from:", input_path)
  df <- readRDS(input_path)
  
  # Check if the dataframe has the expected structure
  if (!("text" %in% names(df))) {
    message("Error: The RDS file does not contain a 'text' column.")
    return(NULL)
  }
  
  # Check if we have any data
  if (nrow(df) == 0) {
    message("Error: The dataframe is empty.")
    return(NULL)
  }
  
  text <- df$text[1]

  message("[4/4] Performing language detection and translation...")
  # Only proceed if the first row of text is not missing or empty
  if (!is.na(text) && text != "") {
    # Detect the language
    tryCatch({
      detection_result <- gl_translate_detect(text)
      lang <- detection_result$language
      message("Detected language:", lang)

      # Translate only if it's not English
      if (lang != "en") {
        translation_result <- gl_translate(text, target = "en")
        translated <- translation_result$translatedText
        message("Translated text successfully.")
      } else {
        translated <- text
        message("Text is already in English.")
      }

      # Add language detection and translation to the dataframe
      df$detected_language[1] <- lang
      df$translated_text[1] <- translated

      # Print results
      message("Sample results:")
      message("  Original: ", substr(text, 1, 100), "...")
      message("  Detected: ", lang)
      message("  Translated: ", substr(translated, 1, 100), "...")
    }, error = function(e) {
      message("Error during translation: ", e$message)
      message("This might be due to API limits, invalid credentials, or network issues.")
      return(NULL)
    })
  } else {
    message("Missing or empty text in row 1")
  }
  
  message("✅ Translation test complete.")
  return(df)
}