# Created by Elisa D'Amico on 19 MAR 2025
# Pilots translation of the first line of the Corp_Folketing_V2.rds data 
# ParlSpeech Download - https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/L4OAKN/YKH4KG&version=1.0

# Load required library
library(googleLanguageR)

getwd()
#setwd() # If needed

Corp_Folketing_V2 <- readRDS("Corp_Folketing_V2.rds")

# Authenticate with your service account
gl_auth("greenpolicyresponse-5877a60aeaf4.json")

# Test on row 1 of Corp_Folketing_V2
test_text <- Corp_Folketing_V2$text[1]

# Check if the text exists
if (!is.na(test_text) && test_text != "") {
  
  # Detect language
  detection_result <- gl_translate_detect(test_text)
  detected_lang <- detection_result$language
  print(paste("Detected language:", detected_lang))
  
  # Translate text if not in English
  if (detected_lang != "en") {
    translation_result <- gl_translate(test_text, target = "en")
    translated_text <- translation_result$translatedText
    print(paste("Translated text:", translated_text))
  } else {
    translated_text <- test_text
    print("Text is already in English.")
  }
  
  # Merge back into dataframe
  Corp_Folketing_V2$detected_language[1] <- detected_lang
  Corp_Folketing_V2$translated_text[1] <- translated_text
  
} else {
  print("Row 1 has missing or empty text.")
}
