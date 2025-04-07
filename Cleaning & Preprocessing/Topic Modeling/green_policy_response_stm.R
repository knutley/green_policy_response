## Code by: Aarushi Sharma and Katie Nutley 
## Created on: 07 April 2025
## Filtered Data from: Elisa D'Amico (GitHub: green_policy_response/Cleaning & Preprocessing/Initial Filtering)

# 1. Load Filtered Data # 

df <- readRDS("~/Documents/GitHub/green_policy_response/Cleaning & Preprocessing/Initial Filtering/filtered_UK (1).rds")
View(df)

# 2. Load Required Libraries # 

library(knitr)
library(LDAvis)
library(readr)
library(tidyverse)
library(quanteda) # quantitative analysis of textual data  (https://quanteda.io/articles/quickstart.html)
library(quanteda.textplots) # complementary to quanteda, for visualisation
library(tidytext) #conversion of text to and from tidy formats
library(cld3) # for language detection
library(servr) # will be used for visualisation
library(topicmodels) # implementation of Latent Dirichlet Allocation, several topic models included
library(lda) # alternative to topicmodels, implementation of LDA only
library(stm) # for structural topic modeling
library(dplyr)
library(ggplot2)
library(textstem)

# Note to Katie: You need to clean up this section of loading the required libraries
# before anything gets published on GitHub. You can bring the notes you've made down
# below. 

# 3. Set Seed for Reproducibility # 

set.seed(123)

# 4. Summary Statistics # 

## 4.1 SumStats - Speaker and Party ## 

# Just to give an overview of how many distinct speakers and parties there are
# this might come in handy later if we need to look at which parties and speakers
# are most responsive to environmental protest! 
df %>% summarise(
  unique_names = n_distinct(speaker),
  unique_parties = n_distinct(party)
) # There's also the potentiality here to expand this using a gender analysis. 
# I can use an estimation tool I've used elsewhere to do this. Might provide 
# interesting insight. 

df %>%
  mutate(ntoken = stringr::str_count(text, "\\S+")) %>%  # Count words in text
  group_by(speaker) %>%
  summarise(count = n(), avg_ntoken = mean(ntoken, na.rm = TRUE)) %>%
  arrange(desc(count))

df %>%
  mutate(ntoken = stringr::str_count(text, "\\S+")) %>%  # Count words in text
  group_by(party) %>%
  summarise(count = n(), avg_ntoken = mean(ntoken, na.rm = TRUE))  %>%
  arrange(desc(count))

# Note: These summary statistics show the actual count of speeches in the UK 
# filtered corpus (that is speeches which mention climate change and its ill
# effects) and the average number of words (not unique) in every speech for 
# individual speakers and parties (in descending order). 

# Now, for some slightly more advanced stuff. 

UK_corpus <- quanteda::corpus(df$text)
df %>%
  mutate(ntoken = stringr::str_count(text, "\\S+")) %>%
  ggplot(aes(x=ntoken)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  labs(title = "Distribution of Speech Lengths", x = "Number of Tokens", y = "Frequency")

# I included this because I think it might be interesting to see how non-ecological
# speech lengths might compare. Are speakers paying comparatively brief lip service
# to ecological issues? 

# Boxplot for speech length by party. (Speaker is too granular at this point.)

df %>% 
  mutate(ntoken = stringr:: str_count(text, "\\S+"))%>%
  ggplot(aes(x = party, y = ntoken, fill = party)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Speech Length by Party", x = "Party", y = "Number of Tokens")

# 5. Creating a DFM # 

# Tokenising the corpus: 
UK_token_speeches <- quanteda::tokens(UK_corpus, remove_punct = TRUE, remove_numbers = TRUE)

# Building the document-feature matrix: 
UK_dfm_speeches <- dfm(UK_token_speeches)

# Going to go ahead and plot frequency and see what happens. 

textplot_wordcloud(UK_dfm_speeches, random_order = FALSE, rotation = 0.25,
                   color = RColorBrewer::brewer.pal(8, "Dark2")) # Fairly 
# It's evident from this which articles, linking words and prepositions need to be taken out. 

## 5.1 Stop Words and Punctuation ##

# For transparency, I created this stop word CSV by copying my word plot into ChatGPT
# with the query, "Using this word cloud, create a list of stop words that are not related 
# to climate change legislation."

# Read the CSV file: 
UK_custom_stopwords <- read_csv("~/Downloads/stop_words.csv")

# Create a complete vector of stopwords:
UK_custom_stopwords_vector <- c(UK_custom_stopwords$stop_word, stopwords("en"), 'too', 'why') # Basically,
# just adding any additional words I missed here. (Note to Katie: you can add to this later!)

# I tried to run this with the code below, but the DFM creation was running out 
# of memory because there were too many documents. So, I decided to break it down 
# even more. 

# UK_dfm_speeches_corpus <- UK_corpus %>% 
#   quanteda::tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>%
#   quanteda::tokens_remove(UK_custom_stopwords) %>%
#   quanteda::tokens_wordstem() %>%
#   quanteda::tokens_ngrams(n = c(1, 2)) %>%
#   dfm() %>%
#   dfm_tolower() %>%
#   dfm_trim(min_termfreq = 5, min_docfreq = 0.0025, docfreq_type = "prop")

# Process tokens first:
UK_tokens <- UK_corpus %>% 
  quanteda::tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>%
  quanteda::tokens_remove(UK_custom_stopwords_vector) %>%
  quanteda::tokens_wordstem()

# Create n-grams with more memory-efficient approach:  
UK_tokens_ngrams <- quanteda::tokens_ngrams(UK_tokens, n = c(1, 2))

# Create DFM with early trimming to reduce size:
UK_dfm_speeches_corpus <- UK_tokens_ngrams %>%
  dfm() %>%
  dfm_tolower() %>%
  # Add more aggressive trimming if needed: 
  dfm_trim(min_termfreq = 10, min_docfreq = 0.005, docfreq_type = "prop")

View(UK_dfm_speeches_corpus)

# Going to plot it and see if that looks better <3 

textplot_wordcloud(UK_dfm_speeches_corpus, random_order = FALSE, rotation = 0.25, 
                   color = RColorBrewer::brewer.pal(8, "Dark2"),max_words =100,max_size = 3)

# 6. Run the Text Structural Topic Model (STM) # 

# Create metadata: 
UK_doc_metadata <- df %>%
  select(party, speaker, date)
rownames(UK_doc_metadata) <- docnames(UK_dfm_speeches_corpus)

# Now run STM with the proper metadata: 
set.seed(123)
UK_stm_model <- stm(UK_dfm_speeches_corpus, K = 20, max.em.its = 10, data = UK_doc_metadata)

# Plot STM: 
plot(UK_stm_model)

# Examine the top words from each topic: 
UK_topic_labels <- labelTopics(UK_stm_model)
view(UK_topic_labels)

# Print the words for each topic: 
print(UK_topic_labels$prob)

# Visualising the correlation between topics:
topicCorr(UK_stm_model)

# Longer way around: 
# topic_proportions <- UK_stm_model$theta
# cor_matrix <- cor(topic_proportions)
# print(cor_matrix)

################################################################################

# Right, so having looked at the text itself, we're moving on to the set agenda
# topic in the ParlSpeech V5 data. 

run_topic_modelling <- function(filtered_path = "~/Documents/GitHub/green_policy_response/Cleaning & Preprocessing/Initial Filtering/filtered_UK (1).rds") {
  message("[1/3] Checking for filtered data file...")
  if (!file.exists(filtered_path)) {
    message("Error: Filtered data file not found at: ", filtered_path)
    message("You need to run the filtering stage first:")
    message("  ./run.sh filter")
    message("This will generate the filtered data file required for topic modeling.")
    return(NULL)
  }
  
  message("[2/3] Loading filtered data...")
  tryCatch({
    df <- readRDS(filtered_path)
    message("Successfully loaded data with ", nrow(df), " filtered rows from ", filtered_path)
    
    # Check if the dataframe has the expected structure
    if (!("text" %in% names(df))) {
      message("Warning: The RDS file does not contain a 'text' column, which is needed for topic modeling.")
      return(NULL)
    }
    
    if (nrow(df) == 0) {
      message("Warning: The filtered dataframe is empty. No data to model.")
      return(NULL)
    }
    
    message("[3/3] Topic modeling (stub)...")
    message("Future implementation will include:")
    message("  - Text preprocessing (stopword removal, lemmatization)")
    message("  - Document-term matrix creation")
    message("  - Topic model selection (STM, LDA, etc.)")
    message("  - Model parameter tuning")
    message("  - Topic visualization and interpretation")
    message("  - Topic classification of speeches")
    
    # Random sample of text for preview
    if (nrow(df) > 0) {
      sample_idx <- sample(1:nrow(df), min(3, nrow(df)))
      message("\nSample texts that will be used for topic modeling:")
      for (i in 1:length(sample_idx)) {
        sample_text <- df$text[sample_idx[i]]
        if (!is.na(sample_text) && nchar(sample_text) > 0) {
          sample_preview <- substr(sample_text, 1, 150)
          message(i, ": ", sample_preview, "...")
        }
      }
    }
    
    message("\n✅ Topic modeling preview complete. Full implementation coming soon.")
    
  }, error = function(e) {
    message("Error loading data: ", e$message)
    message("The file may be corrupted or in an unexpected format.")
    return(NULL)
  })
  
  return(NULL)
} 

result <- run_topic_modelling()
view(result)
