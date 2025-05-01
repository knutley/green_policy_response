## Normalised Cat-Class Code for Adapt/Mitigate
## Code by: Katelyn Nutley
## 01-05-2025

# 1. Load the filtered data 
df <- readRDS("~/Documents/GitHub/green_policy_response/Cleaning & Preprocessing/Initial Filtering/filtered_UK (1).rds")

# 2. Load Required Libraries
library(tidyverse)
library(quanteda)
library(stm)
library(dplyr)
library(stringr)

# 3. Set Seed for Reproducibility
set.seed(123) # This might be redundant -- best to check. 

# 4. Define Mitigation and Adaptation Dictionaries
mitigation_terms <- c(
  "greenhouse gas", "GHG emissions", "carbon emissions", "carbon footprint",
  "carbon neutrality", "carbon sequestration", "carbon offsetting", "carbon sink",
  "carbon pricing", "carbon levy", "carbon credit", "carbon trading", "methane emissions",
  "CO2 reduction", "net zero", "zero-carbon economy", "low-carbon economy", "global warming",
  "climate mitigation policy", "climate mitigation",
  "renewable energy", "clean energy", "solar power", "wind power", "hydropower",
  "geothermal energy", "bioenergy", "tidal energy", "wave energy", "sustainable energy",
  "energy transition", "green energy", "low-carbon energy", "off-grid solutions",
  "energy efficiency", "clean technology", "green technology", "energy conservation",
  "fossil fuel phase-out", "coal phase-out", "oil dependency", "fossil fuel divestment",
  "decarbonization", "transition away from fossil fuels", "energy decarbonization",
  "clean transportation", "electric vehicles", "green transportation", "hydrogen economy",
  "carbon tax", "net-zero commitments", "carbon capture and storage", "direct air capture"
)

adaptation_terms <- c(
  "climate adaptation", "climate resilience", "adaptive capacity", "climate vulnerability",
  "vulnerability assessment", "climate-resilient development", "climate-proofing",
  "climate risk management", "disaster risk reduction", "early warning systems",
  "resilient infrastructure", "coastal protection", "sea level rise adaptation",
  "flood protection", "drought management", "heat resilience", "urban adaptation",
  "climate-resilient agriculture", "climate-resilient infrastructure", "nature-based solutions",
  "ecosystem-based adaptation", "resilient ecosystems", "resilient cities",
  "climate insurance", "climate risk insurance", "adaptation finance", "adaptation funding",
  "national adaptation plan", "local adaptation plan", "community-based adaptation",
  "climate migration", "climate displacement", "managed retreat", "climate refugees",
  "biodiversity", "biodiversity loss", "ecosystem collapse", "ecosystem restoration",
  "environmental degradation", "deforestation", "reforestation", "afforestation",
  "forest conservation", "land degradation", "land-use change", "habitat destruction",
  "wildlife conservation", "marine protection", "ocean conservation", "coral bleaching",
  "ocean acidification", "plastic pollution", "sustainable forestry", "land restoration",
  "loss and damage", "climate-induced loss", "climate-induced damage", "compensation for loss and damage",
  "climate reparations", "climate risk insurance", "financial mechanisms for climate loss",
  "climate finance", "green finance", "climate investment", "sustainable investment",
  "sustainable development finance", "green economy", "circular economy",
  "sustainable supply chains", "climate-resilient infrastructure", "green stimulus",
  "sustainable agriculture", "climate-smart agriculture", "regenerative agriculture",
  "food security", "water security", "drought resilience", "flood-resilient crops",
  "agroecology", "soil conservation", "sustainable fisheries", "ocean governance",
  "water conservation", "climate-resilient farming"
)

# 5. Create Document-Feature Matrix with Preprocessing
# Create corpus
UK_corpus <- quanteda::corpus(df$text, docnames = df$doc_id)

# Create UK custom stopwords
UK_custom_stopwords_vector <- c(stopwords("en"), 'too', 'why') # We can expand these, 
# but we need to discuss how. 

# Process tokens
UK_tokens <- UK_corpus %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(UK_custom_stopwords_vector) %>%
  tokens_wordstem()

# Create n-grams
UK_tokens_ngrams <- tokens_ngrams(UK_tokens, n = c(1, 2))

# Create DFM
UK_dfm_speeches_corpus <- UK_tokens_ngrams %>%
  dfm() %>%
  dfm_tolower() %>%
  dfm_trim(min_termfreq = 10, min_docfreq = 0.005, docfreq_type = "prop")

# 6. Create Category Variables for the Quasi-supervised STM
# Function to count term occurrences in a text
count_term_occurrences <- function(text, terms) {
  text_lower <- tolower(text)
  sum(sapply(terms, function(term) str_count(text_lower, fixed(term))))
}

# Add mitigation and adaptation scores to dataframe
# df <- df %>%
#  mutate(
#    mitigation_score = sapply(text, function(t) count_term_occurrences(t, mitigation_terms)),
#    adaptation_score = sapply(text, function(t) count_term_occurrences(t, adaptation_terms)),
#    # Create normalized scores (0-1 range)
#    mitigation_norm = mitigation_score / (mitigation_score + adaptation_score + 0.001), # Adding small constant to avoid division by zero
#    adaptation_norm = adaptation_score / (mitigation_score + adaptation_score + 0.001),
#    # Create categorical variables
#    primary_focus = case_when(
#      mitigation_score > adaptation_score ~ "Mitigation",
#      adaptation_score > mitigation_score ~ "Adaptation",
#      TRUE ~ "Mixed"
#    )
#  )
# View(df) 

# This is on the right track in that it adds mitigation and adaptation scores, but 
# I think that if it's wholly adaptation, it should be labelled as such. Same with 
# mitigation. If it's a mixed approach, I want that to be labeled. Then if there's
# no approach it should just be an NA value. 

df <- df %>%
  mutate(
    mitigation_score = sapply(text, function(t) count_term_occurrences(t, mitigation_terms)),
    adaptation_score = sapply(text, function(t) count_term_occurrences(t, adaptation_terms)),
    # Create categorical variables
    primary_focus = case_when(
      mitigation_score > 0 & adaptation_score == 0 ~ "Mitigation",
      adaptation_score > 0 & mitigation_score == 0 ~ "Adaptation",
      mitigation_score > 0 & adaptation_score > 0 ~ "Mixed",
      TRUE ~ "NA"
    )
  )
View(df) # Sent this to Elisa and she suggested that I add a threshold, so the 
# following chunk of code creates a separate scoring system in which it introduces 
# a threshold of 3 climate related terms, and then a classification of not climate
# focused, primarily mitigation, primarily adapatation, and then a balanced approach.

df %>% filter(primary_focus == "Adaptation")

table(df$primary_focus)

# df <- df %>%
#  mutate(
#    # Calculate total climate terms
#    total_climate_terms = mitigation_score + adaptation_score,
#    
#    # Only apply classification if enough climate terms exist
#    primary_focus_improved = case_when(
#      total_climate_terms < 3 ~ "Not Climate Focused",
#      mitigation_score / total_climate_terms > 0.8 ~ "Primarily Mitigation",
#      adaptation_score / total_climate_terms > 0.8 ~ "Primarily Adaptation",
#      TRUE ~ "Balanced Approach"
#    )
#  )
# View(df)

# So, I chose to spot check some of this to figure out if a threshold makes this 
# more accurate. 

# table(df$primary_focus)
# table(df$primary_focus_improved) # Adaptation -> Primarily Adaptation reduced by
# 90%; Mitigtion -> Primarily Mitigtion reduced by 80%; Mixed -> Balanced Approach
# reduced by 40 % -- so the amount of environmental observations are getting smaller. 
# I decided to spot check it by hand, starting with the Adaptation category 
# adapt_sub <- df %>% filter (primary_focus == "Adaptation"), and found that of 
# the first ten rows, the simple adapt/mitigate code performed better. Essentially, adding
# a threshold made it too sensitive. In the primarily adapt/mitigate code, 20% seemed
# right from the spot check; whereas the adapt/mitigate without a threshold was 80% right. 
# Need to discuss this with Elisa. 

# Also worth noting that the term sustainable seems to be doing a lot of heavy lifting
# can you threshold that alone? I guess it would be some sort of conditional statement. 

saveRDS(df, "insert_name_here.rds") 