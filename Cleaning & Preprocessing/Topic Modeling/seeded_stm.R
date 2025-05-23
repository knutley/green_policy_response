## Supervised Topic Modelling 
## Code by: Katie Nutley 
## 22 April 2025


# 1. Load the filtered data 
df <- readRDS("~/Documents/GitHub/green_policy_response/Cleaning & Preprocessing/Initial Filtering/filtered_UK (1).rds")

# 2. Load Required Libraries
library(tidyverse)
library(quanteda)
library(stm)
library(dplyr)
library(stringr)

# 3. Set Seed for Reproducibility
set.seed(123)

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
# I went back through these with a fine tooth comb, and I think the following changes might be worth considering: 
# 4. Define Mitigation and Adaptation Dictionaries
# mitigation_terms <- c(
# "greenhouse gas", "GHG emissions", "carbon emissions", "carbon footprint",
#  "carbon neutrality", "carbon sequestration", "carbon offsetting", "carbon sink",
#  "carbon pricing", "carbon levy", "carbon credit", "carbon trading", "methane emissions",
#  "CO2 reduction", "net zero", "zero-carbon economy", "low-carbon economy", "global warming",
#  "climate mitigation policy", "climate mitigation",
#  "renewable energy", "clean energy", "solar power", "wind power", "hydropower",
#  "geothermal energy", "bioenergy", "tidal energy", "wave energy", "sustainable energy",
#  "energy transition", "green energy", "low-carbon energy", "off-grid solutions",
#  "energy efficiency", "clean technology", "green technology", "energy conservation",
#  "fossil fuel phase-out", "coal phase-out", "oil dependency", "fossil fuel divestment",
#  "decarbonization", "transition away from fossil fuels", "energy decarbonization",
#  "clean transportation", "electric vehicles", "green transportation", "hydrogen economy",
#  "carbon tax", "net-zero commitments", "carbon capture and storage", "direct air capture", "circular economy", "deforestation", "afforestation", “sustainable supply chains” , "climate finance", "green finance", "climate investment"
#)

# adaptation_terms <- c(
#  "climate adaptation", "climate resilience", "adaptive capacity", "climate vulnerability",
#  "vulnerability assessment", "climate-resilient development", "climate-proofing",
#  "climate risk management", "disaster risk reduction", "early warning systems",
#  "resilient infrastructure", "coastal protection", "sea level rise adaptation",
#  "flood protection", "drought management", "heat resilience", "urban adaptation",
#  "climate-resilient agriculture", "climate-resilient infrastructure", "nature-based solutions",
#  "ecosystem-based adaptation", "resilient ecosystems", "resilient cities",
#  "climate insurance", "climate risk insurance", "adaptation finance", "adaptation funding",
#  "national adaptation plan", "local adaptation plan", "community-based adaptation",
#  "climate migration", "climate displacement", "managed retreat", "climate refugees",
#  "biodiversity", "biodiversity loss", "ecosystem collapse", "ecosystem restoration",
#  "environmental degradation", "reforestation", 
#  "forest conservation", "land degradation", "land-use change", "habitat destruction",
#  "wildlife conservation", "marine protection", "ocean conservation", "coral bleaching",
#  "ocean acidification", "plastic pollution", "sustainable forestry", "land restoration",
#  "loss and damage", "climate-induced loss", "climate-induced damage", "compensation for loss and damage",
#  "climate reparations", "climate risk insurance", "financial mechanisms for climate loss",
#  "climate finance", "green finance", "climate investment", "sustainable investment",
#  "sustainable development finance", "green economy", 
# "climate-resilient infrastructure", "green stimulus",
#  "sustainable agriculture", "climate-smart agriculture", "regenerative agriculture",
#  "food security", "water security", "drought resilience", "flood-resilient crops",
#  "agroecology", "soil conservation", "sustainable fisheries", "ocean governance",
#  "water conservation", "climate-resilient farming"
# )

# Changes I made: 
# -	Moved “circular economy” from adaptation to mitigation
# -	Moved “deforestation” and “afforestation” from adaptation to mitigation 
# -	Moved “sustainable supply chains” from adaptation to mitgation 
# -	Added "climate finance", "green finance", "climate investment" to mitigation too as per my own understanding


# 5. Create Document-Feature Matrix with Preprocessing
# Create corpus
UK_corpus <- quanteda::corpus(df$text, docnames = df$doc_id)

# Create UK custom stopwords
UK_custom_stopwords_vector <- c(stopwords("en"), 'too', 'why') # We can expand these, 
# but I was sort of at a loss. 

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
df <- df %>%
  mutate(
    mitigation_score = sapply(text, function(t) count_term_occurrences(t, mitigation_terms)),
    adaptation_score = sapply(text, function(t) count_term_occurrences(t, adaptation_terms)),
    # Create normalized scores (0-1 range)
    mitigation_norm = mitigation_score / (mitigation_score + adaptation_score + 0.001), # Adding small constant to avoid division by zero
    adaptation_norm = adaptation_score / (mitigation_score + adaptation_score + 0.001),
    # Create categorical variables
    primary_focus = case_when(
      mitigation_score > adaptation_score ~ "Mitigation",
      adaptation_score > mitigation_score ~ "Adaptation",
      TRUE ~ "Mixed"
    )
  )

View(df) # This is cool, but I'm not convinced? Essentially, I think that if it's 
# wholly adaptation, it should be labelled as such. Same with mitigation. If it's 
# a mixed approach, I want that to be labeled. Then if there's no approach it should
# just be an NA value. 

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
View(df)
table(df$primary_focus)

saveRDS(df, "green_policy_norm_scores.rds") # Sent this to Elisa. She suggested I 
# do some spotchecks to improve it, but that's for Later Katie. 

# 7. Create metadata for STM

# Create metadata and match docnames exactly 

UK_doc_metadata <- df %>%
  select(party, speaker, date, mitigation_norm, adaptation_norm, primary_focus)
View(UK_doc_metadata)
table(UK_doc_metadata$primary_focus)

# 8. Run Supervised STM with Speehc Content Covariates

set.seed(123) 

UK_supervised_stm <- stm(
  UK_dfm_speeches_corpus,
  K = 20,
  prevalence = ~ party,
  content = ~ primary_focus,
  max.em.its = 75,
  data = UK_doc_metadata,
  verbose = TRUE
) # Ran into issues here. 

sum(is.na(UK_doc_metadata$party))
sum(is.na(UK_doc_metadata$primary_focus)) #Right, I seem to have missing values 
# here, so I have to correct that before I can run the stm. 

# Going to troubleshoot through Claude... 

# Check if the DFM has any tokens
sum(UK_dfm_speeches_corpus)

# Check feature counts in first few documents
head(rowSums(UK_dfm_speeches_corpus)) #Contains data, so according to Claude, I can move on.

# Convert DFM to STM format explicitly
# UK_stm_input <- quanteda::dfm2stm(UK_dfm_speeches_corpus, docvars = UK_doc_metadata_imputed)
# This didn't work. 

# Check the structure of the converted object
# str(UK_stm_input)

# First, try importing the converter function correctly
library(stm)

# Convert DFM to STM format using the stm package's converter function
UK_stm_input <- convert(UK_dfm_speeches_corpus, to = "stm", docvars = UK_doc_metadata_imputed)

# Check the structure of the converted object
str(UK_stm_input)

# Get the non-empty documents
nonzero_docs <- rowSums(UK_dfm_speeches_corpus) > 0
UK_dfm_nonzero <- UK_dfm_speeches_corpus[nonzero_docs, ]

# Get matching metadata
UK_metadata_nonzero <- UK_doc_metadata_imputed[nonzero_docs, ]

# Now run the STM on the filtered data
UK_supervised_stm <- stm(
  UK_dfm_nonzero,
  K = 20,
  prevalence = ~ party,
  content = ~ primary_focus,
  max.em.its = 75,
  data = UK_metadata_nonzero,
  verbose = TRUE
) # This worked wonderfully, thanks Claude. But also, it's a bitch to run. Don't 
# do it yourself. Takes ages.

# 9. Analyzing Results
# Plot STM
plot(UK_supervised_stm)

# Examine topic labels
UK_topic_labels <- labelTopics(UK_supervised_stm)

# Print words for each topic
sink("UK_stm_topic_labels.txt") 
print(UK_topic_labels)
sink()

# Topic correlation
topic_corr <- topicCorr(UK_supervised_stm)
plot(topic_corr) # This is hugely cool, but needs refined. It should show up as 
# individual topics rather than numbered. But, that's a job for Later Kater. But,
# let's expand this a bit. 

# 10. Create topic probability summary table
topic_proportions <- colMeans(UK_supervised_stm$theta) 

exists("UK_topic_labels")
str(UK_topic_labels)

# Since your labels are stored in the 'topics' component rather than 'frex'
topic_labels <- apply(UK_topic_labels$topics, 1, paste, collapse = ", ")

# Then create your summary table
topic_proportions <- colMeans(UK_supervised_stm$theta)
topic_summary <- data.frame(
  Topic = 1:length(topic_proportions),
  Label = topic_labels,
  Proportion = topic_proportions
)

# Sort by proportion (optional)
topic_summary <- topic_summary[order(topic_summary$Proportion, decreasing = TRUE), ]

# View the result
print(topic_summary)

# Save results
write_csv(topic_summary, "supervised_climate_policy_stm.csv")

#################################################################################

# I need to rework this section. It was thrown out of wack by changes I made earlier
# in the document and I need to figure out which. 

# 11. Effect of Content Covariates on Topics
# Examine how mitigation and adaptation terms influence topics
mitigation_effect <- estimateEffect(1:20 ~ mitigation_norm, UK_supervised_stm, meta = UK_doc_metadata)
adaptation_effect <- estimateEffect(1:20 ~ adaptation_norm, UK_supervised_stm, meta = UK_doc_metadata)

# Plot effects for selected topics (adjust topics based on your results)
plot(mitigation_effect, covariate = "mitigation_norm", topics = c(1:5), 
     main = "Effect of Mitigation Focus on Selected Topics")
plot(adaptation_effect, covariate = "adaptation_norm", topics = c(1:5), 
     main = "Effect of Adaptation Focus on Selected Topics")

# 12. Categorize Topics by Primary Focus
# Create a function to determine if a topic is more mitigation or adaptation focused
get_topic_category <- function(topic_id, model, labels) {
  # Get the top words for this topic
  top_words <- labels$frex[topic_id,]
  
  # Count mitigation and adaptation terms in top words
  mitigation_count <- sum(top_words %in% mitigation_terms)
  adaptation_count <- sum(top_words %in% adaptation_terms)
  
  if(mitigation_count > adaptation_count) {
    return("Mitigation")
  } else if(adaptation_count > mitigation_count) {
    return("Adaptation")
  } else {
    return("Mixed")
  }
}

# Add categorization to topic summary
topic_summary$Category <- sapply(topic_summary$Topic, 
                                 function(x) get_topic_category(x, UK_supervised_stm, UK_topic_labels))

# Save the categorized topics
write_csv(topic_summary, "categorized_climate_policy_topics.csv")

# 13. Analyzing Party Differences in Topic Focus
# Extract topic proportions by party
topic_proportions_by_party <- estimateEffect(1:20 ~ party, UK_supervised_stm, meta = UK_doc_metadata)

# Plot party differences for selected topics (adjust topics based on your results)
plot(topic_proportions_by_party, covariate = "party", topics = c(1:5),
     main = "Topic Proportions by Political Party")

# 14. Create Visualizations for Reporting
# Average topic proportions by party and topic category
party_category_summary <- df %>%
  mutate(
    max_topic = apply(UK_supervised_stm$theta, 1, which.max)
  ) %>%
  left_join(
    topic_summary %>% select(Topic, Category),
    by = c("max_topic" = "Topic")
  ) %>%
  group_by(party, Category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(party) %>%
  mutate(proportion = count / sum(count))

# Plot party focus on mitigation vs adaptation
ggplot(party_category_summary, aes(x = party, y = proportion, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Focus on Climate Categories by Political Party",
       x = "Party", y = "Proportion of Speeches", fill = "Climate Category")

# Save this plot
ggsave("party_climate_focus.png", width = 10, height = 6)

# 15. Time Trends in Climate Discussion
# First ensure date is properly formatted
df$year <- lubridate::year(df$date)

# Create time trend analysis
time_trend <- df %>%
  group_by(year, primary_focus) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(proportion = count / sum(count))

# Plot time trends
ggplot(time_trend, aes(x = year, y = proportion, color = primary_focus, group = primary_focus)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Trends in Climate Discussion Focus Over Time",
       x = "Year", y = "Proportion of Speeches", color = "Primary Focus") +
  scale_color_brewer(palette = "Set1")

# Save this plot
ggsave("climate_focus_trends.png", width = 10, height = 6)
