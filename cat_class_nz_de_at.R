## Cat Class Code for NZ, Germany, and Austria 
## Code by: Katelyn Nutley
## 02-05-2025

##################################################################################

### New Zealand ### 

# 1. Load the filtered data 
df_NZ <- readRDS("~/Documents/GitHub/green_policy_response/filtered_NZ.rds")

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
NZ_corpus <- quanteda::corpus(df_NZ$text, docnames = df_NZ$doc_id)

# Create German custom stopwords
NZ_custom_stopwords_vector <- c(stopwords("en"), 'too', 'why') # We can expand these, 
# but we need to discuss how. 

# Process tokens
NZ_tokens <- NZ_corpus %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(NZ_custom_stopwords_vector) %>%
  tokens_wordstem()

# Create n-grams
NZ_tokens_ngrams <- tokens_ngrams(NZ_tokens, n = c(1, 2))

# Create DFM
NZ_dfm_speeches_corpus <- NZ_tokens_ngrams %>%
  dfm() %>%
  dfm_tolower() %>%
  dfm_trim(min_termfreq = 10, min_docfreq = 0.005, docfreq_type = "prop")

# 6. Create Category Variables 

# Function to count term occurrences in a text
count_term_occurrences <- function(text, terms) {
  text_lower <- tolower(text)
  sum(sapply(terms, function(term) str_count(text_lower, fixed(term))))
}

# Add mitigation and adaptation scores to dataframe
df_NZ <- df_NZ %>%
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
View(df_NZ) 

# This next chunk of code improves on the first. 

df_NZ <- df_NZ %>%
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
View(df_NZ) 

table(df_NZ$primary_focus)

# 7. Save the Scored Data 

saveRDS(df_NZ, "scored_NZ.rds") 

#################################################################################

### Germany ### 

library(dplyr)
library(stringi)

# 1. Load the data
df_DE <- readRDS("~/Downloads/Corp_Bundestag_V2.rds") # I had to use unfiltered
# here because there was an issue when I pushed the filtered through the code above.
# Decided to basically strip it back, normalise the language and then run the adapt/
# mitigate code. 

# 2. Define Mitigation and Adaptation Dictionaries
mitigation_terms_de <- c(
  "Treibhausgas", "Treibhausgasemissionen", "CO₂-Emissionen", "CO₂-Fußabdruck",
  "Klimaneutralität", "Kohlenstoffbindung", "CO₂-Kompensation", "Kohlenstoffsenke",
  "CO₂-Bepreisung", "CO₂-Abgabe", "Emissionszertifikat", "Emissionshandel", "Methanemissionen",
  "CO₂-Reduktion", "Netto-Null", "klimaneutrale Wirtschaft", "kohlenstoffarme Wirtschaft", "Erderwärmung",
  "Klimaschutzpolitik", "Klimaschutzmaßnahmen", "erneuerbare Energie", "saubere Energie", 
  "Solarenergie", "Windkraft", "Wasserkraft","Geothermie", "Bioenergie", "Gezeitenenergie", 
  "Wellenenergie", "nachhaltige Energie","Energiewende", "grüne Energie", "emissionsarme Energie", 
  "netzunabhängige Lösungen","Energieeffizienz", "Umwelttechnologie", "grüne Technologie", 
  "Energieeinsparung", "Ausstieg aus fossilen Brennstoffen", "Kohleausstieg", "Ölabhängigkeit", 
  "Desinvestition aus fossilen Brennstoffen", "Dekarbonisierung", "Abkehr von fossilen Brennstoffen", 
  "Dekarbonisierung der Energie", "umweltfreundlicher Verkehr", "Elektrofahrzeuge", 
  "grüne Mobilität", "Wasserstoffwirtschaft", "CO₂-Steuer", "Netto-Null-Verpflichtungen", 
  "CO₂-Abscheidung und -Speicherung", "direkte Luftabscheidung"
)

adaptation_terms_de <- c(
  "Klimaanpassung", "Klimaresilienz", "Anpassungsfähigkeit", "Klimavulnerabilität",
  "Vulnerabilitätsbewertung", "klimaresiliente Entwicklung", "Klimasicherung",
  "Klimarisikomanagement", "Katastrophenrisikominderung", "Frühwarnsysteme",
  "widerstandsfähige Infrastruktur", "Küstenschutz", "Anpassung an den Meeresspiegelanstieg",
  "Hochwasserschutz", "Dürremanagement", "Hitzeresilienz", "städtische Anpassung",
  "klimaresiliente Landwirtschaft", "klimaresiliente Infrastruktur", "naturbasierte Lösungen",
  "ökosystembasierte Anpassung", "widerstandsfähige Ökosysteme", "resiliente Städte",
  "Klimaversicherung", "Klimarisikoversicherung", "Anpassungsfinanzierung", "Anpassungsförderung",
  "nationaler Anpassungsplan", "lokaler Anpassungsplan", "gemeindebasierte Anpassung",
  "Klimamigration", "klimabedingte Vertreibung", "geordneter Rückzug", "Klimaflüchtlinge",
  "Biodiversität", "Biodiversitätsverlust", "Ökosystemkollaps", "Ökosystemwiederherstellung",
  "Umweltzerstörung", "Entwaldung", "Wiederaufforstung", "Aufforstung",
  "Waldschutz", "Landdegradation", "Landnutzungsänderung", "Lebensraumzerstörung",
  "Wildtierschutz", "Meeresschutz", "Ozeanschutz", "Korallenbleiche",
  "Ozeanversauerung", "Plastikverschmutzung", "nachhaltige Forstwirtschaft", "Landrenaturierung",
  "Verluste und Schäden", "klimabedingte Verluste", "klimabedingte Schäden", "Entschädigung für Verluste und Schäden",
  "Klimareparationen", "Klimarisikoversicherung", "Finanzmechanismen für Klimaschäden",
  "Klimafinanzierung", "grüne Finanzierung", "Klimainvestition", "nachhaltige Investition",
  "Finanzierung nachhaltiger Entwicklung", "grüne Wirtschaft", "Kreislaufwirtschaft",
  "nachhaltige Lieferketten", "klimaresiliente Infrastruktur", "grünes Konjunkturprogramm",
  "nachhaltige Landwirtschaft", "klimaintelligente Landwirtschaft", "regenerative Landwirtschaft",
  "Ernährungssicherheit", "Wasserversorgungssicherheit", "Dürreresilienz", "hochwasserresistente Nutzpflanzen",
  "Agrarökologie", "Bodenschutz", "nachhaltige Fischerei", "Meerespolitik",
  "Wasserschutz", "klimaresiliente Landwirtschaft"
)

# 3. Normalise text
df_DE <- df_DE %>%
  mutate(text = stri_trans_general(text, "Latin-ASCII"),
         text = stri_trim_both(text),
         text_lower = tolower(text))

# 4. Fast vectorized term counting
count_terms_vectorized <- function(texts, terms) {
  rowSums(sapply(terms, function(term) stri_count_fixed(texts, tolower(term))))
} 

# 5. Score and label
df_DE <- df_DE %>%
  mutate(
    mitigation_score = count_terms_vectorized(text_lower, mitigation_terms_de),
    adaptation_score = count_terms_vectorized(text_lower, adaptation_terms_de),
    mitigation_norm = mitigation_score / (mitigation_score + adaptation_score + 0.001),
    adaptation_norm = adaptation_score / (mitigation_score + adaptation_score + 0.001),
    primary_focus = case_when(
      mitigation_score > 0 & adaptation_score == 0 ~ "Mitigation",
      adaptation_score > 0 & mitigation_score == 0 ~ "Adaptation",
      mitigation_score > 0 & adaptation_score > 0 ~ "Mixed",
      TRUE ~ "NA"
    )
  )

View(df_DE) # Oh my sweet baby jesus, this worked. 

table(df_DE$primary_focus) # Check to make sure this isn't scuppered. 

# Save
saveRDS(df_DE, "scored_DE.rds")

################################################################################

### Austria ### 

# 1. Load the filtered data 
df_AT <- readRDS("~/Downloads/Corp_Nationalrat_V2.rds") # Also used the unfiltered data here.  

# 2. Normalise text
df_AT <- df_AT %>%
  mutate(text = stri_trans_general(text, "Latin-ASCII"),
         text = stri_trim_both(text),
         text_lower = tolower(text))

# We can skip the load terms, because Austrians also speak German. 

# 3. Fast vectorised term counting
count_terms_vectorized <- function(texts, terms) {
  rowSums(sapply(terms, function(term) stri_count_fixed(texts, tolower(term))))
}

# 4. Score and label
df_AT <- df_AT %>%
  mutate(
    mitigation_score = count_terms_vectorized(text_lower, mitigation_terms_de),
    adaptation_score = count_terms_vectorized(text_lower, adaptation_terms_de),
    mitigation_norm = mitigation_score / (mitigation_score + adaptation_score + 0.001),
    adaptation_norm = adaptation_score / (mitigation_score + adaptation_score + 0.001),
    primary_focus = case_when(
      mitigation_score > 0 & adaptation_score == 0 ~ "Mitigation",
      adaptation_score > 0 & mitigation_score == 0 ~ "Adaptation",
      mitigation_score > 0 & adaptation_score > 0 ~ "Mixed",
      TRUE ~ "NA"
    )
  )

View(df_AT) # Looking good. 

table(df_AT$primary_focus)

# 5. Save EVERYTHING .
saveRDS(df_AT, "scored_AT.rds")


table(df$primary_focus)
table(df_NZ$primary_focus)
table(df_DE$primary_focus)
table(df_AT$primary_focus)