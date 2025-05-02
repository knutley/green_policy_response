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

# 1. Load the filtered data 
df_DE <- readRDS("~/Documents/GitHub/green_policy_response/filtered_DE.rds") 

# 2. Set Seed for Reproducability 
set.seed(123)

# 3. Define Mitigation and Adaptation Dictionaries
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

# 4. Create Document-Feature Matrix with Preprocessing
# Create corpus
DE_corpus <- quanteda::corpus(df_DE$text, docnames = df_DE$doc_id)

# Create German custom stopwords
DE_custom_stopwords_vector <- c(stopwords("de"), 
                                'auch', 'oder', 'bzw', 'sowie', 'damit', 'dazu', 
                                'dabei', 'hierbei', 'dafür', 'dadurch', 'somit',
                                'jedoch', 'dennoch', 'allerdings', 'hingegen',
                                'zudem', 'außerdem', 'ebenso', 'weiterhin',
                                'übrigens', 'schließlich', 'letztlich', 'eigentlich',
                                'natürlich', 'tatsächlich', 'wirklich', 'eben',
                                'halt', 'mal', 'gar', 'ganz', 'sehr', 'eher',
                                'etwa', 'circa', 'ca', 'usw', 'etc', 'bspw', 'z.b')

# Process tokens
DE_tokens <- DE_corpus %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(DE_custom_stopwords_vector) %>%
  tokens_wordstem()

# Create n-grams
DE_tokens_ngrams <- tokens_ngrams(DE_tokens, n = c(1, 2))

# Create DFM
DE_dfm_speeches_corpus <- DE_tokens_ngrams %>%
  dfm() %>%
  dfm_tolower() %>%
  dfm_trim(min_termfreq = 10, min_docfreq = 0.005, docfreq_type = "prop")

# 5. Create Category Variables 

# Function to count term occurrences in a text
count_term_occurrences <- function(text, terms) {
  text_lower <- tolower(text)
  sum(sapply(terms, function(term) str_count(text_lower, fixed(term))))
}

# Add mitigation and adaptation scores to dataframe
df_DE <- df_DE %>%
  mutate(
    mitigation_score = sapply(text, function(t) count_term_occurrences(t, mitigation_terms_de)),
    adaptation_score = sapply(text, function(t) count_term_occurrences(t, adaptation_terms_de)),
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

table(df_DE$primary_focus) #Right, there's an issue here in that it's not finding 
# any of the mitigate/adapt terms. 

df_DE <- df_DE %>%
  mutate(
    mitigation_score = sapply(text, function(t) count_term_occurrences(t, mitigation_terms_de)),
    adaptation_score = sapply(text, function(t) count_term_occurrences(t, adaptation_terms_de)),
    # Create categorical variables
    primary_focus = case_when(
      mitigation_score > 0 & adaptation_score == 0 ~ "Mitigation",
      adaptation_score > 0 & mitigation_score == 0 ~ "Adaptation",
      mitigation_score > 0 & adaptation_score > 0 ~ "Mixed",
      TRUE ~ "NA"
    )
  )

View(df_DE) 
table(df_DE$primary_focus)

# 6. Save the Scored Data 

saveRDS(df_DE, "scored_DE.rds") 

################################################################################

### Austria ### 

# 1. Load the filtered data 
df_AT <- readRDS("~/Documents/GitHub/green_policy_response/filtered_AT.rds") 

# 2. Set Seed for Reproducability 
set.seed(123)

# 3. Create Document-Feature Matrix with Preprocessing

# Create corpus
AT_corpus <- quanteda::corpus(df_AT$text, docnames = df_AT$doc_id)

# Create German custom stopwords
AT_custom_stopwords_vector <- c(stopwords("de"), 
                                'auch', 'oder', 'bzw', 'sowie', 'damit', 'dazu', 
                                'dabei', 'hierbei', 'dafür', 'dadurch', 'somit',
                                'jedoch', 'dennoch', 'allerdings', 'hingegen',
                                'zudem', 'außerdem', 'ebenso', 'weiterhin',
                                'übrigens', 'schließlich', 'letztlich', 'eigentlich',
                                'natürlich', 'tatsächlich', 'wirklich', 'eben',
                                'halt', 'mal', 'gar', 'ganz', 'sehr', 'eher',
                                'etwa', 'circa', 'ca', 'usw', 'etc', 'bspw', 'z.b')


# Process tokens
AT_tokens <- AT_corpus %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(AT_custom_stopwords_vector) %>%
  tokens_wordstem()

# Create n-grams
AT_tokens_ngrams <- tokens_ngrams(AT_tokens, n = c(1, 2))

# Create DFM
AT_dfm_speeches_corpus <- AT_tokens_ngrams %>%
  dfm() %>%
  dfm_tolower() %>%
  dfm_trim(min_termfreq = 10, min_docfreq = 0.005, docfreq_type = "prop")

# 5. Create Category Variables 

# Function to count term occurrences in a text
count_term_occurrences <- function(text, terms) {
  text_lower <- tolower(text)
  sum(sapply(terms, function(term) str_count(text_lower, fixed(term))))
}

# Add mitigation and adaptation scores to dataframe
df_AT <- df_AT %>%
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
View(df_AT) 

df_AT <- df_AT %>%
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

View(df_AT) 
table(df_AT$primary_focus)

# 6. Save the Scored Data 

saveRDS(df_AT, "scored_AT.rds") 



