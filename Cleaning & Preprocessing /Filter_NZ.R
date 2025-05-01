## Filtering  for New Zealand Parliament ## 
## Written by Katie Nutley from modified code by Elisa D'Amico (Cleaning & Preprocessing/Initial Filtering/climatefiltering_sample.R) 
## Created on: 01 May 2025
## ParlSpeech Data available https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/L4OAKN/LLMYON&version=1.0

library(data.table)  # For fast data manipulation
library(stringr)  # For string matching
library(furrr)  # For parallel processing

# Load data & filter date to 2000-2020
df <- readRDS("~/Downloads/Corp_NZHoR_V2.rds")
df$date <- as.Date(df$date)
df <- df[df$date >= "2000-01-01" & df$date <= "2020-12-31", ]

keywords <- c(
  # General Climate Change and Environment
  "climate change", "global warming", "climate crisis", "climate emergency", "climate breakdown",
  "climate disruption", "climate instability", "climate catastrophe", "climatic shifts",
  "climate variability", "climate science", "changing climate", "anthropogenic climate change",
  "climate-induced change", "climate-related risks",
  
  # Greenhouse Gases and Emissions
  "greenhouse gases", "GHG emissions", "carbon emissions", "carbon footprint",
  "carbon neutrality", "carbon sequestration", "carbon offsetting", "carbon sink",
  "carbon pricing", "carbon levy", "carbon credit", "carbon trading", "methane emissions",
  "CO2 reduction", "net zero", "zero-carbon economy", "low-carbon economy",
  
  # Climate Policy and Governance
  "climate action", "climate policy", "climate governance", "climate adaptation policy",
  "climate mitigation policy", "environmental policy", "climate diplomacy",
  "international climate agreements", "climate negotiations", "environmental treaties",
  "UNFCCC", "Paris Agreement", "COP", "Kyoto Protocol", "IPCC", "climate pledges",
  "nationally determined contributions", "climate targets", "sustainable policies",
  
  # Adaptation and Mitigation
  "climate adaptation", "climate resilience", "resilience planning", "climate-proofing",
  "climate mitigation", "disaster preparedness", "disaster risk reduction", "DRR",
  "disaster resilience", "risk reduction strategies", "risk adaptation",
  "ecosystem-based adaptation", "nature-based solutions", "adaptive capacity",
  "community-based adaptation", "vulnerability reduction",
  
  # Renewable Energy and Clean Tech
  "renewable energy", "clean energy", "solar power", "wind power", "hydropower",
  "geothermal energy", "bioenergy", "tidal energy", "wave energy", "sustainable energy",
  "energy transition", "green energy", "low-carbon energy", "off-grid solutions",
  "energy efficiency", "clean technology", "green technology", "energy conservation",
  
  # Fossil Fuels and Decarbonization
  "fossil fuel phase-out", "coal phase-out", "oil dependency", "fossil fuel divestment",
  "decarbonization", "transition away from fossil fuels", "energy decarbonization",
  "clean transportation", "electric vehicles", "green transportation", "hydrogen economy",
  
  # Biodiversity, Conservation, and Land Use
  "biodiversity", "biodiversity loss", "ecosystem collapse", "ecosystem restoration",
  "environmental degradation", "deforestation", "reforestation", "afforestation",
  "forest conservation", "land degradation", "land-use change", "habitat destruction",
  "wildlife conservation", "marine protection", "ocean conservation", "coral bleaching",
  "ocean acidification", "plastic pollution", "sustainable forestry", "land restoration",
  
  # Loss and Damage
  "loss and damage", "climate-induced loss", "climate-induced damage", "compensation for loss and damage",
  "climate reparations", "climate risk insurance", "financial mechanisms for climate loss",
  
  # Climate Finance and Economic Policies
  "climate finance", "green finance", "climate investment", "sustainable investment",
  "green bonds", "carbon tax", "climate funds", "financial instruments for climate action",
  "cap and trade", "emissions trading scheme", "ETS", "environmental taxation",
  "sustainable development finance", "green economy", "circular economy",
  "sustainable supply chains", "climate-resilient infrastructure", "green stimulus",
  
  # Extreme Weather and Disasters
  "natural disaster", "climate disaster", "environmental disaster", "heatwaves",
  "drought mitigation", "wildfire prevention", "flood management", "storm surges",
  "sea level rise", "extreme weather", "hurricane resilience", "typhoon preparedness",
  "tornado risk management", "monsoon variability", "weather-related displacement",
  
  # Climate-Induced Migration and Social Impacts
  "climate refugees", "environmental migrants", "displacement due to climate change",
  "forced migration", "climate-driven displacement", "climate justice", "climate equity",
  "environmental justice", "just transition", "fair transition", "climate rights",
  
  # Agriculture, Water Security, and Food Systems
  "sustainable agriculture", "climate-smart agriculture", "regenerative agriculture",
  "food security", "water security", "drought resilience", "flood-resilient crops",
  "agroecology", "soil conservation", "sustainable fisheries", "ocean governance",
  "water conservation", "climate-resilient farming",
  
  # Environmental Regulations and Legal Frameworks
  "environmental regulations", "green laws", "climate litigation", "climate lawsuits",
  "environmental protection laws", "polluter pays principle", "environmental liability",
  "corporate environmental responsibility", "net-zero commitments", "legally binding climate targets",
  
  # Activism, Movements, and Awareness
  "climate activism", "environmental activism", "youth climate movement",
  "grassroots climate action", "civil society climate engagement", "climate protests",
  "climate advocacy", "public awareness on climate change", "media coverage of climate",
  
  # Innovation and Future Technologies
  "climate innovation", "carbon capture and storage", "direct air capture",
  "geoengineering", "solar radiation management", "climate tech", "climate-positive solutions",
  "blue carbon", "negative emissions technology", "climate monitoring systems",
  
  # Miscellaneous Key Phrases
  "environmental responsibility", "green transition", "sustainability strategies",
  "ecological balance", "natural capital", "regenerative practices", "planetary boundaries",
  "environmental stewardship", "ecological footprint", "environmental risk management"
)

# Parallel processing
plan(multisession, workers = availableCores() - 1)

# Word and phrase filtering function
contains_keyword <- function(text, keywords) {
  str_detect(text, regex(str_c(keywords, collapse = "|"), ignore_case = TRUE))
}

# Run filtering in parallel and keep only matching rows
df$match <- future_map_lgl(df$text, ~contains_keyword(.x, keywords))
filtered_df <- df[df$match == TRUE, ]
filtered_df$match <- NULL

# Export filtered data
saveRDS(filtered_df, "filtered_NZ.rds")
