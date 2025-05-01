## Filtering Bundestag and Nationalrat Data ## 
## Written by Katie Nutley from modified code by Elisa D'Amico (Cleaning & Preprocessing/Initial Filtering/climatefiltering_sample.R) 
## Created on: 22 April 2025
## ParlSpeech Data available https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/L4OAKN/PCYUNY&version=1.0

library(data.table)  # For fast data manipulation
library(stringr)  # For string matching
library(furrr)  # For parallel processing

# Load data & filter date to 2000-2020
df <- readRDS("~/Downloads/Corp_Bundestag_V2.rds")
df$date <- as.Date(df$date)
df <- df[df$date >= "2000-01-01" & df$date <= "2020-12-31", ]

de_keywords <- c( 
  
  # General Climate Change and Environment
  "klimawandel", "globale erwärmung", "klimakrise", "klimanotstand", "klimazusammenbruch", 
  "klimastörung", "klimainstabilität", "klimakatastrophe", "klimaveränderungen", 
  "klimavariabilität", "klimawissenschaft", "anthropogener klimawandel", 
  "klimabedingter wandel", "klimarisiken", 
  
  # Greenhouse Gases and Emissions
  "treibhausgase", "treibhausgasemissionen", "CO2-emissionen", "kohlenstoff-fußabdruck", 
  "klimaneutralität", "kohlenstoffbindung", "CO2-kompensation", "kohlenstoffsenke",
  "CO2-preis", "CO2-abgabe", "kohlenstoffgutschriften", "kohlenstoffhandel", "methanemissionen",
  "CO2-reduktion", "netto-null", "kohlenstofffreie wirtschaft", "klimaneutrale wirtschaft", 
  "emissionsfreie wirtschaft",
  
  # Climate Policy and Governance
  "klimaschutz", "klimapolitik", "klimagovernance", "politik zur klimaanpassung", 
  "klimaschutzpolitik", "umweltpolitik", "klimadiplomatie",
  "internationale klimaabkommen", "klimaverhandlungen", "umweltverträge",
  "UNFCCC", "Pariser Abkommen", "COP", "Kyoto-Protokoll", "IPCC", "klimaschutz-zusagen",
  "national festgelegte beiträge", "NDCs", "klimaziele", "nachhaltige politikgestaltung", 
  
  # Adaptation and Mitigation
  "klimaanpassung", "klimaresilienz", "resilienzplanung", "klimaschutz",
  "katastrophenvorsorge", "katastrophenrisikominderung", "katastrophenrisikominderung", 
  "katastrophenresilienz", "strategien zur risikominderung", "anpassung an risiken", "risikoadjustierung",
  "ökosystembasierte anpassung", "naturbasierte lösungen", "anpassungsfähigkeit",
  "gemeindebasierte anpassung", "verletzlichkeitsminderung", "klimafeste planung",
  
  # Renewable Energy and Clean Tech
  "erneuerbare energien", "saubere energie", "solarenergie", "windkraft", "wasserkraft",
  "geothermie", "bioenergie", "gezeitenenergie", "wellenenergie", "nachhaltige energie",
  "energiewende", "grüne energie", "CO2-arme energie", "inselnetzlösungen",
  "energieeffizienz", "saubere technologie", "grüne technologie", "energieeinsparen",
  
  # Fossil Fuels and Decarbonization
  "ausstieg aus fossilen brennstoffen", "kohleausstieg", "Ölabhängigkeit", "desinvestitionen aus fossilen brennstoffen", 
  "dekarbonisierung", "abkehr von fossilen brennstoffen", "energiedekarbonisierung",
  "emissionsarmer verkehr", "elektrofahrzeuge", "nachhaltiger verkehr", "klimafreundlicher verkehr", "wasserstoffwirtschaft",
  
  # Biodiversity, Conservation, and Land Use
  "biodiversität", "biodiversitätsverlust", "Ökosystemkollaps", "Ökosystemwiederherstellung",
  "umweltzerstörung", "abholzung", "wiederaufforstung", "aufforstung",
  "waldschutz", "landdegradierung", "landnutzungsänderung", "lebensraumzerstörung",
  "naturschutz", "meeresschutz", "ozeanschutz", "korallenbleiche",
  "ozeanversauerung", "plastikverschmutzung", "nachhaltige forstwirtschaft", "renaturierung",
  
  # Loss and Damage
  "verluste und schäden", "klimabedingte verluste", "klimabedingte schäden", "entschädigung für verluste und schäden",
  "klimareparationen", "klimarisikoversicherung", "finanzierungsmechanismen für verluste und schäden",
  
  # Climate Finance and Economic Policies
  "klimafinanzierung", "grüne finanzierung", "klimainvestitionen", "nachhaltige investitionen",
  "grüne anleihen", "kohlenstoffsteuer", "klimafonds", "finanzierungsinstrumente für den klimaschutz",
  "cap-and-trade", "emissionshandelssystem", "EHS", "umweltsteuern",
  "nachhaltige entwicklungsfinanzierung", "grüne Wirtschaft", "kreislaufwirtschaft",
  "nachhaltige lieferketten", "klimaresiliente infrastruktur", "grüne konjunkturimpulse",
  
  # Extreme Weather and Disasters
  "naturkatastrophe", "klimakatastrophe", "umweltkatastrophe", "hitzewellen",
  "dürrevorsorge", "waldbrandprävention", "hochwassermanagement", "sturmfluten",
  "anstieg des meeresspiegels", "extremwetter", "hurrikan-resilienz", "taifun-vorsorge",
  "tornado-risikomanagement", "monsunvariabilität", "wetterbedingte vertreibung",  
  
  # Climate-Induced Migration and Social Impacts
  "klimaflüchtlinge", "umweltmigranten", "vertreibung durch den klimawandel",
  "zwangsmigration", "klimabedingte vertreibung", "klimagerechtigkeit",
  "umweltgerechtigkeit", "gerechter Übergang", "fairer Übergang", "klimarechte",
  
  # Agriculture, Water Security, and Food Systems
  "nachhaltige landwirtschaft", "klimafreundliche landwirtschaft", "regenerative landwirtschaft",
  "ernährungssicherheit", "wassersicherheit", "dürreresilienz", "hochwasserresistente nutzpflanzen",
  "agrarökologie", "bodenschutz", "nachhaltige fischerei", "meeresmanagement", "meeresgovernance",
  "wasserschutz", "klimaresiliente landwirtschaft",
  
  # Environmental Regulations and Legal Frameworks
  "umweltvorschriften", "grüne gesetze", "klimaklagen",
  "umweltschutzgesetze", "verursacherprinzip", "umwelthaftung",
  "unternehmensverantwortung für die umwelt", "netto-null-verpflichtungen", "rechtlich verbindliche klimaziele",
  
  # Activism, Movements, and Awareness
  "klimaaktivismus", "umweltaktivismus", "jugendklimabewegung", 
  "klimaaktionen an der basis", "zivilgesellschaftliches klimaengagement", "klimaproteste",
  "klima-advocacy", "öffentliches bewusstsein für den klimawandel", "medienberichterstattung zum thema klima",
  
  # Innovation and Future Technologies
  "klimainnovation", "kohlenstoffabscheidung und -speicherung", "direkte luftabscheidung",
  "geoengineering", "sonnenstrahlungsmanagement", "klimatechnologie", "klimapositive lösungen",
  "blauer kohlenstoff", "negative emissionstechnologie", "klimaüberwachungssysteme",
  
  # Miscellaneous Key Phrases
  "umweltverantwortung", "grüner wandel", "nachhaltigkeitsstrategien",
  "Ökologisches gleichgewicht", "naturkapital", "regenerative praktiken", "planetarische grenzen",
  "Ökologischer fußabdruck", "umweltrisikomanagement"
)

# Parallel processing
plan(multisession, workers = availableCores() - 1)

# Word and phrase filtering function
contains_keyword <- function(text, de_keywords) {
  str_detect(text, regex(str_c(de_keywords, collapse = "|"), ignore_case = TRUE))
}

# Run filtering in parallel and keep only matching rows
df$match <- future_map_lgl(df$text, ~contains_keyword(.x, de_keywords))
filtered_df <- df[df$match == TRUE, ]
filtered_df$match <- NULL

# Export filtered data
saveRDS(filtered_df, "filtered_DE.rds")

# Now Austria... 

# Load data & filter date to 2000-2020
df1 <- readRDS("~/Downloads/Corp_Nationalrat_V2.rds")
df1$date <- as.Date(df1$date)
df1 <- df1[df1$date >= "2000-01-01" & df1$date <= "2020-12-31", ]

# Parallel processing
plan(multisession, workers = availableCores() - 1)

# Word and phrase filtering function
contains_keyword <- function(text, de_keywords) {
  str_detect(text, regex(str_c(de_keywords, collapse = "|"), ignore_case = TRUE))
}

# Run filtering in parallel and keep only matching rows
df1$match <- future_map_lgl(df1$text, ~contains_keyword(.x, de_keywords))
filtered_df1 <- df1[df1$match == TRUE, ]
filtered_df1$match <- NULL

# Export filtered data
saveRDS(filtered_df1, "filtered_AT.rds")



