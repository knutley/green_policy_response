## Cat Class Code for Sweden and Denmark
## Code by: Katelyn Nutley
## 15-08-2025

# Load Required Libraries
library(tidyverse)
library(dplyr)
library(stringi)
library(quanteda)
library(stm)
library(dplyr)
library(stringr)

##################################################################################
## SWEDEN ##

# 1. Load the filtered Swedish data 
df_swe <- filtered_swe_df
  # readRDS("~/Downloads/filtered_SWE.rds"); correct this so it's the actual file name

# 2. Define Mitigation and Adaptation Dictionaries
mitigation_terms_swe <- c("växthusgaser", "växthusgasutsläpp", "koldioxidutsläpp",
  "koldioxidavtryck", "koldioxidneutralitet", "kolbindning", "koldioxidkompensation",
  "kolsänka", "koldioxidprissättning", "koldioxidavgift", "koldioxidkredit", "handel med koldioxid",
  "metanutsläpp", "CO2-minskning", "netto noll", "koldioxidfri ekonomi", "koldioxidsnål ekonomi",
  "global uppvärmning", "klimatpolitik", "klimatbegränsning", "förnybar energi",
  "ren energi", "solenergi", "vindkraft", "vattenkraft", "geotermisk energi",
  "bioenergi", "tidvattenenergi", "vågenergi", "hållbar energi", "energiomställning",
  "grön energi", "koldioxidsnål energi", "lösningar utanför elnätet", "energieffektivitet",
  "ren teknik","grön teknik", "energibesparing", "utfasning av fossila bränslen",
  "utfasning av kol", "oljeberoende", "avyttring av fossila bränslen", "avkarbonisering",
  "övergången bort från fossila bränslen", "energiavkarbonisering", "ren transport",
  "elfordon", "gröna transporter", "väteekonomi", "koldioxidskatt",  "netto-noll åtaganden",
  "kolavskiljning och lagring", "direkt luftinfångning")

adaptation_terms_swe <- c("klimatanpassning", "klimattålighet", "anpassningsförmåga",
  "klimatsårbarhet", "sårbarhetsbedömning", "klimattålig utveckling", "klimatsäker",
  "klimatriskhantering", "katastrofriskminskning", "system för tidig varning",
  "motståndskraftig infrastruktur", "kustskydd", "anpassning till havsnivåhöjningen",
  "översvämningsskydd", "torkahantering", "värmetålighet", "stadsanpassning",
  "klimattåligt jordbruk", "klimattålig infrastruktur", "naturbaserade lösningar",
  "ekosystembaserad anpassning", "motståndskraftiga ekosystem", "motståndskraftiga städer",
  "klimatförsäkring", "klimatriskförsäkring", "anpassningsfinansiering",
  "nationell anpassningsplan", "lokal anpassningsplan", "samhällsbaserad anpassning",
  "klimatmigration", "klimatförskjutning", "planerad reträtt", "klimatflyktingar",
  "biologisk mångfald",  "förlust av biologisk mångfald",
  "ekosystemkollaps", "återställande av ekosystem", "miljöförstöring", "avskogning",
  "återplantering av skog", "skogsplantering", "skogsvård", "markförstöring",
  "förändrad markanvändning", "livsmiljöförstörelse", "naturvård", "marint skydd",
  "havsbevarande", "korallblekning", "havets försurning", "plastföroreningar",
  "hållbart skogsbruk", "markåterställning", "förlust och skada", "klimatinducerad förlust",
  "klimatinducerade skador", "ersättning för förlust och skada", "klimatkompensation",
  "finansiella mekanismer för klimatförlust", "klimatfinansiering", "grön finans",
  "klimatinvesteringar", "hållbara investeringar", "hållbar utvecklingsfinansiering",
  "grön ekonomi", "cirkulär ekonomi", "hållbara leveranskedjor", "grön stimulans",
  "hållbart jordbruk", "klimatsmart jordbruk", "regenerativt jordbruk",
  "livsmedelssäkerhet", "vattensäkerhet", "motståndskraft mot torka", "översvämningståliga grödor",
  "agroekologi", "markvård", "hållbart fiske", "havsförvaltning", "vattenbesparing")

# Function to count term occurrences in a text
count_term_occurrences <- function(text, terms) {
  text_lower <- tolower(text)
  sum(sapply(terms, function(term) str_count(text_lower, fixed(term))))
}

# 3. Normalise text
# function to clean scandi text (I think I might be losing a lot of stuff here)
improve_text_cleaning <- function(df) {
  df %>%
    mutate(
      # 1. Handle encoding issues first
      text = iconv(text, from = "UTF-8", to = "UTF-8", sub = ""),
      
      # 2. Normalize whitespace and remove extra spaces
      text = str_squish(text),
      
      # 3. Remove URLs, email addresses, and special characters that add noise
      text = str_remove_all(text, "http\\S+|www\\S+"),
      text = str_remove_all(text, "\\S+@\\S+\\.\\S+"),
      
      # 4. Handle hyphenated words (common in Swedish/Danish compound words)
      # Keep climate-related hyphens intact but normalize others
      text = str_replace_all(text, "(?<!klimat|miljö|koldioxid|energi|växthusgass|drivhusgass)-(\\s)", " "),
      
      # 5. Normalize punctuation - replace multiple punctuation with single space
      text = str_replace_all(text, "[[:punct:]]+", " "),
      
      # 6. Convert to lowercase BEFORE removing accents to preserve meaning
      text_lower = tolower(text),
      
      # 7. MORE CAREFUL accent handling - preserve meaning
      # Instead of removing ALL accents, only normalize common variations
      text_lower = str_replace_all(text_lower, "å", "a"),
      text_lower = str_replace_all(text_lower, "ä", "a"),  
      text_lower = str_replace_all(text_lower, "ö", "o"),
      text_lower = str_replace_all(text_lower, "ø", "o"),
      text_lower = str_replace_all(text_lower, "æ", "ae"),
      
      # 8. Handle common abbreviations and expand them
      text_lower = str_replace_all(text_lower, "\\bco2\\b", "koldioxid"),
      text_lower = str_replace_all(text_lower, "\\beu\\b", "europeiska unionen"),
      text_lower = str_replace_all(text_lower, "\\bfn\\b", "förenta nationerna"),
      text_lower = str_replace_all(text_lower, "\\bunfccc\\b", "klimatkonvention"),
      
      # 9. Normalize compound words (Swedish/Danish often use compounds)
      text_lower = str_replace_all(text_lower, "klimat\\s+politik", "klimatpolitik"),
      text_lower = str_replace_all(text_lower, "miljö\\s+politik", "miljöpolitik"),
      text_lower = str_replace_all(text_lower, "energi\\s+effektivitet", "energieffektivitet"),
      
      # 10. Final cleanup - remove extra whitespace
      text_lower = str_squish(text_lower),
      
      # 11. Create a version for exact matching (preserve original case patterns)
      text_original = text
    )
}

df_swe<- improve_text_cleaning(df_swe)

# 4. Fast vectorized term counting
count_terms_vectorized <- function(texts, terms) {
  rowSums(sapply(terms, function(term) stri_count_fixed(texts, tolower(term))))
} 

# 5. Score and label
df_swe <- df_swe %>%
  mutate(
    mitigation_score = count_terms_vectorized(text_lower, mitigation_terms_swe),
    adaptation_score = count_terms_vectorized(text_lower, adaptation_terms_swe),
    mitigation_norm = mitigation_score / (mitigation_score + adaptation_score + 0.001),
    adaptation_norm = adaptation_score / (mitigation_score + adaptation_score + 0.001),
    primary_focus = case_when(
      mitigation_score > 0 & adaptation_score == 0 ~ "Mitigation",
      adaptation_score > 0 & mitigation_score == 0 ~ "Adaptation",
      mitigation_score > 0 & adaptation_score > 0 ~ "Mixed",
      TRUE ~ "NA"
    )
  )

View(df_swe) 

table(df_swe$primary_focus) # Check to make sure this isn't scuppered. 

# 7. Save the Scored Data 

saveRDS(df_swe, "scored_swe.rds") 

##################################################################################
## Denmark ##

# 1. Load the filtered Danish data 
df_dk <- filtered_dk_df
# readRDS("~/Downloads/filtered_DK.rds"); correct this so it's the actual file name

# 3. Define Mitigation and Adaptation Dictionaries
mitigation_terms_dk <- c ("drivhusgasser", "drivhusgasemissioner", "kulstofemissioner",
"CO2-aftryk", "kulstofneutralitet", "kulstofbinding", "kulstofkompensation", "kulstofvask",
"kulstofpriser", "kulstofafgift", "kulstofkredit", "handel med kulstof", "metan-emissioner",
"CO2-reduktion", "netto nul", "kulstoffri økonomi", "lavkulstoføkonomi", "global opvarmning",
"klimaafbødningspolitik", "afbødning af klimaet", "vedvarende energi", "ren energi",
"solenergi", "vindkraft", "vandkraft", "geotermisk energi", "bioenergi", "tidevandsenergi",                 
"bølgeenergi", "bæredygtig energi", "energiomstilling", "grøn energi", "kulstoffattig energi",
"off-grid løsninger", "energieffektivitet", "ren teknologi", "grøn teknologi",
"energibesparelse", "udfasning af fossile brændstoffer", "kuludfasning", "olieafhængighed",
"frasalg af fossile brændstoffer", "dekarbonisering", "overgangen væk fra fossile brændstoffer",
"energidekarbonisering", "ren transport", "elektriske køretøjer", "grøn transport",
"brintøkonomi", "kulstofafgift", "netto-nul forpligtelser", "kulstoffangst og -lagring",
"direkte luftindfangning")

adaptation_terms_dk <- c("klimavenlig landbrug", "klimaresiliens", "tilpasningsevne",
"klimasårbarhed", "sårbarhedsvurdering", "klimarobust udvikling", "klimasikre",
"styring af klimarisiko", "katastroferisikoreduktion", "tidlige varslingssystemer",
"robust infrastruktur", "kystsikring", "tilpasning til havniveaustigningen",
"beskyttelse mod oversvømmelse", "tørkehåndtering", "varmebestandighed", "bytilpasning",
"klimarobust landbrug", "klimarobust infrastruktur", "naturbaserede løsninger",
"økosystembaseret tilpasning", "modstandsdygtige økosystemer", "modstandsdygtige byer",
"klimaforsikring", "klimarisikoforsikring", "tilpasningsfinansiering", "tilpasningsmidler",
"national tilpasningsplan", "lokal tilpasningsplan", "lokalsamfundsbaseret tilpasning",
"klimamigration", "klimaforskydning", "planlagt tilbagetog", "klimaflygtninge",
"biodiversitet", "tab af biodiversitet", "økosystemsammenbrud", "genopretning af økosystemer",
"miljøforringelse", "skovrydning", "genplantning af skov", "skovrejsning", "skovbevarelse",
"jordforringelse", "ændring af arealanvendelsen", "ødelæggelse af levesteder",
"bevarelse af vilde dyr", "havbeskyttelse", "havbevaring", "koralblegning", "havforsuring",
"plastikforurening", "bæredygtigt skovbrug", "jordgenopretning", "tab og skade",
"klimabetinget tab", "klimabetingede skader", "erstatning for tab og skade",
"klimakompensation", "finansielle mekanismer for klimatab", "klimafinansiering",
"grøn finans", "klimainvesteringer", "bæredygtig investering", "finansiering af bæredygtig udvikling",
"grøn økonomi", "cirkulær økonomi", "bæredygtige forsyningskæder", "grøn stimulus",
"bæredygtigt landbrug", "klimasmart landbrug", "regenerativt landbrug", "fødevaresikkerhed",
"vandsikkerhed", "modstandsdygtighed over for tørke", "oversvømmelsesresistente afgrøder",
"agroøkologi", "jordbevarelse", "bæredygtigt fiskeri", "havstyring", "vandbesparelse")

# 3. Normalise text
df_dk<- improve_text_cleaning(df_dk)


# 5. Score and label
df_dk <- df_dk %>%
  mutate(
    mitigation_score = count_terms_vectorized(text_lower, mitigation_terms_dk),
    adaptation_score = count_terms_vectorized(text_lower, adaptation_terms_dk),
    mitigation_norm = mitigation_score / (mitigation_score + adaptation_score + 0.001),
    adaptation_norm = adaptation_score / (mitigation_score + adaptation_score + 0.001),
    primary_focus = case_when(
      mitigation_score > 0 & adaptation_score == 0 ~ "Mitigation",
      adaptation_score > 0 & mitigation_score == 0 ~ "Adaptation",
      mitigation_score > 0 & adaptation_score > 0 ~ "Mixed",
      TRUE ~ "NA"
    )
  )

View(df_dk) 

table(df_dk$primary_focus)

# 7. Save the Scored Data 

saveRDS(df_dk, "scored_dk.rds")
