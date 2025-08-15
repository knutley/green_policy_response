## Filtering Riksdag and Folketing Data## 
## Written by Katie Nutley from modified code by Elisa D'Amico (Cleaning & Preprocessing/Initial Filtering/climatefiltering_sample.R) 
## Created on: 22 April 2025
## ParlSpeech Data available https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/L4OAKN/PCYUNY&version=1.0

library(data.table)  # For fast data manipulation
library(stringr)  # For string matching
library(furrr)  # For parallel processing

################################################################################

## SWEDEN ## 

# Load Swedish data & filter date to 2000-2020
df <- readRDS("~/Downloads/Corp_Bundestag_V2.rds")
df$date <- as.Date(df$date)
df <- df[df$date >= "2000-01-01" & df$date <= "2020-12-31", ]