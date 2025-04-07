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
  summarise(count = n(), avg_ntoken = mean(ntoken, na.rm = TRUE))

df %>%
  mutate(ntoken = stringr::str_count(text, "\\S+")) %>%  # Count words in text
  group_by(party) %>%
  summarise(count = n(), avg_ntoken = mean(ntoken, na.rm = TRUE))

# Note: These summary statistics show the actual count of speeches in the UK 
# filtered corpus (that is speeches which mention climate change and its ill
# effects to )

