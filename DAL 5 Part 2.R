# Title: DAL 5 part 2
# Author: Garrett Kent
# Email: gwkent@clemson.edu
# Date Created: 2021-03-12

# Purpose: Learn how to do chi-square tests on the GSS file

# Set Up ####
# libraries
library(tidyverse)
library(stringr)
library(infer)

# Data
load("/Users/garrettkent/Downloads/Clemson/POSC Research Methods/Kent-POSC-3410/DAL4/gss_df.Rdata")

# Theory: Support of science and technology --> views on space exploration.

# Theoretical Null Hypothesis: A person's views on science and technology









#Data Wrangling ####
analysis_df <- gss_df %>% 
  # Filter to keep the rows we want
  filter(YEAR == 2018 & !is.na(NEXTGEN) & !is.na(INTSPACE) & (NEXTGEN == "Agree" | NEXTGEN == "Strongly Agree" | NEXTGEN == "Strongly Disagree" | NEXTGEN == "Disagree") & (INTSPACE == "Moderately Interested" | INTSPACE=="Very interested" | INTSPACE == "Not at all interested")) %>% 
  #only keep variables we need
  select(INTSPACE, NEXTGEN, WTSSALL) %>% 
  group_by(NEXTGEN, INTSPACE) %>% 
  summarise(count=sum(WTSSALL))

# Convert strongly to agree or disagree, reaggregate
analysis_df <- analysis_df %>% 
  mutate(NEXTGEN = str_remove_all(NEXTGEN, "Strongly"),
         NEXTGEN = str_to_lower(NEXTGEN),
         NEXTGEN = str_trim(NEXTGEN, side = c("both"))) %>% 
  group_by(NEXTGEN, INTSPACE) %>% 
  summarise(count = sum(count))

# Reorder factor variable INTSPACE
analysis_df <- analysis_df %>% 
  mutate(INTSPACE = factor(INTSPACE))

#Add Data Represented INTSPACE == "Not at all interested"
add_rows <- tribble(
  ~NEXTGEN, ~INTSPACE, ~count,
  "agree", "Not at all interested", 1,
  "disagree", "Not at all interested"
)

analysis_df <- bind_rows(analysis_df, add_rows)


# Script ####

