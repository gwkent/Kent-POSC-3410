# Title: DAL4 Part 1 
# Author: Garrett Kent
# Author's Email: gwkent@clemson.edu
# Date Created: 2021-02-02

# Purpose:

# Set Up####
# Libraries
library(tidyverse)

# Data 
# Load your Data and assign to gss_df ####
gss_df <- read_csv("/Users/garrettkent/Downloads/Clemson/POSC Research Methods/Kent-POSC-3410/DAL4/gss_df.csv")

# Script ####

# Question A ####
# View the data and read the code book to answer the following question. What do the columns and rows represent. Assign you answer as a text vector the below object. 

#view the code in a seperate window

#View(gss_df)

AnswerA <- "Columns represent variables, which are the questions respondents were asked on the GSS. Rows represent respondents."

# Question B ####
# What is the start and end year present in the data? Code as a NUMERIC vector and assign to AnswerB.  Plot how many observations per year there are. Assign your code to AnswerB_plot
summary(gss_df$YEAR)



AnswerB <- c(1972, 2018)
  
AnswerB_plot <- gss_df %>% 
  group_by(YEAR) %>% 
  count() %>% 
  rename(Observations = n, Year = YEAR) %>% 
  ggplot(aes(x= Year, y= Observations)) +
  geom_bar(stat = "identity")

#print AnswerB_plot
#AnswerB_plot

  

# Question C ####
# Is every question asked each year? Use the code book or the GSS Data Explorer (link posted on Canvas) to answer this question. Code your answer as a LOGICAL vector and assign to AnswerC. 

AnswerC <- FALSE

# Question D ####
# What are the names of all the variables in the data frame? Assign your answer to col.names. 
col.names<- names(gss_df)
  
# Question 1 ####
# Our ultimate goal is to understand what causes people to support federal funding for science. Our starting point for this inquiry should be to explore the dataset by developing a baseline understanding of the respondents and the sample. The most basic disaggregator in our society is gender. Make a plot of how many men and women respondents there in the sample overall. 

# When you make a plot that graphs number of respondents by sex, what do you expect to see. What ratio do you expect to see between men and women? Write your answer as a text vector and assign to Answer1_expectation. 

# Create Answer1_expectation
Answer1_expectation <- "Expect to create a bar graph with two variables. The x axis should be sex and Number should be y. Expect to see roughly the same amount of men and women."

# Create Answer1_plot
Answer1_plot<- gss_df %>% 
  group_by(SEX) %>% 
  count() %>% 
  rename(Number = n, Sex = SEX) %>% 
  ggplot(aes(x = Sex, y=Number)) +
    geom_bar(stat="identity")

#pring Answer1_plot
#Answer1_plot

# Use a text vector to explain what is peculiar about the result that you see. 
Answer1 <- "Much higher ratio of females to males than expected."


# Question 2 ####
# Repeat the steps we used in Question 1 for SEX to explore RACE 

# Create Answer2 Expectation 
Answer2_Expectation <- "Bar graph; X axis should be race and Number should be Y. Race should include white, black, and other. Will likely be close to majority white, roughly 15% black, less other."

# Create Answer2_plot
Answer2_plot <- gss_df %>% 
  group_by(RACE) %>% 
  count() %>% 
  rename(Number = n, Race = RACE) %>% 
  ggplot(aes(x = Race, y=Number)) +
  geom_bar(stat="identity")

#print Answer2_plot
Answer2_plot

# Use a text vector to explain what is peculiar about the result that you see. 
Answer2 <- "Race is about what we expected to see; roughly the same as in society."

# Question 3 ####
# When using GSS data, it is vital that we compare weighted percentages. In this question we will learn more about sampling weights. 

# View the structure of the WTSSALL variable. What is peculiar about the values of this variable. Describe your answer in a text vector and assign to Question3_part1. 
str(gss_df$WTSSALL)

Question3_part1 <- "All of the values are equivalent except for the first value, which is roughly half of the others."

# Find the description wtssall in the code book. What is this variable and why is it in the data? Describe your answer in a text vector and assign to Question3_part2.

Question3_part2 <- "This is the weight variable. It is included in order to adjust the data in a way that better represents the whole population."
 
# Important Takeaway #### 
# Our takeaway should be that we will need to compare the proportions. The GSS Team has surveyed a representative sample of Americans, which required oversampling some demographic groups to make sure there was enough data. The goal of this survey is NOT to take a Census of how many Americans in each demographic there are; rather, its goal is to understand the views and opinions Americans of each demographic group. 

# Therefore, analyzing data in the GSS will require us to find what percentage of a demographic group hold a certain opinion.

# Question 4 - Pointing out Ballot Difference ####
# Further, each respondent is also given a different ballot, which includes a different set of questions. You can read more about this in the Codebook or online. But we need to be aware that for each question in each year, we may have to filter to keep only the results for specific ballots. 

# How many different values are there for `BALLOT`?
Question4A <- unique(gss_df$BALLOT)

#call unique(gss_df$BALLOT)
#there are 5 different values for BALLOT.

# Let's say we want to understand which Ballots asked a specific question `ADVFRONT` (look up what it means in the codebook) in a specific year (2018), now let's use tidyverse to find this answer. 
gss_df %>% 
  filter(YEAR == 2018) %>% 
  group_by(BALLOT, ADVFRONT) %>% 
  count() %>% 
  View()

# Code a list of text vectors with the Ballots that asked this question. 
Question4B <- c("Ballot a, Ballot b")

# As you can see, the GSS lends itself to cross-sectional study but makes time-series study difficult because we have to develop a data frame for each question and variable we want to study and then combine these data frames together. We will learn how to do this next week. 

# Question 4 ####
# Here IS how we want to compare data in the GSS within 1 year. We will use the 2018 data. 

# 1) How many respondents were male and female (fill in the blanks)
gss_df %>% 
  filter(YEAR == 2018 & (BALLOT == "Ballot a" | BALLOT == "Ballot b")) %>% 
  group_by(SEX) %>% 
  count()

# 2A) Now create a dataframe of Male respondents who answered specific Ballots. 
Male_ab_df <-gss_df %>% 
  filter(YEAR == 2018 & SEX == "Male" & (BALLOT == "Ballot a" | BALLOT == "Ballot b"))

# 2B) Now create a dataframe of Female respondents who answered specific Ballots. 
Female_ab_df <-gss_df %>% 
  filter(YEAR == 2018 & SEX == "Female" & (BALLOT == "Ballot a" | BALLOT == "Ballot b"))

# 3A) Group_by SEX and ADVFRONT, summarize data to calculate proportion of men who responded each value
Male_ADVFRONT_df<- Male_ab_df %>% 
  group_by(SEX, ADVFRONT) %>%
  summarize(prop = n() / 691 * 100 )

#print Male_ADVFRONT_df
#Male_ADVFRONT_df

# 3B) Group_by SEX and ADVFRONT, summarize data to calculate proportion of women who responded each value
Female_ADVFRONT_df <- Female_ab_df %>% 
  group_by(SEX, ADVFRONT) %>%
  summarize(prop = n() / 868 * 100)

#print Female_ADVFRONT_df
#Female_ADVFRONT_df

# 4) Combine these dataframes using the bind_rows function 
ADVFRONT_by_SEX_df <-bind_rows(Male_ADVFRONT_df, Female_ADVFRONT_df)

# 5A) Create ggplot bar graph  
ADVFRONT_by_SEX_df %>%
  ggplot(aes(x = SEX, y= prop, fill= ADVFRONT)) +
  geom_bar(stat="identity")

# 5B) Filter out "Not Applicable" & "No answer" and make a ggplot. 
ADVFRONT_by_SEX_df %>% 
  filter(ADVFRONT != "Not applicable" & ADVFRONT != "No answer") %>% 
  ggplot(aes(x=SEX, y= prop, fill= ADVFRONT)) +
  geom_bar(stat="identity")

# 5C) What are some aspects of this visualization that are wonky? Describe in detail what you want to change about this graph in the future. Assing your answer to Answer5C 
Answer5C <- "The proportions do not add up to 100% now that I have filtered out the Not Applicable and No answer responses. This is because these numbers represent the proportions of people that chose each response out of ALL of the people, including those who selected Not Applicable or No answer. Ideally, I would like to adjust this graph so that the proportions of respondents that chose each answer add up perfectly to 100% without the Not Applicable and No answer responses. Essentially, I would be finding the proportions of respondents out of the combined number of respondents who chose Agree, Disagree, Dont Know, Strongly Agree, or Strongly Disagree. Most likely, I would need to filter the NA responses out somewhere earlier in the process in order to accomplish this."

# Copyright (c) Grant Allard, 2021
