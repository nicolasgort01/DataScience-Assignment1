# Assignment 1.

## Preliminaries

library(rio)
library(tidyverse)

rm(list = ls())
df <- import('Data/a1_data_group_9.csv')

# Take a look at the data. 

colnames(df)

colSums(is.na(df))

# Data is complete, no missing values. 

# Data processing. 

df <- df %>% 
  rename(`GDP growth (annual %)`)
# Task: Run a regression model using the variables you deem relevant from the dataset and include Net trade
# in goods and services (BoP, current US$) to explain GDP growth (annual %).

fit1 <- lm(`GDP growth (annual %)`, df)

colnames(df)
