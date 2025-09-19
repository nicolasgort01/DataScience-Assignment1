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

df_processed <- df %>% 
  select(`GDP growth (annual %)`,`Export value index (2000 = 100)`, `Fertility rate, total (births per woman)`,
         `Individuals using the Internet (% of population)`, `Labor force, total`, `Population, total`,
         `Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate)`, `Urban population (% of total)`,
         `Net trade in goods and services (BoP, current US$)`)
# Task: Run a regression model using the variables you deem relevant from the dataset and include Net trade
# in goods and services (BoP, current US$) to explain GDP growth (annual %).

fit1 <- lm(`GDP growth (annual %)` ~ ., df_processed, singular.ok = FALSE.1 )
summary(fit1)
colnames(df)

X <- model.matrix(`GDP growth (annual %)` ~ ., data = df_processed, singular.ok = FALSE)
y <- df_processed$`GDP growth (annual %)`
fit1 <- lm.fit(X, y)
coefficients(fit1)
