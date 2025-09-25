
library(tidyverse)
library(caret)
library(glmnet)
library(MASS)
library(lmtest) 
library(dplyr)

# no scientific notations
options(scipen = 999)

# Read data
stopifnot(file.exists("a1_data_group_9.csv"))
data <- readr::read_csv("a1_data_group_9.csv", show_col_types = FALSE)





# Task 1 

# model without Net trade in goods and services (BoP, current US$)

model_1 <- lm(`GDP growth (annual %)` ~ 
                   `Employers, total (% of total employment) (modeled ILO estimate)` +
                   `Fertility rate, total (births per woman)` +
                   `Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)` +
                   `Life expectancy at birth, male (years)` +
                   `Unemployment, total (% of total labor force) (modeled ILO estimate)` +
                   `Export value index (2000 = 100)` +
                   `Population growth (annual %)` +
                   `GDP per capita, PPP (constant 2011 international $)` +
                   `Wage and salaried workers, total (% of total employment) (modeled ILO estimate)` ,
                 data = data, singular.ok = FALSE
                 
)

summary(model_1)

vif(model_1) 

# drop fertility rate because VIF is > 5

model_1.5 <- lm(`GDP growth (annual %)` ~ 
                `Employers, total (% of total employment) (modeled ILO estimate)` +
                `Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)` +
                `Life expectancy at birth, male (years)` +
                `Unemployment, total (% of total labor force) (modeled ILO estimate)` +
                `Export value index (2000 = 100)` +
                `Population growth (annual %)` +
                `GDP per capita, PPP (constant 2011 international $)` +
                `Wage and salaried workers, total (% of total employment) (modeled ILO estimate)` ,
              data = data, singular.ok = FALSE
              
)

summary(model_1.5)

vif(model_1.5) 




# model with Net trade in goods and services (BoP, current US$)
model_2 <- lm(`GDP growth (annual %)` ~ 
                `Employers, total (% of total employment) (modeled ILO estimate)` +
                `Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)` +
                `Life expectancy at birth, male (years)` +
                `Net trade in goods and services (BoP, current US$)` +
                `Unemployment, total (% of total labor force) (modeled ILO estimate)` +
                `Export value index (2000 = 100)` +
                `Population growth (annual %)` +
                `GDP per capita, PPP (constant 2011 international $)` +
                `Wage and salaried workers, total (% of total employment) (modeled ILO estimate)` ,
              data = data, singular.ok = FALSE
              
)

vif(model_2)

summary(model_2)

# Task 2

# Create logged GDP per capita
data$log_GDP_per_capita <- log(data$`GDP per capita, PPP (constant 2011 international $)`)


# Run updated regression with log term
model_log <- lm(`GDP growth (annual %)` ~ 
                  `Employers, total (% of total employment) (modeled ILO estimate)` +
                  `Age dependency ratio (% of working-age population)` +
                  `Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)` +
                  `Life expectancy at birth, male (years)` +
                  `Net trade in goods and services (BoP, current US$)` +
                  `Unemployment, total (% of total labor force) (modeled ILO estimate)` +
                  `Export value index (2000 = 100)` +
                  `Population growth (annual %)` +
                  log_GDP_per_capita +           # add the non linear effects 
                  `Wage and salaried workers, total (% of total employment) (modeled ILO estimate)`,
                data = data, singular.ok = FALSE)

summary(model_log)



##### need to do diagnostic checks 
### Diagnostics for model_2 and model_log###

# non linearity
par(mfrow = c(1,2))
plot(model_2, which = 1, main = "Pre-transformed model")
plot(model_log, which = 1, main = "Post-transformed model")
par(mfrow = c(1,1))


# Task 3 run the lasso regression

# Prepare predictors (X) and outcome (y)
X <- model.matrix(
  `GDP growth (annual %)` ~ 
    `Employers, total (% of total employment) (modeled ILO estimate)` +
    `Age dependency ratio (% of working-age population)` +
    `Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)` +
    `Life expectancy at birth, male (years)` +
    `Net trade in goods and services (BoP, current US$)` +
    `Unemployment, total (% of total labor force) (modeled ILO estimate)` +
    `Export value index (2000 = 100)` +
    `Population growth (annual %)` +
    log(`GDP per capita, PPP (constant 2011 international $)`) +   # log transform inline
    `Wage and salaried workers, total (% of total employment) (modeled ILO estimate)`,
  data = data
)[,-1]   # drop intercept column (glmnet adds its own)

y <- data$`GDP growth (annual %)`


# Run lasso with cross-validation to find best lambda
cv_lasso <- cv.glmnet(X, y, alpha = 1, standardize = TRUE, lambda = 10^seq(-2, 5, length.out = 50), nfolds = 10)
plot(cv_lasso, xvar = "lambda", label = TRUE)

cv_lasso$lambda.min   # best λ
cv_lasso$lambda.1se   # 1-SE λ

coef(cv_lasso, s = "lambda.min")
coef(cv_lasso, s = "lambda.1se")


# Task 4


# Task 5 

# exclude country as it only serves as an indentifier
# wakala
df <- data[, !names(data) %in% "Country"]

# variables 

y <- as.vector(df$`GDP growth (annual %)`)
X <- model.matrix(`GDP growth (annual %)` ~ ., data = df)[, -1]

# Lasso Regression alpha = 1 with CV

cv_lasso_full <- cv.glmnet(X, y, alpha = 1, standardize = TRUE, lambda = 10^seq(-2, 5, length.out = 50), nfolds = 10)

plot(cv_lasso_full)


cv_lasso_caret <- train(
  `GDP growth (annual %)` ~ .,
  data = df,
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("center","scale"),
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0.00001, 1, length.out = 100))
)

cv_lasso_caret$bestTune
plot(cv_lasso_caret)




# install.packages(c("caret", "glmnet", "readr"))  # uncomment if needed

library(caret)
library(glmnet)
library(readr)

set.seed(123)  # reproducibility

# 1) Load data
df <- read_csv("a1_data_group_9.csv")

# 2) Define outcome and predictors
y <- df$`GDP growth (annual %)`
X <- df[ , !(names(df) %in% c("Country", "GDP growth (annual %)"))]

# Ensure all predictors are numeric (caret + glmnet require numeric matrix)
stopifnot(all(vapply(X, is.numeric, logical(1))))

# 3) Train-control: k-fold CV (change number if you like)
ctrl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = FALSE
)

# 4) LASSO via caret::train (alpha = 1)
# caret will standardize if we set preProcess = c("center", "scale")
lasso_fit <- train(
  x = X,
  y = y,
  method = "glmnet",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneGrid = expand.grid(
    alpha = 1,                    # LASSO
    lambda = exp(seq(log(1e-6), log(1e2), length.out = 300))
  ),
  metric = "RMSE"
)

# 5) Results
print(lasso_fit)
plot(lasso_fit)  # RMSE vs lambda

# Best tuning parameter (lambda)
lasso_fit$bestTune

# 6) Final model and coefficients at best lambda
final_glmnet <- lasso_fit$finalModel
best_lambda <- lasso_fit$bestTune$lambda

coefs <- coef(final_glmnet, s = best_lambda)
coefs

# Extract non-zero coefficients (selected features)
nz <- as.matrix(coefs)
nz <- nz[nz[,1] != 0, , drop = FALSE]
nz

# 7) Variable importance (from glmnet coefficients)
vip <- varImp(lasso_fit, scale = TRUE)
print(vip)
plot(vip, top = 20)



# Task 6




set.seed(123)  # for reproducibility
# total number of observations
n <- nrow(df)
# sample 110 indices for training
train_idx <- sample(1:n, 110, replace = FALSE)
# split X and y
X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test  <- X[-train_idx, ]
y_test  <- y[-train_idx]


















