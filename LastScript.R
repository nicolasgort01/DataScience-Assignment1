# Preliminaries. 

library(rio)
library(tidyverse)
library(caret)
library(lmtest)
library(car)
library(glmnet)
library(knitr)
library(kableExtra)
getwd()

rm(list = ls())
df <- import('Data/a1_data_group_9.csv')

getwd()

options(scipen = 999)

# This is from Q6. 

# Data Splitting. 
set.seed(254646)
n1 <- 110
idx <- sample(1:nrow(df), n1)

data_clean <- df

dfm_train <- data_clean[idx, ]

dfm_test <- data_clean[-idx, ]  

colnames(data_clean)

Xvars_mat_train <- model.matrix(`GDP growth (annual %)` ~.-Country,data = dfm_train)[, -1]
Yvar_train      <- dfm_train$`GDP growth (annual %)`           

Xvars_mat_test  <- model.matrix(`GDP growth (annual %)` ~.-Country,data = dfm_test)[, -1]
Yvar_test       <- dfm_test$`GDP growth (annual %)`    

lambda_grid <- seq(0.001, 2, length.out = 100)

cv_lasso <- cv.glmnet(
  Xvars_mat_train, Yvar_train,
  family = "gaussian",
  alpha = 1,                        # LASSO
  lambda = lambda_grid,             # grid explícito
  nfolds = 10,
  type.measure = "mse",
  standardize = TRUE
)



best_lambda_min  <- cv_lasso$lambda.min
best_lambda_1se  <- cv_lasso$lambda.1se

cat("Best λ (min):", signif(best_lambda_min,4),
    " | Best λ (1-SE):", signif(best_lambda_1se,4), "\n")

lam_tbl <- data.frame(
  `λ.min`  = signif(best_lambda_min, 4),
  `λ.1se`  = signif(best_lambda_1se, 4)
)

kable(lam_tbl, caption = "Optimal λ from cross-validation")

# Variables that survive using Lamda Min.
coef_min <- as.matrix(coef(cv_lasso, s = "lambda.min"))
sel_min <- coef_min[coef_min[,1] != 0, , drop = FALSE]

cat("\n--- Selected Variables with lambda.min ---\n")
print(sel_min)

# Variables that survive with Lamda1se

coef_1se <- as.matrix(coef(cv_lasso, s = "lambda.1se"))
sel_1se <- coef_1se[coef_1se[,1] != 0, , drop = FALSE]

cat("\n--- Selected Variables with lambda.1se ---\n")
print(sel_1se)

# Make sure our test and train data sets make sense. 
miss <- setdiff(colnames(Xvars_mat_train), colnames(Xvars_mat_test))
if (length(miss)) {
  Xvars_mat_test <- cbind(
    Xvars_mat_test,
    matrix(0, nrow(Xvars_mat_test), length(miss),
           dimnames = list(NULL, miss))
  )
}
extra <- setdiff(colnames(Xvars_mat_test), colnames(Xvars_mat_train))
if (length(extra)) {
  Xvars_mat_test <- Xvars_mat_test[, setdiff(colnames(Xvars_mat_test), extra), drop = FALSE]
}
Xvars_mat_test <- Xvars_mat_test[, colnames(Xvars_mat_train), drop = FALSE]

# Functions for metrics.
rmse <- function(y, p) sqrt(mean((y - p)^2))
mae  <- function(y, p) mean(abs(y - p))
r2   <- function(y, p) 1 - sum((y - p)^2) / sum((y - mean(y))^2)

row_metrics <- function(name, y, p) {
  data.frame(
    Model = name,
    RMSE  = rmse(y, p),
    MAE   = mae(y, p),
    R2    = r2(y, p),
    SD_y  = sd(y)
  )
}

nz_coefs <- function(cv_obj, s) {
  cf <- as.matrix(coef(cv_obj, s = s))
  out <- data.frame(Variable = rownames(cf), Coef = cf[,1], row.names = NULL)
  subset(out, Coef != 0)
}

# Lasso Predictions. 
pred_lasso_min <- as.numeric(predict(cv_lasso, newx = Xvars_mat_test, s = "lambda.min"))
pred_lasso_1se <- as.numeric(predict(cv_lasso, newx = Xvars_mat_test, s = "lambda.1se"))

met_lasso_min  <- row_metrics("Lasso (lambda.min)",  Yvar_test, pred_lasso_min)
met_lasso_1se  <- row_metrics("Lasso (lambda.1se)",  Yvar_test, pred_lasso_1se)

sel_lasso_min  <- nz_coefs(cv_lasso, "lambda.min")
sel_lasso_1se  <- nz_coefs(cv_lasso, "lambda.1se")

# Baseline (Mean of the Variable)
pred_base <- rep(mean(Yvar_train), length(Yvar_test))
met_base  <- row_metrics("Baseline (mean of train)", Yvar_test, pred_base)

# Elastic Net (Tuning for alpha and Lambda)
set.seed(254646)
alpha_grid <- seq(0.1, 0.9, by = 0.1)

cv_list <- lapply(alpha_grid, function(a)
  cv.glmnet(
    Xvars_mat_train, Yvar_train,
    family = "gaussian",
    alpha  = a,
    nfolds = 10,
    type.measure = "mse",
    standardize = TRUE
  )
)


cv_mins <- sapply(cv_list, function(cv) min(cv$cvm))
best_i  <- which.min(cv_mins)
best_a  <- alpha_grid[best_i]
best_cv <- cv_list[[best_i]]

cat("\nElastic Net — best alpha:", best_a,
    "| lambda.min:", signif(best_cv$lambda.min, 4),
    "| lambda.1se:", signif(best_cv$lambda.1se, 4), "\n")

# Elastic Net Predictions. 
pred_en_min <- as.numeric(predict(best_cv, newx = Xvars_mat_test, s = "lambda.min"))
pred_en_1se <- as.numeric(predict(best_cv, newx = Xvars_mat_test, s = "lambda.1se"))

met_en_min  <- row_metrics(paste0("Elastic Net α=", best_a, " (lambda.min)"), Yvar_test, pred_en_min)
met_en_1se  <- row_metrics(paste0("Elastic Net α=", best_a, " (lambda.1se)"), Yvar_test, pred_en_1se)

sel_en_min  <- nz_coefs(best_cv, "lambda.min")
sel_en_1se  <- nz_coefs(best_cv, "lambda.1se")

# Metric Comparison. 
metrics_all <- rbind(met_base, met_lasso_min, met_lasso_1se, met_en_min, met_en_1se)
print(metrics_all, row.names = FALSE)

metrics_tbl <- metrics_all %>%
  select(Model, RMSE, MAE, R2, SD_y) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

best_rmse_idx <- which.min(metrics_all$RMSE)

kable(metrics_tbl, caption = "Test Performance (RMSE, MAE, R², SD_y)") %>%
  kable_styling(full_width = FALSE, position = "center",
                bootstrap_options = c("striped","hover","condensed")) %>%
  row_spec(best_rmse_idx, bold = TRUE)


# Best features used. 
top_k <- 15
top_lasso_min <- head(sel_lasso_min[order(abs(sel_lasso_min$Coef), decreasing = TRUE), ], top_k)
top_lasso_1se <- head(sel_lasso_1se[order(abs(sel_lasso_1se$Coef), decreasing = TRUE), ], top_k)
top_en_min    <- head(sel_en_min[order(abs(sel_en_min$Coef),   decreasing = TRUE), ], top_k)
top_en_1se    <- head(sel_en_1se[order(abs(sel_en_1se$Coef),   decreasing = TRUE), ], top_k)

cat("\nTop", top_k, "coef (|coef|) — Lasso λ.min:\n"); print(top_lasso_min, row.names = FALSE)
cat("\nTop", top_k, "coef (|coef|) — Lasso λ.1se:\n"); print(top_lasso_1se, row.names = FALSE)
cat("\nTop", top_k, "coef (|coef|) — ENet λ.min:\n");  print(top_en_min,    row.names = FALSE)
cat("\nTop", top_k, "coef (|coef|) — ENet λ.1se:\n");  print(top_en_1se,    row.names = FALSE)

# Amount of Variables Selected in each model. 
count_nz <- function(df) sum(df$Variable != "(Intercept)")
cat("\n Amount of Variables -> Lasso min:", count_nz(sel_lasso_min),
    "| Lasso 1se:", count_nz(sel_lasso_1se),
    "| ENet min:", count_nz(sel_en_min),
    "| ENet 1se:", count_nz(sel_en_1se), "\n")

# Ridge and Logistic Regression. 

df_logistic <- data_clean %>% 
  select(!Country)

df_logistic$GrowingMore <- as.integer(df_logistic$`GDP growth (annual %)` > 2.7)

colSums(is.na(df_logistic))

# Data Splitting (Seed One)
set.seed(1111)

train_index <- sample.int(nrow(df_logistic), 110)

train_set <- df_logistic[train_index, ]
test_set  <- df_logistic[-train_index, ]

y_train <- train_set$GrowingMore
y_test  <- test_set$GrowingMore

X_train <- model.matrix(GrowingMore ~ . -`GDP growth (annual %)`, data=train_set)[, -1]
X_test  <- model.matrix(GrowingMore ~ . - `GDP growth (annual %)`, data=test_set )[,-1]

# Ridge and Elastic Net. 
cv_ridge <- cv.glmnet(
  X_train, y_train,
  family = "binomial",
  alpha  = 0,               
  nfolds = 10,
  type.measure = "deviance",
  standardize = TRUE
)

lam_min <- cv_ridge$lambda.min
lam_1se <- cv_ridge$lambda.1se
cat("lambda.min =", signif(lam_min,4), " | lambda.1se =", signif(lam_1se,4), "\n")

# Prediction on Test Using both variables. 
p_min <- as.numeric(predict(cv_ridge, newx = X_test, s = "lambda.min", type = "response"))
p_1se <- as.numeric(predict(cv_ridge, newx = X_test, s = "lambda.1se", type = "response"))

# Classify at 0.5
pred_min <- ifelse(p_min >= 0.5, 1, 0)
pred_1se <- ifelse(p_1se >= 0.5, 1, 0)

# Metrics for performance. 
logloss <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1-1e-15); -mean(y*log(p) + (1-y)*log(1-p)) }
brier   <- function(y, p) mean((p - y)^2)

acc_min  <- mean(pred_min == y_test)
acc_1se  <- mean(pred_1se == y_test)
ll_min   <- logloss(y_test, p_min)
ll_1se   <- logloss(y_test, p_1se)
br_min   <- brier(y_test, p_min)
br_1se   <- brier(y_test, p_1se)

# optional AUC
get_auc <- function(y,p){
  if (requireNamespace("pROC", quietly = TRUE)) as.numeric(pROC::auc(y, p)) else NA_real_
}
auc_min <- get_auc(y_test, p_min)
auc_1se <- get_auc(y_test, p_1se)

results <- data.frame(
  Model   = c("Ridge (lambda.min)", "Ridge (lambda.1se)"),
  Accuracy= c(acc_min, acc_1se),
  LogLoss = c(ll_min, ll_1se),
  Brier   = c(br_min, br_1se),
  AUC     = c(auc_min, auc_1se)
)

print(results, row.names = FALSE)

results_tbl <- results %>%
  transmute(
    Model,
    Accuracy = round(Accuracy, 4),
    LogLoss  = round(LogLoss, 4),
    Brier    = round(Brier, 4),
    AUC      = ifelse(is.na(AUC), NA, round(AUC, 3))
  )

kable(results_tbl, caption = "Ridge (logistic) — Test Metrics") %>%
  kable_styling(full_width = FALSE, position = "center",
                bootstrap_options = c("striped","hover","condensed"))

# Final Model and Coefficientes. 
coef_min <- coef(cv_ridge, s = "lambda.min")
coef_1se <- coef(cv_ridge, s = "lambda.1se")
# Example: show 10 largest (by |coef|) at lambda.1se
ix <- order(abs(as.numeric(coef_1se)) , decreasing = TRUE)
print(coef_1se[ix[1:10], , drop = FALSE])


# Data Splitting (Seed Two)
set.seed(9999)

train_index <- sample.int(nrow(df_logistic), 110)

train_set <- df_logistic[train_index, ]
test_set  <- df_logistic[-train_index, ]

y_train <- train_set$GrowingMore
y_test  <- test_set$GrowingMore

X_train <- model.matrix(GrowingMore ~ . -`GDP growth (annual %)`, data=train_set)[, -1]
X_test  <- model.matrix(GrowingMore ~ . - `GDP growth (annual %)`, data=test_set )[,-1]

# Ridge and Elastic Net. 
cv_ridge <- cv.glmnet(
  X_train, y_train,
  family = "binomial",
  alpha  = 0,               
  nfolds = 10,
  type.measure = "deviance",
  standardize = TRUE
)

lam_min <- cv_ridge$lambda.min
lam_1se <- cv_ridge$lambda.1se
cat("lambda.min =", signif(lam_min,4), " | lambda.1se =", signif(lam_1se,4), "\n")

# Prediction on Test Using both variables. 
p_min <- as.numeric(predict(cv_ridge, newx = X_test, s = "lambda.min", type = "response"))
p_1se <- as.numeric(predict(cv_ridge, newx = X_test, s = "lambda.1se", type = "response"))

# Classify at 0.5
pred_min <- ifelse(p_min >= 0.5, 1, 0)
pred_1se <- ifelse(p_1se >= 0.5, 1, 0)

# Metrics for performance. 
logloss <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1-1e-15); -mean(y*log(p) + (1-y)*log(1-p)) }
brier   <- function(y, p) mean((p - y)^2)

acc_min  <- mean(pred_min == y_test)
acc_1se  <- mean(pred_1se == y_test)
ll_min   <- logloss(y_test, p_min)
ll_1se   <- logloss(y_test, p_1se)
br_min   <- brier(y_test, p_min)
br_1se   <- brier(y_test, p_1se)

# optional AUC
get_auc <- function(y,p){
  if (requireNamespace("pROC", quietly = TRUE)) as.numeric(pROC::auc(y, p)) else NA_real_
}
auc_min <- get_auc(y_test, p_min)
auc_1se <- get_auc(y_test, p_1se)

results1 <- data.frame(
  Model   = c("Ridge (lambda.min)", "Ridge (lambda.1se)"),
  Accuracy= c(acc_min, acc_1se),
  LogLoss = c(ll_min, ll_1se),
  Brier   = c(br_min, br_1se),
  AUC     = c(auc_min, auc_1se)
)

print(results, row.names = FALSE)

results_tbl_1 <- results1 %>%
  transmute(
    Model,
    Accuracy = round(Accuracy, 4),
    LogLoss  = round(LogLoss, 4),
    Brier    = round(Brier, 4),
    AUC      = ifelse(is.na(AUC), NA, round(AUC, 3))
  )

kable(results_tbl_1, caption = "Ridge (logistic) — Test Metrics") %>%
  kable_styling(full_width = FALSE, position = "center",
                bootstrap_options = c("striped","hover","condensed"))

# Final Model and Coefficientes. 
coef_min <- coef(cv_ridge, s = "lambda.min")
coef_1se <- coef(cv_ridge, s = "lambda.1se")
# Example: show 10 largest (by |coef|) at lambda.1se
ix <- order(abs(as.numeric(coef_1se)) , decreasing = TRUE)
print(coef_1se[ix[1:10], , drop = FALSE])

