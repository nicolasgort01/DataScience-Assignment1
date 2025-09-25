library(tidyverse)
library(caret)
library(glmnet)
library(MASS)
library(lmtest) 
library(dplyr)

# no scientific notations
options(scipen = 999)
rm(list = ls())
# Read data
stopifnot(file.exists("a1_data_group_9.csv"))
df <- readr::read_csv("a1_data_group_9.csv", show_col_types = FALSE)

data_lasso <- df[, !(names(df) %in% "Country")]


# Train Test Split 

set.seed(42)
train_idx <- sample(seq_len(nrow(data_lasso)), 110)

X_train <- model.matrix(`GDP growth (annual %)` ~ ., data = data_lasso[train_idx, ])[, -1]
y_train <- data_lasso$`GDP growth (annual %)`[train_idx]

X_test  <- model.matrix(`GDP growth (annual %)` ~ ., data = data_lasso[-train_idx, ])[, -1]
y_test  <- data_lasso$`GDP growth (annual %)`[-train_idx]


# lasso regression
alpha1.fit <- cv.glmnet(X_train, y_train, type.measure = "mse", alpha = 1, family = "gaussian")
alpha1.predict <- predict(alpha1.fit, s = alpha1.fit$lambda.1se, newx = X_test)
mean((y_test - alpha1.predict)^2)

# Extract coefficients at lambda.1se
coef(alpha1.fit, s = alpha1.fit$lambda.1se)

######

# Chat lasso 

## Reproducible CV folds (optional but recommended for fairness)
set.seed(42)
foldid <- sample(rep(1:10, length.out = nrow(X_train)))

# Cross-validated LASSO (alpha = 1)
lasso_cv <- cv.glmnet(
  X_train, y_train,
  family = "gaussian",
  alpha  = 1,
  nfolds = 10,
  foldid = foldid,          # comment out if you don't care about fixed folds
  type.measure = "mse",
  standardize = TRUE
)

# Optimal lambdas
best_lambda_min  <- lasso_cv$lambda.min   # lowest CV error
best_lambda_1se  <- lasso_cv$lambda.1se   # 1-SE rule (simpler model)

cat("LASSO | lambda.min:", signif(best_lambda_min, 4),
    "| lambda.1se:", signif(best_lambda_1se, 4),
    "| CV-MSE @ min:", signif(min(lasso_cv$cvm), 4), "\n")

# Coefficients at both lambdas
coef_min  <- coef(lasso_cv, s = best_lambda_min)
coef_1se  <- coef(lasso_cv, s = best_lambda_1se)

cat("\nNon-zero betas @ lambda.min:\n")
print(coef_min[which(as.numeric(coef_min) != 0), , drop = FALSE])

cat("\nNon-zero betas @ lambda.1se:\n")
print(coef_1se[which(as.numeric(coef_1se) != 0), , drop = FALSE])

# Test predictions + metrics
pred_min <- as.numeric(predict(lasso_cv, s = best_lambda_min, newx = X_test))
pred_1se <- as.numeric(predict(lasso_cv, s = best_lambda_1se, newx = X_test))

mse_min <- mean((y_test - pred_min)^2)
mse_1se <- mean((y_test - pred_1se)^2)

r2_min  <- 1 - sum((y_test - pred_min)^2) / sum((y_test - mean(y_test))^2)
r2_1se  <- 1 - sum((y_test - pred_1se)^2) / sum((y_test - mean(y_test))^2)

cat("\nTEST metrics:\n",
    "lambda.min -> MSE:", signif(mse_min, 4), " | R^2:", signif(r2_min, 4), "\n",
    "lambda.1se -> MSE:", signif(mse_1se, 4), " | R^2:", signif(r2_1se, 4), "\n")



plot(lasso_cv)




###########

# lasso from lecture video 

# Use your split
train_data <- data_lasso[train_idx, ]
test_data  <- data_lasso[-train_idx, ]

# ---- Cross-validation control (you asked to specify it) ----
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  verboseIter = TRUE
)

# ---- LASSO (alpha = 1) ----
set.seed(42)
lasso_fit <- train(
  `GDP growth (annual %)` ~ .,
  data = train_data,
  method = "glmnet",
  preProcess = c("center", "scale"),      # standardize predictors
  trControl = fitControl,
  tuneGrid = expand.grid(
    alpha  = 1,                           # LASSO
    lambda = exp(seq(log(1e-4), log(1e+2), length.out = 1000))
  )
)

# CV summary + best lambda
lasso_fit$bestTune
print(lasso_fit)

# Coefficients at best lambda
coef_best <- coef(lasso_fit$finalModel, s = lasso_fit$bestTune$lambda)
coef_best[which(as.numeric(coef_best) != 0), , drop = FALSE]

# Test-set performance
pred_lasso <- predict(lasso_fit, newdata = test_data)
postResample(pred_lasso, test_data$`GDP growth (annual %)`)

# (Optional) CV plot
plot(lasso_fit)



# Extract the results table from caret
res <- lasso_fit$results

# Remove rows with NaN R²
res_clean <- res[!is.na(res$Rsquared), ]

# Find the λ with the lowest RMSE (or highest R² if you prefer)
best_row <- res_clean[which.min(res_clean$RMSE), ]
best_row

lasso_fit

###################
# elastic net 
alpha0.5.fit <- cv.glmnet(X_train, y_train, type.measure = "mse", alpha = 0.5, family = "gaussian")
alpha0.5.predict <- predict(alpha0.5.fit, s = alpha0.5.fit$lambda.1se, newx = X_test)
mean((y_test - alpha0.5.predict)^2)


list.of.fits <- list()

for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  list.of.fits[[fit.name]] <- cv.glmnet(X_train, y_train, type.measure = "mse", alpha = i/10, family = "gaussian")
}

results <- data.frame()

for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  # Predict on test data using lambda.1se
  predicted <- predict(list.of.fits[[fit.name]], s = list.of.fits[[fit.name]]$lambda.1se, newx = X_test)
  # Compute test MSE
  mse <- mean((y_test - predicted)^2)
  # Store results
  temp <- data.frame(alpha = i/10, mse = mse, fit.name = fit.name)
  results <- rbind(results, temp)
}

################################################

# nico elastic net 
set.seed(123)
alpha_grid <- seq(0.1, 0.9, by = 0.01)

cv_list <- vector("list", length(alpha_grid))
cv_min  <- numeric(length(alpha_grid))


for (i in seq_along(alpha_grid)) {
  cv_list[[i]] <- cv.glmnet(X_train, y_train, family = "gaussian", alpha  = alpha_grid[i], nfolds = 10, type.measure = "mse",standardize = TRUE)
  cv_min[i] <- min(cv_list[[i]]$cvm)
}

best_i      <- which.min(cv_min)
best_alpha  <- alpha_grid[best_i]
best_lambda <- cv_list[[best_i]]$lambda.min

cat("\nElastic Net — best alpha:", best_alpha,
    "| best lambda:", signif(best_lambda, 4),
    "| best CV-MSE:", signif(cv_min[best_i], 4), "\n")



enet_fit <- glmnet(X_train, y_train, family = "gaussian", alpha  = best_alpha, lambda = best_lambda, standardize = TRUE)

# Non-zero coefficients (for interpretation/appendix)
enet_coef <- coef(enet_fit)
nz <- which(as.numeric(enet_coef) != 0)
cat("\nNon-zero Elastic Net coefficients:\n")
print(enet_coef[nz, , drop = FALSE])

# Predict on TEST
enet_pred <- as.numeric(predict(enet_fit, newx = X_test))

# Compute test MSE and R^2 
mse_enet <- mean((y_test - enet_pred)^2)
r2_enet  <- 1 - sum((y_test - enet_pred)^2) / sum((y_test - mean(y_test))^2)

r2_enet

###################################

# elastic net chat 
set.seed(123)
foldid <- sample(rep(1:10, length.out = nrow(X_train)))  # 10 folds reused

alpha_grid <- seq(0, 1, by = 0.005)  # include ridge (0) and lasso (1) if you want
cv_list <- vector("list", length(alpha_grid))
cv_min  <- numeric(length(alpha_grid))

for (i in seq_along(alpha_grid)) {
  cv_list[[i]] <- cv.glmnet(
    X_train, y_train,
    family = "gaussian",
    alpha  = alpha_grid[i],
    nfolds = 10,
    foldid = foldid,        # <-- same folds for all alphas
    type.measure = "mse",
    standardize = TRUE
  )
  cv_min[i] <- min(cv_list[[i]]$cvm)
}

best_i      <- which.min(cv_min)
best_alpha  <- alpha_grid[best_i]
best_lambda <- cv_list[[best_i]]$lambda.min

# Fit final ENet on training with best hyperparams & evaluate
enet_fit  <- glmnet(X_train, y_train, family="gaussian", alpha=best_alpha, lambda=best_lambda)
enet_pred <- as.numeric(predict(enet_fit, newx = X_test))
mse_enet  <- mean((y_test - enet_pred)^2)
r2_enet   <- 1 - sum((y_test - enet_pred)^2) / sum((y_test - mean(y_test))^2)

# Non-zero coefficients
enet_coef <- coef(enet_fit)
nz <- which(as.numeric(enet_coef) != 0)
nonzero <- enet_coef[nz, , drop = FALSE]

nonzero
###########################################

# elastic net bahar 


## 1) Cross-validate over alpha grid (reuse folds for fairness, optional)
set.seed(080808)

alpha_grid <- seq(0.1, 0.9, by = 0.01)

cv_list <- vector("list", length(alpha_grid))
cv_min  <- numeric(length(alpha_grid))

for (i in seq_along(alpha_grid)) {
  cv_list[[i]] <- cv.glmnet(
    X_train, y_train,
    family = "gaussian",
    alpha  = alpha_grid[i],
    nfolds = 10,         
    type.measure = "mse",
    standardize = TRUE
  )
  cv_min[i] <- min(cv_list[[i]]$cvm)
}

best_i      <- which.min(cv_min)
best_alpha  <- alpha_grid[best_i]
best_lambda_min <- cv_list[[best_i]]$lambda.min
best_lambda_1se <- cv_list[[best_i]]$lambda.1se

cat("\nElastic Net — best alpha:", best_alpha,
    "| lambda.min:", signif(best_lambda_min, 4),
    "| lambda.1se:", signif(best_lambda_1se, 4),
    "| best CV-MSE:", signif(cv_min[best_i], 4), "\n")

## 2) Fit final ENet at (alpha*, lambda.min) and (alpha*, lambda.1se)
enet_min  <- glmnet(X_train, y_train, family="gaussian",
                    alpha=best_alpha, lambda=best_lambda_min, standardize=TRUE)
enet_1se  <- glmnet(X_train, y_train, family="gaussian",
                    alpha=best_alpha, lambda=best_lambda_1se, standardize=TRUE)

## 3) Non-zero coefficients (nice for the appendix)
cat("\nNon-zero coefficients @ lambda.min:\n")
coef_min <- coef(enet_min); print(coef_min[which(as.numeric(coef_min)!=0), , drop=FALSE])

cat("\nNon-zero coefficients @ lambda.1se:\n")
coef_1se <- coef(enet_1se); print(coef_1se[which(as.numeric(coef_1se)!=0), , drop=FALSE])

## 4) Test predictions + metrics
pred_min <- as.numeric(predict(enet_min, newx = X_test))
pred_1se <- as.numeric(predict(enet_1se, newx = X_test))

mse_min <- mean((y_test - pred_min)^2)
mse_1se <- mean((y_test - pred_1se)^2)

r2_min  <- 1 - sum((y_test - pred_min)^2) / sum((y_test - mean(y_test))^2)
r2_1se  <- 1 - sum((y_test - pred_1se)^2) / sum((y_test - mean(y_test))^2)

cat("\nTEST metrics:\n",
    "lambda.min -> MSE:", signif(mse_min, 4), " | R^2:", signif(r2_min, 4), "\n",
    "lambda.1se -> MSE:", signif(mse_1se, 4), " | R^2:", signif(r2_1se, 4), "\n")
#######

#Chat gpt

# create train & test sets
set.seed(49)  # for reproducibility

# randomly divide dataset
train_index <- sample(1:nrow(df), 110)
train_set <- df[train_index, ]
test_set  <- df[-train_index, ]

Xvars_mat_train <- model.matrix(`GDP growth (annual %)` ~.-Country,data = train_set)[, -1]
Yvar_train      <- train_set$`GDP growth (annual %)`              

Xvars_mat_test  <- model.matrix(`GDP growth (annual %)` ~ .-Country, data = test_set)[, -1]  
Yvar_test       <- test_set$`GDP growth (annual %)`    

missing <- setdiff(colnames(Xvars_mat_train), colnames(Xvars_mat_test))
if (length(missing)) {
  Xvars_mat_test <- cbind(
    Xvars_mat_test,
    matrix(0, nrow(Xvars_mat_test), length(missing), dimnames = list(NULL, missing))
  )
}
extra <- setdiff(colnames(Xvars_mat_test), colnames(Xvars_mat_train))
if (length(extra)) Xvars_mat_test <- Xvars_mat_test[, setdiff(colnames(Xvars_mat_test), extra), drop=FALSE]

Xvars_mat_test <- Xvars_mat_test[, colnames(Xvars_mat_train), drop = FALSE]



# Search a small grid for alpha (0<alpha<1)
alpha_grid <- seq(0.1, 0.9, by = 0.01)

cv_list <- vector("list", length(alpha_grid))
cv_min  <- numeric(length(alpha_grid))

for (i in seq_along(alpha_grid)) {
  cv_list[[i]] <- cv.glmnet(
    Xvars_mat_train, Yvar_train,
    family = "gaussian",
    alpha  = alpha_grid[i],
    nfolds = 10,
    type.measure = "mse",
    standardize = TRUE
  )
  cv_min[i] <- min(cv_list[[i]]$cvm)
}

best_i      <- which.min(cv_min)
best_alpha  <- alpha_grid[best_i]
best_lambda <- cv_list[[best_i]]$lambda.min

cat("\nElastic Net — best alpha:", best_alpha,
    "| best lambda:", signif(best_lambda, 4),
    "| best CV-MSE:", signif(cv_min[best_i], 4), "\n")

# Fit final ENet on TRAIN at chosen (alpha, lambda)
enet_fit <- glmnet(
  Xvars_mat_train, Yvar_train,
  family = "gaussian",
  alpha  = best_alpha,
  lambda = best_lambda,
  standardize = TRUE
)

# Non-zero coefficients (for interpretation/appendix)
enet_coef <- coef(enet_fit)
nz <- which(as.numeric(enet_coef) != 0)
cat("\nNon-zero Elastic Net coefficients:\n")
print(enet_coef[nz, , drop = FALSE])

#ENet test predictions + metrics

# Predict on TEST
enet_pred <- as.numeric(predict(enet_fit, newx = Xvars_mat_test))

# Compute test MSE and R^2 
mse_enet <- mean((Yvar_test - enet_pred)^2)
r2_enet  <- 1 - sum((Yvar_test - enet_pred)^2) / sum((Yvar_test - mean(Yvar_test))^2)

# If results_2 doesn't exist yet, initialize it
if (!exists("results_2")) {
  results_2 <- data.frame(
    Model = character(),
    MSE   = numeric(),
    R2    = numeric(),
    stringsAsFactors = FALSE
  )
}

results_2 <- rbind(                                  
  results_2,
  data.frame(
    Model = paste0("Elastic Net (alpha=", best_alpha, ", lambda.min)"),
    MSE   = mse_enet,
    R2    = r2_enet
  )
)



results_2






