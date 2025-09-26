library(rio)
library(tidyverse)
library(caret)
library(lmtest)
library(car)
library(glmnet)
getwd()


rm(list = ls())
df <- import('Data/a1_data_group_9.csv')

# Data Processing. 
data_clean <- df %>%
  rename(
    country                = `Country`,
    electricity_access     = `Access to electricity (% of population)`,
    adolescent_fertility   = `Adolescent fertility rate (births per 1,000 women ages 15-19)`,
    age_dependency_ratio   = `Age dependency ratio (% of working-age population)`,
    contrib_family_fem     = `Contributing family workers, female (% of female employment) (modeled ILO estimate)`,
    contrib_family_male    = `Contributing family workers, male (% of male employment) (modeled ILO estimate)`,
    contrib_family_total   = `Contributing family workers, total (% of total employment) (modeled ILO estimate)`,
    credit_info_index      = `Depth of credit information index (0=low to 8=high)`,
    employers_fem          = `Employers, female (% of female employment) (modeled ILO estimate)`,
    employers_male         = `Employers, male (% of male employment) (modeled ILO estimate)`,
    employers_total        = `Employers, total (% of total employment) (modeled ILO estimate)`,
    emp_agriculture_total  = `Employment in agriculture (% of total employment) (modeled ILO estimate)`,
    emp_agriculture_fem    = `Employment in agriculture, female (% of female employment) (modeled ILO estimate)`,
    emp_agriculture_male   = `Employment in agriculture, male (% of male employment) (modeled ILO estimate)`,
    emp_industry_total     = `Employment in industry (% of total employment) (modeled ILO estimate)`,
    emp_industry_fem       = `Employment in industry, female (% of female employment) (modeled ILO estimate)`,
    emp_industry_male      = `Employment in industry, male (% of male employment) (modeled ILO estimate)`,
    emp_services_total     = `Employment in services (% of total employment) (modeled ILO estimate)`,
    emp_services_fem       = `Employment in services, female (% of female employment) (modeled ILO estimate)`,
    emp_services_male      = `Employment in services, male (% of male employment) (modeled ILO estimate)`,
    export_value_index     = `Export value index (2000 = 100)`,
    export_volume_index    = `Export volume index (2000 = 100)`,
    fertility_rate_total   = `Fertility rate, total (births per woman)`,
    broadband_subs         = `Fixed broadband Internet subscribers (per 100 people)`,
    gdp_growth             = `GDP growth (annual %)`,
    gdp_pc_const2005       = `GDP per capita (constant 2005 US$)`,
    gdp_pc_ppp             = `GDP per capita, PPP (constant 2011 international $)`,
    internet_users         = `Individuals using the Internet (% of population)`,
    lfpr_fem               = `Labor force participation rate, female (% of female population ages 15+) (modeled ILO estimate)`,
    lfpr_male              = `Labor force participation rate, male (% of male population ages 15+) (modeled ILO estimate)`,
    lfpr_total             = `Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)`,
    labor_force_total      = `Labor force, total`,
    life_exp_fem           = `Life expectancy at birth, female (years)`,
    life_exp_male          = `Life expectancy at birth, male (years)`,
    mobile_subs            = `Mobile cellular subscriptions (per 100 people)`,
    own_account_fem        = `Own-account workers, female (% of female employment) (modeled ILO estimate)`,
    own_account_male       = `Own-account workers, male (% of male employment) (modeled ILO estimate)`,
    own_account_total      = `Own-account workers, total (% of male employment) (modeled ILO estimate)`,
    pop_0_14_pct           = `Population ages 0-14 (% of total)`,
    pop_0_14_total         = `Population ages 0-14, total`,
    pop_15_64_pct          = `Population ages 15-64 (% of total)`,
    pop_15_64_total        = `Population ages 15-64, total`,
    pop_65plus_pct         = `Population ages 65 and above (% of total)`,
    pop_65plus_total       = `Population ages 65 and above, total`,
    pop_density            = `Population density (people per sq. km of land area)`,
    pop_growth             = `Population growth (annual %)`,
    pop_total              = `Population, total`,
    credit_coverage_priv   = `Private credit bureau coverage (% of adults)`,
    credit_coverage_pub    = `Public credit registry coverage (% of adults)`,
    rural_pop_total        = `Rural population`,
    rural_pop_pct          = `Rural population (% of total population)`,
    self_emp_fem           = `Self-employed, female (% of female employment) (modeled ILO estimate)`,
    self_emp_male          = `Self-employed, male (% of male employment) (modeled ILO estimate)`,
    self_emp_total         = `Self-employed, total (% of total employment) (modeled ILO estimate)`,
    tax_payments_num       = `Tax payments (number)`,
    telephone_lines        = `Telephone lines (per 100 people)`,
    contract_days          = `Time required to enforce a contract (days)`,
    start_business_days    = `Time required to start a business (days)`,
    tax_prep_hours         = `Time to prepare and pay taxes (hours)`,
    unemp_fem              = `Unemployment, female (% of female labor force) (modeled ILO estimate)`,
    unemp_male             = `Unemployment, male (% of male labor force) (modeled ILO estimate)`,
    unemp_total            = `Unemployment, total (% of total labor force) (modeled ILO estimate)`,
    unemp_youth_fem        = `Unemployment, youth female (% of female labor force ages 15-24) (modeled ILO estimate)`,
    unemp_youth_male       = `Unemployment, youth male (% of male labor force ages 15-24) (modeled ILO estimate)`,
    unemp_youth_total      = `Unemployment, youth total (% of total labor force ages 15-24) (modeled ILO estimate)`,
    urban_pop_total        = `Urban population`,
    urban_pop_pct          = `Urban population (% of total)`,
    vulner_emp_fem         = `Vulnerable employment, female (% of female employment) (modeled ILO estimate)`,
    vulner_emp_male        = `Vulnerable employment, male (% of male employment) (modeled ILO estimate)`,
    vulner_emp_total       = `Vulnerable employment, total (% of total employment) (modeled ILO estimate)`,
    waged_emp_fem          = `Wage and salaried workers, female (% of female employment) (modeled ILO estimate)`,
    waged_emp_male         = `Wage and salaried workers, male (% of male employment) (modeled ILO estimate)`,
    waged_emp_total        = `Wage and salaried workers, total (% of total employment) (modeled ILO estimate)`,
    net_trade              = `Net trade in goods and services (BoP, current US$)`
  ) %>% 
  mutate(
    log_net_trade_signed = sign(net_trade) * log1p(abs(net_trade)),
    pop_growth_sq = pop_growth^2,
  )

data_copy <- data_clean

# Now we will focus on this variables, lets check their distribution. 

data_clean %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()

# As we can see, there is skewdness in several variables, so we will scale relevant variables.
# We will also include non linear information.
data_model <- data_clean %>% 
  select(employers_total,
         fertility_rate_total, 
         lfpr_total, unemp_total,
         export_value_index, 
         pop_growth, gdp_pc_ppp, 
         waged_emp_total, 
         gdp_growth, 
         life_exp_male,
         net_trade) %>% 
  mutate(gdp_pc_ppp = log(gdp_pc_ppp),
         export_value_index = log(export_value_index),
         employers_total = log(employers_total),
  )

# Firts Linear Regression. 

linear_fit_with <- lm(
  gdp_growth ~
    employers_total +
    fertility_rate_total +
    lfpr_total +
    life_exp_male +
    unemp_total +
    export_value_index +
    pop_growth +
    gdp_pc_ppp +
    net_trade+
    waged_emp_total,
  
  data = data_model
)

summary(linear_fit_with)

# Model without net trade. 
linear_fit_without <- update(linear_fit_with, . ~ . - net_trade)

summary(linear_fit_without)

# Comparison between both. 
anova(linear_fit_with, linear_fit_without)

# Here we should try different transformations and include non linear relations and interactions. 

safe_log   <- function(x) log(pmax(x, 1, na.rm = TRUE))
signed_log <- function(x) ifelse(is.na(x), NA_real_,
                                 sign(x) * log1p(abs(x)))


# Variable Selection for first Lasso. 
data1 <- data_clean %>%
  mutate(
    # engineered variables
    log_gdppc_ppp        = safe_log(gdp_pc_ppp),
    log_net_trade_signed = signed_log(net_trade),
    log_export_value_idx = safe_log(export_value_index),
  ) %>%
  # select target + predictors
  select(
    gdp_growth,                                # dependent
    log_gdppc_ppp,                             # convergence
    pop_growth,                                # demography
    age_dependency_ratio,                      # demography
    life_exp_male,                             # male life expectancy
    urban_pop_pct,                             # urbanization
    log_net_trade_signed,                      # openness
    log_export_value_idx,                      # external demand
    unemp_total,                               # labor market slack
    credit_info_index,                         # finance/institutions (or credit_coverage_priv)
    internet_users                             # technology diffusion
  )


# Specific form for Lasso. 
form_base <- gdp_growth ~ log_gdppc_ppp + pop_growth + age_dependency_ratio +
  life_exp_male + urban_pop_pct + log_net_trade_signed + log_export_value_idx +
  unemp_total + credit_info_index + internet_users

form_plus <- update(form_base, . ~ . + I(log_net_trade_signed^2))


x_mat <- model.matrix(form_plus, data = data1)[, -1, drop = FALSE]
y_vec <- data1$gdp_growth

set.seed(222)
cv_lasso <- cv.glmnet(x_mat, y_vec, alpha = 1, family = "gaussian", nfolds = 10)
cat("\nQ3: LASSO λ_min & λ_1se \n")
print(c(lambda.min = cv_lasso$lambda.min, lambda.1se = cv_lasso$lambda.1se))
cat("\nLASSO nonzero coefficients at λ_min:\n")
print(coef(cv_lasso, s = "lambda.min"))

# Importance of standarizing. 
set.seed(222)
cv_lasso_std    <- cv_glmnet <- cv_lasso                  # already standardized (default)
cv_lasso_nostd  <- cv.glmnet(x_mat, y_vec, alpha = 1, family = "gaussian",
                             nfolds = 10, standardize = FALSE)
cat("\nQ4: CV error (with std vs no-std) \n")
print(c(with_std = min(cv_lasso_std$cvm), no_std = min(cv_lasso_nostd$cvm)))

# Different seed.
set.seed(2025)
idx   <- sample(seq_len(nrow(data1)), size = 110)
train <- data1[idx, , drop = FALSE]
test  <- data1[-idx, , drop = FALSE]  # will be 40 when n = 150

x_tr <- model.matrix(form_plus, data = train)[, -1, drop = FALSE]
y_tr <- train$gdp_growth
x_te <- model.matrix(form_plus, data = test)[, -1, drop = FALSE]
y_te <- test$gdp_growth

### Elastic Net and Lasso. 

dfm_train <- data_clean[idx, ]

dfm_test <- data_clean[-idx, ]  
Xvars_mat_train <- model.matrix(gdp_growth ~.-country,data = dfm_train)[, -1]


Yvar_train      <- dfm_train$gdp_growth           

Xvars_mat_test  <- model.matrix(gdp_growth ~ .-country, data = dfm_train)[, -1]  
Yvar_test       <- dfm_train$gdp_growth    


alpha_grid <- seq(0.1, 0.9, by = 0.1)

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

results_elastic_net <- data.frame(
  Model = character (),
  MSE = numeric (),
  R2 = numeric(),
  stringsAsFactors = F
)
results_elastic_net <- rbind(                                  
  results_elastic_net,
  data.frame(
    Model = paste0("Elastic Net (alpha=", best_alpha, ", lambda.min)"),
    MSE   = mse_enet,
    R2    = r2_enet
  )
)



# From 11.

df <- data_clean

df$GrowingMore <- as.integer(df$gdp_growth > 2.7)

# 2) Train / test split -------------------------------------------------------
train_index <- sample.int(nrow(df), 110)
train_set <- df[train_index, ]
test_set  <- df[-train_index, ]

y_train <- train_set$GrowingMore
y_test  <- test_set$GrowingMore

# 3) Model matrices (drop intercept col; exclude 'country' and the original pct) ----
X_train <- model.matrix(GrowingMore ~ . - country - gdp_growth, data=train_set)[, -1]
X_test  <- model.matrix(GrowingMore ~ . - country - gdp_growth, data=test_set )[,-1]

# ensure same columns in train/test (in case factors differ)
miss <- setdiff(colnames(X_train), colnames(X_test))
if (length(miss))
  X_test <- cbind(X_test, matrix(0, nrow(X_test), length(miss),
                                 dimnames=list(NULL, miss)))
extra <- setdiff(colnames(X_test), colnames(X_train))
if (length(extra)) X_test <- X_test[, setdiff(colnames(X_test), extra), drop=FALSE]
X_test <- X_test[, colnames(X_train), drop=FALSE]

# 4) CV Ridge (logistic) ------------------------------------------------------
# (You can set type.measure="auc" if you prefer maximizing AUC)
cv_ridge <- cv.glmnet(
  X_train, y_train,
  family = "binomial",
  alpha  = 0,               # Ridge
  nfolds = 10,
  type.measure = "deviance",# default for binomial
  standardize = TRUE
)

lam_min <- cv_ridge$lambda.min
lam_1se <- cv_ridge$lambda.1se
cat("lambda.min =", signif(lam_min,4), " | lambda.1se =", signif(lam_1se,4), "\n")

# 5) Predictions on TEST ------------------------------------------------------
p_min <- as.numeric(predict(cv_ridge, newx = X_test, s = "lambda.min", type = "response"))
p_1se <- as.numeric(predict(cv_ridge, newx = X_test, s = "lambda.1se", type = "response"))

# classify at 0.5
pred_min <- ifelse(p_min >= 0.5, 1, 0)
pred_1se <- ifelse(p_1se >= 0.5, 1, 0)

# 6) Metrics (Accuracy, LogLoss, Brier; add AUC if pROC installed) ------------
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

# 7) Final model & coefficients (for reporting) --------------------------------
coef_min <- coef(cv_ridge, s = "lambda.min")
coef_1se <- coef(cv_ridge, s = "lambda.1se")
# Example: show 10 largest (by |coef|) at lambda.1se
ix <- order(abs(as.numeric(coef_1se)) , decreasing = TRUE)
print(coef_1se[ix[1:10], , drop = FALSE])

