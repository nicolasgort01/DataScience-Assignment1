# Group Assignment 1 IDS
rm(list= ls())
a1_data_group_9 <- read.csv("~/Desktop/Group Assignment 1 IDS /a1_data_group_9.csv")

install.packages("rio","elasticnet","psych","caret")
library(rio)
library(tidyverse)
library(ggplot2)
library(car)
library(glmnet)
library(elasticnet)
library(psych)
library(caret)

set.seed(123)
options(scipen = 999)
summary(a1_data_group_9)
df <- a1_data_group_9
library(dplyr)
names(df)

data_clean <- df %>%
  dplyr::rename(
    country                = Country,
    electricity_access     = Access.to.electricity....of.population.,
    adolescent_fertility   = Adolescent.fertility.rate..births.per.1.000.women.ages.15.19.,
    age_dependency_ratio   = Age.dependency.ratio....of.working.age.population.,
    contrib_family_fem     = Contributing.family.workers..female....of.female.employment...modeled.ILO.estimate.,
    contrib_family_male    = Contributing.family.workers..male....of.male.employment...modeled.ILO.estimate.,
    contrib_family_total   = Contributing.family.workers..total....of.total.employment...modeled.ILO.estimate.,
    credit_info_index      = Depth.of.credit.information.index..0.low.to.8.high.,
    employers_fem          = Employers..female....of.female.employment...modeled.ILO.estimate.,
    employers_male         = Employers..male....of.male.employment...modeled.ILO.estimate.,
    employers_total        = Employers..total....of.total.employment...modeled.ILO.estimate.,
    emp_agriculture_total  = Employment.in.agriculture....of.total.employment...modeled.ILO.estimate.,
    emp_agriculture_fem    = Employment.in.agriculture..female....of.female.employment...modeled.ILO.estimate.,
    emp_agriculture_male   = Employment.in.agriculture..male....of.male.employment...modeled.ILO.estimate.,
    emp_industry_total     = Employment.in.industry....of.total.employment...modeled.ILO.estimate.,
    emp_industry_fem       = Employment.in.industry..female....of.female.employment...modeled.ILO.estimate.,
    emp_industry_male      = Employment.in.services..male....of.male.employment...modeled.ILO.estimate.,
    emp_services_total     = Employment.in.services....of.total.employment...modeled.ILO.estimate.,
    emp_services_fem       = Employment.in.services..female....of.female.employment...modeled.ILO.estimate.,
    emp_services_male      = Employment.in.services..male....of.male.employment...modeled.ILO.estimate.,
    export_value_index     = Export.value.index..2000...100.,
    export_volume_index    = Export.volume.index..2000...100.,
    fertility_rate_total   = Fertility.rate..total..births.per.woman.,
    broadband_subs         = Fixed.broadband.Internet.subscribers..per.100.people.,
    gdp_growth             = GDP.growth..annual...,
    gdp_pc_const2005       = GDP.per.capita..constant.2005.US..,
    gdp_pc_ppp             = GDP.per.capita..PPP..constant.2011.international...,
    internet_users         = Individuals.using.the.Internet....of.population.,
    lfpr_fem               = Labor.force.participation.rate..female....of.female.population.ages.15....modeled.ILO.estimate.,
    lfpr_male              = Labor.force.participation.rate..male....of.male.population.ages.15....modeled.ILO.estimate.,
    lfpr_total             = Labor.force.participation.rate..total....of.total.population.ages.15....modeled.ILO.estimate.,
    labor_force_total      = Labor.force..total,
    life_exp_fem           = Life.expectancy.at.birth..female..years.,
    life_exp_male          = Life.expectancy.at.birth..male..years.,
    mobile_subs            = Mobile.cellular.subscriptions..per.100.people.,
    own_account_fem        = Own.account.workers..female....of.female.employment...modeled.ILO.estimate.,
    own_account_male       = Own.account.workers..male....of.male.employment...modeled.ILO.estimate.,
    own_account_total      = Own.account.workers..total....of.male.employment...modeled.ILO.estimate.,
    pop_0_14_pct           = Population.ages.0.14....of.total.,
    pop_0_14_total         = Population.ages.0.14..total,
    pop_15_64_pct          = Population.ages.15.64....of.total.,
    pop_15_64_total        = Population.ages.15.64..total,
    pop_65plus_pct         = Population.ages.65.and.above....of.total.,
    pop_65plus_total       = Population.ages.65.and.above..total,
    pop_density            = Population.density..people.per.sq..km.of.land.area.,
    pop_growth             = Population.growth..annual...,
    pop_total              = Population..total,
    credit_coverage_priv   = Private.credit.bureau.coverage....of.adults.,
    credit_coverage_pub    = Public.credit.registry.coverage....of.adults.,
    rural_pop_total        = Rural.population,
    rural_pop_pct          = Rural.population....of.total.population.,
    self_emp_fem           = Self.employed..female....of.female.employment...modeled.ILO.estimate.,
    self_emp_male          = Self.employed..male....of.male.employment...modeled.ILO.estimate.,
    self_emp_total         = Self.employed..total....of.total.employment...modeled.ILO.estimate.,
    tax_payments_num       = Tax.payments..number.,
    telephone_lines        = Telephone.lines..per.100.people.,
    contract_days          = Time.required.to.enforce.a.contract..days.,
    start_business_days    = Time.required.to.start.a.business..days.,
    tax_prep_hours         = Time.to.prepare.and.pay.taxes..hours.,
    unemp_fem              = Unemployment..female....of.female.labor.force...modeled.ILO.estimate.,
    unemp_male             = Unemployment..male....of.male.labor.force...modeled.ILO.estimate.,
    unemp_total            = Unemployment..total....of.total.labor.force...modeled.ILO.estimate.,
    unemp_youth_fem        = Unemployment..youth.female....of.female.labor.force.ages.15.24...modeled.ILO.estimate.,
    unemp_youth_male       = Unemployment..youth.male....of.male.labor.force.ages.15.24...modeled.ILO.estimate.,
    unemp_youth_total      = Unemployment..youth.total....of.total.labor.force.ages.15.24...modeled.ILO.estimate.,
    urban_pop_total        = Urban.population,
    urban_pop_pct          = Urban.population....of.total.,
    vulner_emp_fem         = Vulnerable.employment..female....of.female.employment...modeled.ILO.estimate.,
    vulner_emp_male        = Vulnerable.employment..male....of.male.employment...modeled.ILO.estimate.,
    vulner_emp_total       = Vulnerable.employment..total....of.total.employment...modeled.ILO.estimate.,
    waged_emp_fem          = Wage.and.salaried.workers..female....of.female.employment...modeled.ILO.estimate.,
    waged_emp_male         = Wage.and.salaried.workers..male....of.male.employment...modeled.ILO.estimate.,
    waged_emp_total        = Wage.and.salaried.workers..total....of.total.employment...modeled.ILO.estimate.,
    net_trade              = Net.trade.in.goods.and.services..BoP..current.US.. 
  ) %>% 
  dplyr::mutate(
    log_net_trade_signed = sign(net_trade) * log1p(abs(net_trade)),
    pop_growth_sq = pop_growth^2,
  )

colnames(data_clean)

# now the data selection 

# helpers for transformations
safe_log   <- function(x) log(pmax(x, 1, na.rm = TRUE))
signed_log <- function(x) ifelse(is.na(x), NA_real_,
                                 sign(x) * log1p(abs(x)))

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

# --- facet histograms for all numeric columns (data "imbalance"/distribution check) ---
library(ggplot2)
data1 %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distributions of selected variables", x = NULL, y = "Count")

## 2) Q1 — OLS with singular.ok = FALSE (must include Net trade)
## Base formula without nonlinearity
form_base <- gdp_growth ~ log_gdppc_ppp + pop_growth + age_dependency_ratio +
  life_exp_male + urban_pop_pct + log_net_trade_signed + log_export_value_idx +
  unemp_total + credit_info_index + internet_users

ols_fit <- try(lm(form_base, data = data1, singular.ok = FALSE), silent = TRUE)
if(inherits(ols_fit, "try-error")){
  ## Drop aliased columns to enforce full rank if needed
  mm <- model.matrix(form_base, data = data1)
  keep <- qr(mm)$pivot[seq_len(qr(mm)$rank)]
  keep_vars <- setdiff(colnames(mm)[keep], "(Intercept)")
  form_base <- as.formula(paste("gdp_growth ~", paste(keep_vars, collapse = " + ")))
  ols_fit <- lm(form_base, data = data1, singular.ok = FALSE)
}
cat("\nQ1: OLS (with Net trade) summary \n")
print(summary(ols_fit))

## 3) Q2 — Add a nonlinear effect (squared Net trade) & diagnostics 
form_plus <- update(form_base, . ~ . + I(log_net_trade_signed^2))
ols_plus  <- lm(form_plus, data = data1, singular.ok = FALSE)
cat("\nQ2: OLS + nonlinearity (net trade^2) summary \n")
print(summary(ols_plus))

## 4) Q3 — LASSO with same variables as Q2  
x_mat <- model.matrix(form_plus, data = data1)[, -1, drop = FALSE]
y_vec <- data1$gdp_growth

set.seed(222)
cv_lasso <- cv.glmnet(x_mat, y_vec, alpha = 1, family = "gaussian", nfolds = 10)
cat("\nQ3: LASSO λ_min & λ_1se \n")
print(c(lambda.min = cv_lasso$lambda.min, lambda.1se = cv_lasso$lambda.1se))
cat("\nLASSO nonzero coefficients at λ_min:\n")
print(coef(cv_lasso, s = "lambda.min"))

## 5) Q4 — Why standardize? simple demo 
set.seed(222)
cv_lasso_std    <- cv_glmnet <- cv_lasso                  # already standardized (default)
cv_lasso_nostd  <- cv.glmnet(x_mat, y_vec, alpha = 1, family = "gaussian",
                             nfolds = 10, standardize = FALSE)
cat("\nQ4: CV error (with std vs no-std) \n")
print(c(with_std = min(cv_lasso_std$cvm), no_std = min(cv_lasso_nostd$cvm)))

## 6) Q6 — Train/Test split 110/40 
set.seed(2025)
idx   <- sample(seq_len(nrow(data1)), size = 110)
train <- data1[idx, , drop = FALSE]
test  <- data1[-idx, , drop = FALSE]  # will be 40 when n = 150

x_tr <- model.matrix(form_plus, data = train)[, -1, drop = FALSE]
y_tr <- train$gdp_growth
x_te <- model.matrix(form_plus, data = test)[, -1, drop = FALSE]
y_te <- test$gdp_growth

## 7) Q7 — Ridge & Elastic Net with CV (optimize α for EN) 
# Ridge
set.seed(2025)
cv_ridge <- cv.glmnet(x_tr, y_tr, alpha = 0, family = "gaussian", nfolds = 10)
ridge_min <- glmnet(x_tr, y_tr, alpha = 0, lambda = cv_ridge$lambda.min)
ridge_1se <- glmnet(x_tr, y_tr, alpha = 0, lambda = cv_ridge$lambda.1se)

# Elastic Net: search alpha grid
alphas <- seq(0, 1, by = 0.1)
cv_grid <- purrr::map_dfr(alphas, function(a){
  set.seed(2025)
  cv <- cv.glmnet(x_tr, y_tr, alpha = a, family = "gaussian", nfolds = 10)
  tibble(alpha = a,
         lambda_min = cv$lambda.min,
         cvm_min = min(cv$cvm),
         lambda_1se = cv$lambda.1se,
         cvm_1se = cv$cvm[match(cv$lambda.1se, cv$lambda)],
         cv_obj = list(cv))
})
best_alpha <- cv_grid$alpha[which.min(cv_grid$cvm_min)]
cv_en_best <- cv_grid$cv_obj[[which.min(cv_grid$cvm_min)]]
en_min <- glmnet(x_tr, y_tr, alpha = best_alpha, lambda = cv_en_best$lambda.min)
en_1se <- glmnet(x_tr, y_tr, alpha = best_alpha, lambda = cv_en_best$lambda.1se)

cat("\nQ7: Best alpha for Elastic Net \n"); print(best_alpha)
# so ridge = 0 works best

## 8) Q8 — Test RMSE comparison for λ_min and λ_1se 
rmse_vec <- function(truth, estimate) {
  sqrt(mean((truth - estimate)^2, na.rm = TRUE))
}

pred_ridge_min <- as.numeric(predict(ridge_min, x_te))
pred_ridge_1se <- as.numeric(predict(ridge_1se, x_te))
pred_en_min    <- as.numeric(predict(en_min,    x_te))
pred_en_1se    <- as.numeric(predict(en_1se,    x_te))

res_reg <- tibble(
  model = c("ridge_min","ridge_1se",
            paste0("elastic_min_a=", round(best_alpha,2)),
            paste0("elastic_1se_a=", round(best_alpha,2))),
  rmse  = c(
    rmse_vec(y_te, pred_ridge_min),
    rmse_vec(y_te, pred_ridge_1se),
    rmse_vec(y_te, pred_en_min),
    rmse_vec(y_te, pred_en_1se)
  )
) %>% arrange(rmse)
cat("\nQ8: Test RMSE \n"); print(res_reg)

## 9) Q9 — (Write-up point, no code) 1-SE rule vs overfitting 

## 10) Q10 — (Write-up) Why regularization vs OLS drop-insig vars 

## 11) Q11 — Create binary Growing more (>= 2.7%) 
dat_cls <- data1 %>% mutate(growing_more = as.integer(gdp_growth >= 2.7))

## 12) Q12 — Logistic Ridge (binomial) with CV; compare λs
set.seed(3030)
dat_cls <- data1 |> transform(growing_more = as.integer(gdp_growth >= 2.7))

p <- 110/(110+40)
idx <- unlist(lapply(split(seq_len(nrow(dat_cls)), dat_cls$growing_more),
                     function(i) sample(i, ceiling(length(i)*p))))
tr2 <- dat_cls[idx, ];  te2 <- dat_cls[-idx, ]

x_tr2 <- model.matrix(form_plus, data = tr2)[, -1, drop = FALSE]
y_tr2 <- tr2$growing_more
x_te2 <- model.matrix(form_plus, data = te2)[, -1, drop = FALSE]
y_te2 <- te2$growing_more

set.seed(3030)
cv_ridge_log <- cv.glmnet(x_tr2, y_tr2, alpha = 0, family = "binomial", nfolds = 10)
ridge_log_min <- glmnet(x_tr2, y_tr2, alpha = 0, family = "binomial", lambda = cv_ridge_log$lambda.min)
ridge_log_1se <- glmnet(x_tr2, y_tr2, alpha = 0, family = "binomial", lambda = cv_ridge_log$lambda.1se)
# --- show CV-chosen lambdas and surviving coefs ---
cat("\nLambdas: "); print(c(lambda.min = cv_ridge_log$lambda.min,
                            lambda.1se = cv_ridge_log$lambda.1se))
cat("\nNon-zero coefs at lambda.min:\n"); print(coef(cv_ridge_log, s = "lambda.min"))
cat("\nNon-zero coefs at lambda.1se:\n"); print(coef(cv_ridge_log, s = "lambda.1se"))

# --- predict on test set ---
p_min <- as.numeric(predict(ridge_log_min, x_te2, type = "response"))
p_1se <- as.numeric(predict(ridge_log_1se, x_te2, type = "response"))

pred_min <- ifelse(p_min >= 0.5, 1L, 0L)
pred_1se <- ifelse(p_1se >= 0.5, 1L, 0L)

# --- simple metrics ---
accuracy <- function(truth, pred) mean(truth == pred)

cat("\nTest accuracy:\n")
print(data.frame(
  model    = c("ridge_logit_min","ridge_logit_1se"),
  accuracy = c(accuracy(y_te2, pred_min),
               accuracy(y_te2, pred_1se))
))

cat("\nConfusion matrix (lambda.min):\n");  print(table(Truth = y_te2, Pred = pred_min))
cat("\nConfusion matrix (lambda.1se):\n"); print(table(Truth = y_te2, Pred = pred_1se))

## 13) Q13 — Different split once; compare predictive perf 
## Q13 — Alternative split with base R

set.seed(4040)   # new seed for a different split
n_train <- 110
n_test  <- 40

# 1) draw new indices
idx_alt <- sample(seq_len(nrow(data1)), size = n_train)
train_alt <- data1[idx_alt, , drop = FALSE]
test_alt  <- data1[-idx_alt, , drop = FALSE]

# 2) model matrices (same form_plus with nonlinearity)
x_tr_alt <- model.matrix(form_plus, data = train_alt)[, -1, drop = FALSE]
y_tr_alt <- train_alt$gdp_growth
x_te_alt <- model.matrix(form_plus, data = test_alt)[, -1, drop = FALSE]
y_te_alt <- test_alt$gdp_growth

# 3) Ridge CV + refit at lambda.min
set.seed(4040)
cv_ridge_alt <- cv.glmnet(x_tr_alt, y_tr_alt, alpha = 0, family = "gaussian", nfolds = 10)
ridge_alt_min <- glmnet(x_tr_alt, y_tr_alt, alpha = 0, lambda = cv_ridge_alt$lambda.min)

# 4) Predictions + RMSE on test
pred_alt <- as.numeric(predict(ridge_alt_min, x_te_alt))
rmse_alt <- sqrt(mean((y_te_alt - pred_alt)^2))

cat("\nQ13 — Alternative split test RMSE (ridge_min):", rmse_alt, "\n")


## 14) (Optional) Small tables you can paste in appendix 
coef_table <- function(cvfit, s, label){
  cf <- coef(cvfit$glmnet.fit, s = s)
  tibble(term = rownames(as.matrix(cf)), estimate = as.numeric(cf)) %>%
    filter(term != "(Intercept)") %>%
    arrange(desc(abs(estimate))) %>%
    mutate(model = label, .before = 1)
}
ridge_tbl_min <- coef_table(cv_ridge, s = cv_ridge$lambda.min, label = "ridge_min")
en_tbl_min    <- coef_table(cv_en_best, s = cv_en_best$lambda.min,
                            label = paste0("elastic_min_a=", round(best_alpha,2)))
cat("\n=== Top coefficients (head) ===\n")
print(head(ridge_tbl_min, 10))
print(head(en_tbl_min, 10))
