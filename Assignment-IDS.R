# Data Splitting
set.seed(254646); n1<-110; idx<-sample(nrow(df), n1)  # draw n1 rows
data_clean<-df; dfm_train<-data_clean[idx,]; dfm_test<-data_clean[-idx,]  # train/test
Xvars_mat_train<-model.matrix(`GDP growth (annual %)`~.-Country, data=dfm_train)[,-1]  # drop intercept
Yvar_train<-dfm_train$`GDP growth (annual %)`  # target train
Xvars_mat_test<-model.matrix(`GDP growth (annual %)`~.-Country, data=dfm_test)[,-1]   # drop intercept
Yvar_test<-dfm_test$`GDP growth (annual %)`    # target test

# OLS Regressions without trade and with trade
model <- lm(`GDP growth (annual %)`~`Fertility rate, total (births per woman)`+`Employers, total (% of total employment) (modeled ILO estimate)`
+`Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)` +`Life expectancy at birth, male (years)`
+`Unemployment, total (% of total labor force) (modeled ILO estimate)`+`Export value index (2000 = 100)`+`Population growth (annual %)`
+`GDP per capita, PPP (constant 2011 international $)`+`Net trade in goods and services (BoP, current US$)`
+`Wage and salaried workers, total (% of total employment) (modeled ILO estimate)`,data = df,singular.ok = FALSE)
model_without <- update(model, . ~ . - `Net trade in goods and services (BoP, current US$)`, singular.ok = FALSE)

# OLS Regression with non linear and interaction effects
df2<-within(df,{`GDP per capita, PPP (constant 2011 international $)^2`<-(`GDP per capita, PPP (constant 2011 international $)`)^2
`Population growth (annual %)^2`<-(`Population growth (annual %)`^2)
`Unemp x LFP`<-`Unemployment, total (% of total labor force) (modeled ILO estimate)`*`Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)`})
model_ext <- lm(`GDP growth (annual %)`~`Fertility rate, total (births per woman)`+`Employers, total (% of total employment) (modeled ILO estimate)`
+`Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)`+`Life expectancy at birth, male (years)`
+`Unemployment, total (% of total labor force) (modeled ILO estimate)`+`Export value index (2000 = 100)`
+`Population growth (annual %)`+`Population growth (annual %)^2` # nonlinear
+`GDP per capita, PPP (constant 2011 international $)`+`GDP per capita, PPP (constant 2011 international $)^2` # convergence
+`Wage and salaried workers, total (% of total employment) (modeled ILO estimate)`+`Unemp x LFP`,data = df2,singular.ok = FALSE)

models <-list("With Net Trade"=model,"Without Net Trade"=model_without,"Non-Linear Model"=model_ext) #putting in list
df_models <-lapply(names(models),function(name){augment(models[[name]]) %>% mutate(Model=name)}) %>% bind_rows() #tidy dataframe
gg_resfit <-ggplot(df_models, aes(.fitted, .resid, color = Model)) + geom_point(alpha=0.6)+
geom_hline(yintercept=0,linetype="dashed") + facet_wrap(~Model)+labs(title="Residuals vs Fitted Comparison",
x="Fitted values",y="Residuals")+theme_minimal() #Residuals vs Fitted plot 
gg_qq <- ggplot(df_models,aes(sample=.std.resid,color=Model))+stat_qq(alpha=0.6)+stat_qq_line()+facet_wrap(~Model)+
labs(title="Normal Q-Q",x ="Theory Quantiles",y="Standardized Residuals")+theme_minimal() # Normal Q-Q plot
gg_scale <- ggplot(df_models, aes(.fitted, sqrt(abs(.std.resid)),color=Model))+geom_point(alpha = 0.6)+
geom_smooth(se = FALSE, color = "black")+facet_wrap(~Model)+labs(title = "Scale-Location Comparison",
x="Fitted values",y="Sqrt(|Standardized Residuals|)")+theme_minimal() #Scale-Location plot

# LASSO Regression
X_lasso <- model.matrix(`GDP growth (annual %)`~`Fertility rate, total (births per woman)`+`Employers, total (% of total employment) (modeled ILO estimate)`+
`Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)`+`Life expectancy at birth, male (years)`+
`Unemployment, total (% of total labor force) (modeled ILO estimate)`+`Export value index (2000 = 100)`+
`Population growth (annual %)`+`Population growth (annual %)^2`+`GDP per capita, PPP (constant 2011 international $)`+
`GDP per capita, PPP (constant 2011 international $)^2`+`Net trade in goods and services (BoP, current US$)`+
`Wage and salaried workers, total (% of total employment) (modeled ILO estimate)`+`Unemp x LFP`, # Interaction
data = df2)[, -1] # drop intercept
y_lasso <- df2$`GDP growth (annual %)`
#Cross-validated LASSO 
set.seed(2354245);cv_lasso_nl_int<-cv.glmnet(X_lasso,y_lasso,alpha = 1,nfolds = 10,standardize = TRUE)
cv_lasso_nl_int$lambda.min;cv_lasso_nl_int$lambda.1se
coef(cv_lasso_nl_int, s = "lambda.min");coef(cv_lasso_nl_int, s = "lambda.1se")

set.seed(2134);cv_no_std <- cv.glmnet(X_lasso, y_lasso, alpha = 1, nfolds = 10, standardize = FALSE)
coef(cv_no_std, s = "lambda.min") # LASSO without standardization
set.seed(2134);cv_std <- cv.glmnet(X_lasso, y_lasso, alpha = 1, nfolds = 10, standardize = TRUE)
coef(cv_std, s = "lambda.min") # LASSO with standardization (default in glmnet)

coef_no_std<-as.matrix(coef(cv_no_std,s="lambda.min"));coef_std<-as.matrix(coef(cv_std,s="lambda.min"))
comparison <- data.frame(Variable = rownames(coef_no_std),`No Standardization` = round(coef_no_std[,1], 6),
Standardization = round(coef_std[,1], 6)) # Combining into a comparison table

# LASSO Grid search for lambda
set.seed(254646);lambda_grid<-seq(0.001,2,length.out=100)
cv_lasso<-cv.glmnet(Xvars_mat_train,Yvar_train,family="gaussian",alpha=1,lambda=lambda_grid,
nfolds = 10,type.measure="mse",standardize=TRUE)
best_lambda_min<-cv_lasso$lambda.min; best_lambda_1se<-cv_lasso$lambda.1se

coef_min<-as.matrix(coef(cv_lasso,s="lambda.min"));coef_1se<-as.matrix(coef(cv_lasso,s="lambda.1se"))
sel_min<-coef_min[coef_min[,1] !=0, ,drop=FALSE] #Get selected variables from extracted coefficients
sel_1se<-coef_1se[coef_1se[,1] != 0, ,drop=FALSE] #non-zero coefficients
df_min<-data.frame(Variable=rownames(sel_min),Coef_min=sel_min[,1]) #Create data frames
df_1se<-data.frame(Variable=rownames(sel_1se),Coef_1se=sel_1se[,1])
comparative_tbl<-merge(df_min,df_1se,by="Variable",all=TRUE) 

rmse<-function(y,p) sqrt(mean((y-p)^2)); mae<-function(y,p) mean(abs(y-p))
r2<-function(y,p) 1-sum((y-p)^2)/sum((y-mean(y))^2) # metrics functions
row_metrics<-function(name,y,p){data.frame(Model=name,RMSE=rmse(y,p),
MAE=mae(y,p),R2=r2(y,p),SD_y=sd(y))}
nz_coefs<-function(cv_obj,s){cf<-as.matrix(coef(cv_obj,s=s))
out<-data.frame(Variable=rownames(cf),Coef=cf[,1],row.names=NULL);subset(out,Coef!=0)}
# Lasso Predictions
pred_lasso_min <- as.numeric(predict(cv_lasso, newx = Xvars_mat_test, s = "lambda.min"))
pred_lasso_1se <- as.numeric(predict(cv_lasso, newx = Xvars_mat_test, s = "lambda.1se"))
met_lasso_min  <- row_metrics("Lasso (lambda.min)",  Yvar_test, pred_lasso_min)
met_lasso_1se  <- row_metrics("Lasso (lambda.1se)",  Yvar_test, pred_lasso_1se)
sel_lasso_min  <- nz_coefs(cv_lasso, "lambda.min"); sel_lasso_1se  <- nz_coefs(cv_lasso, "lambda.1se")
pred_base <- rep(mean(Yvar_train), length(Yvar_test)) # Baseline (mean)
met_base  <- row_metrics("Baseline (mean of train)", Yvar_test, pred_base)

# ELASTIC NET, grid search for alpha and training the model
set.seed(2546); alpha_grid<-seq(0.1,0.9,by=0.1); 
cv_list<-lapply(alpha_grid,function(a) cv.glmnet(Xvars_mat_train,Yvar_train,family="gaussian",
alpha=a,nfolds=10,type.measure="mse",standardize=TRUE)); cv_mins<-sapply(cv_list, function(cv) min(cv$cvm)); best_i<-which.min(cv_mins); best_a<-alpha_grid[best_i]; 
best_cv <- cv_list[[best_i]];cat("\nElastic Net — best alpha:",best_a,"| lambda.min:",signif(best_cv$lambda.min,4), "| lambda.1se:", signif(best_cv$lambda.1se, 4), "\n")

lam_tbl<-data.frame(Method=c("LASSO","LASSO"),Metric=c("$λ$.min","$λ$.1se"),
Value=c(signif(best_lambda_min,4),signif(best_lambda_1se,4))) # existing lasso table
elastic_tbl<-data.frame(Method=c("Elastic Net","Elastic Net","Elastic Net"),Metric=c("$α$","$λ$.min","$λ$.1se"),
Value=c(best_a,signif(best_cv$lambda.min,4),signif(best_cv$lambda.1se,4))) #adding elastic net results
lam_tbl <- rbind(lam_tbl, elastic_tbl) #combine 
knitr::kable(lam_tbl, caption = "Optimal tuning parameters for LASSO and Elastic Net") #display

# Elastic Net Predictions. 
pred_en_min<-as.numeric(predict(best_cv, newx = Xvars_mat_test, s = "lambda.min"))
pred_en_1se<-as.numeric(predict(best_cv, newx = Xvars_mat_test, s = "lambda.1se"))
met_en_min<-row_metrics(paste0("Elastic Net α=", best_a, " (lambda.min)"), Yvar_test, pred_en_min)
met_en_1se<-row_metrics(paste0("Elastic Net α=", best_a, " (lambda.1se)"), Yvar_test, pred_en_1se)
sel_en_min<-nz_coefs(best_cv, "lambda.min"); sel_en_1se<-nz_coefs(best_cv, "lambda.1se")

metrics_all<-rbind(met_base, met_lasso_min, met_lasso_1se, met_en_min, met_en_1se) #metric comparison
metrics_tbl<-metrics_all%>%dplyr::select(Model, RMSE, MAE, R2, SD_y) %>%
mutate(across(where(is.numeric),~round(.,3))); best_rmse_idx<-which.min(metrics_all$RMSE)
kable(metrics_tbl,caption="Test Performance (RMSE, MAE, $R²$, $SD_y$)")%>%
kable_styling(full_width=FALSE,position="center",bootstrap_options=c("striped","hover","condensed"))%>%
row_spec(best_rmse_idx, bold = TRUE)

top_k<-15 #Best features used
top_lasso_min <- head(sel_lasso_min[order(abs(sel_lasso_min$Coef), decreasing = TRUE), ], top_k)
top_lasso_1se <- head(sel_lasso_1se[order(abs(sel_lasso_1se$Coef), decreasing = TRUE), ], top_k)
top_en_min    <- head(sel_en_min[order(abs(sel_en_min$Coef),   decreasing = TRUE), ], top_k)
top_en_1se    <- head(sel_en_1se[order(abs(sel_en_1se$Coef),   decreasing = TRUE), ], top_k)
count_nz <- function(df) sum(df$Variable != "(Intercept)") # amount of variables selected in each model
cat("\n Amount of Variables -> Lasso min:", count_nz(sel_lasso_min),"| Lasso 1se:", count_nz(sel_lasso_1se),
   "| ENet min:", count_nz(sel_en_min), "| ENet 1se:", count_nz(sel_en_1se), "\n")
vars_lasso_min <-sel_lasso_min$Variable; vars_lasso_1se<-sel_lasso_1se$Variable
vars_en_min<-sel_en_min$Variable; vars_en_1se<-sel_en_1se$Variable # extract variable names
all_vars<-unique(c(vars_lasso_min,vars_lasso_1se,vars_en_min,vars_en_1se)) # combined into 1 master vector 
comparative_df<-data.frame(Variable=all_vars,L_min=ifelse(all_vars%in%vars_lasso_min,"T","F"),
L_1se=ifelse(all_vars %in% vars_lasso_1se,"T","F"),ENet_min=ifelse(all_vars %in% vars_en_min,"T","F"),
ENet_1se = ifelse(all_vars %in% vars_en_1se,"T","F"))

# Ridge and Logistic
df_logistic<-data_clean[ , !(names(data_clean) %in% "Country")] 
df_logistic$GrowingMore <- as.integer(df_logistic$`GDP growth (annual %)` > 2.7)
set.seed(1111); train_index<-sample.int(nrow(df_logistic),110) # Data Splitting (Seed One)
train_set<-df_logistic[train_index, ]; test_set  <- df_logistic[-train_index, ]
y_train<-train_set$GrowingMore; y_test<-test_set$GrowingMore
X_train <- model.matrix(GrowingMore ~ . -`GDP growth (annual %)`, data=train_set)[, -1]
X_test  <- model.matrix(GrowingMore ~ . - `GDP growth (annual %)`, data=test_set )[,-1]

cv_ridge<-cv.glmnet(X_train,y_train,family="binomial",alpha=0,nfolds=10,type.measure="deviance",standardize=TRUE)
lam_min<-cv_ridge$lambda.min; lam_1se<-cv_ridge$lambda.1se
cat("lambda.min =",signif(lam_min,4)," | lambda.1se =",signif(lam_1se,4), "\n")
# Prediction on Test Using both variables. 
p_min <- as.numeric(predict(cv_ridge, newx = X_test, s = "lambda.min", type = "response"))
p_1se <- as.numeric(predict(cv_ridge, newx = X_test, s = "lambda.1se", type = "response"))
pred_min <- ifelse(p_min >= 0.5, 1, 0); pred_1se <- ifelse(p_1se >= 0.5, 1, 0) # Classify at 0.5

logloss<-function(y,p){p<-pmin(pmax(p,1e-15),1-1e-15);-mean(y*log(p)+(1-y)*log(1-p))}
brier<-function(y,p) mean((p-y)^2) # Metrics for performance
acc_min<-mean(pred_min==y_test); acc_1se<-mean(pred_1se==y_test); ll_min<-logloss(y_test,p_min)
ll_1se<-logloss(y_test,p_1se); br_min<-brier(y_test,p_min); br_1se<-brier(y_test,p_1se)
get_auc<-function(y,p){if(requireNamespace("pROC",quietly=TRUE))as.numeric(pROC::auc(y,p))else NA_real_}
auc_min<-get_auc(y_test,p_min); auc_1se<-get_auc(y_test,p_1se) # optional AUC
results<-data.frame(Model=c("Ridge (lambda.min)","Ridge (lambda.1se)"),Accuracy=c(acc_min,acc_1se),
LogLoss=c(ll_min,ll_1se),Brier=c(br_min,br_1se),AUC=c(auc_min,auc_1se)); print(results,row.names=FALSE)

results_tbl<-results%>%transmute(Model,Accuracy=round(Accuracy,4),LogLoss=round(LogLoss,4),
Brier=round(Brier,4),AUC=ifelse(is.na(AUC),NA,round(AUC,3)))
kable(results_tbl,caption="Ridge (logistic) — Test Metrics")%>%
kable_styling(full_width=FALSE,position="center",bootstrap_options = c("striped","hover","condensed"))

# Final Model Coefficients, and example showing 10 largest (by |coef|) at lambda.1se
coef_min<-coef(cv_ridge,s="lambda.min"); coef_1se<-coef(cv_ridge,s="lambda.1se")
ix<-order(abs(as.numeric(coef_1se)),decreasing = TRUE); print(coef_1se[ix[1:10], ,drop=FALSE])

set.seed(9999); train_index<-sample.int(nrow(df_logistic),110) # Data Splitting (Seed Two)
train_set<-df_logistic[train_index, ]; test_set<-df_logistic[-train_index, ]
y_train<-train_set$GrowingMore; y_test<-test_set$GrowingMore
X_train<-model.matrix(GrowingMore ~ . -`GDP growth (annual %)`, data=train_set)[, -1]
X_test<-model.matrix(GrowingMore ~ . - `GDP growth (annual %)`, data=test_set )[,-1]

# Ridge and Elastic Net. 
cv_ridge<-cv.glmnet(X_train,y_train,family="binomial",alpha=0,nfolds = 10,type.measure="deviance",standardize=TRUE)
lam_min<-cv_ridge$lambda.min; lam_1se<-cv_ridge$lambda.1se
cat("lambda.min =", signif(lam_min,4), " | lambda.1se =", signif(lam_1se,4), "\n")
# Prediction on Test Using both variables. 
p_min <- as.numeric(predict(cv_ridge, newx = X_test, s = "lambda.min", type = "response"))
p_1se <- as.numeric(predict(cv_ridge, newx = X_test, s = "lambda.1se", type = "response"))
pred_min<-ifelse(p_min>=0.5,1,0); pred_1se<-ifelse(p_1se>=0.5,1,0) # Classify at 0.5

logloss<-function(y,p){p<-pmin(pmax(p,1e-15),1-1e-15); -mean(y*log(p)+(1-y)*log(1-p))} # Metrics for performance
brier<-function(y,p) mean((p-y)^2); acc_min<-mean(pred_min==y_test); acc_1se<-mean(pred_1se==y_test)
ll_min<-logloss(y_test,p_min); ll_1se<-logloss(y_test,p_1se); br_min<-brier(y_test,p_min); br_1se<-brier(y_test,p_1se)
get_auc<-function(y,p){if(requireNamespace("pROC",quietly=TRUE))as.numeric(pROC::auc(y, p))else NA_real_}
auc_min<-get_auc(y_test,p_min); auc_1se<-get_auc(y_test,p_1se) # optional AUC
results1<-data.frame(Model=c("Ridge (lambda.min)","Ridge (lambda.1se)"),Accuracy=c(acc_min,acc_1se),LogLoss=c(ll_min,ll_1se),
Brier=c(br_min,br_1se),AUC=c(auc_min,auc_1se)); print(results, row.names = FALSE)
results_tbl_1<-results1%>%transmute(Model,Accuracy=round(Accuracy,4),LogLoss=round(LogLoss,4),Brier=round(Brier,4),
AUC=ifelse(is.na(AUC),NA,round(AUC,3))); kable(results_tbl_1,caption="Ridge (logistic) — Test Metrics") %>% 
kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped","hover","condensed"))

coef_min<-coef(cv_ridge,s="lambda.min"); coef_1se<-coef(cv_ridge,s="lambda.1se")
ix<-order(abs(as.numeric(coef_1se)),decreasing=TRUE); print(coef_1se[ix[1:10], ,drop=FALSE])

tblA<-results_tbl_1%>% select(Model,Accuracy,LogLoss,Brier,AUC)%>% mutate(across(-Model,~round(.,4)))
tblB<-results_tbl%>% select(Model,Accuracy,LogLoss,Brier,AUC)%>% mutate(across(-Model,~round(.,4)))
wide<-inner_join(tblA,tblB,by="Model",suffix=c("_run1","_run2"))
kable(wide,col.names=c("Model","Accuracy","LogLoss","Brier","AUC","Accuracy","LogLoss","Brier","AUC"),
caption="Ridge (logistic) — Test Metrics (different seeds)") %>% kable_styling(full_width=FALSE,position="center",
bootstrap_options=c("striped","hover","condensed")) %>% add_header_above(c(" " =1, "Run 1" =4, "Run 2" =4))

kable(comparison[ , -1],caption="Comparison of LASSO Coefficients With and Without Standardization") # Pretty table
knitr::kable(comparative_df,caption="Variable selection across LASSO and Elastic Net") # Display nicely
knitr::kable(comparative_tbl, caption = "Variables Selected under λ.min vs λ.1se") # Show the table

print(gg_resfit); print(gg_qq); print(gg_scale)
