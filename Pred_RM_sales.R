### Load data ---- 

a <- read_excel("C:/.../Pred_RM_sales_data.xlsx")



### Data preparation ----

# Split dataset into 80% training and 20% test data
set.seed(321)
a_split <- initial_split(a, prop = .8)
a_train <- training(a_split)
a_test  <- testing(a_split)

# Shuffle training data
set.seed(321)
rows <- sample(nrow(a_train))
a_train <- a_train[rows, ]

# Shuffle test data
set.seed(321)
rows <- sample(nrow(a_test))
a_test <- a_test[rows, ]

# Create matrix for training data 
a_train_rms <- model.matrix(log(a_train$ja_2019) ~ ja_2019 + 
                              log(umsatzteil) +
                              log(kuehe) + 
                              log(alter_BL) + 
                              log(inbetrieb) +  
                              log(Milchpreis_2019_B) +
                              log(Pachtentgelt_2010) +
                              bio + 
                              log(preis) + 
                              produkte + 
                              gmaps + 
                              online + 
                              zeitung + 
                              schild + 
                              fb + 
                              hpage + 
                              automaten + 
                              hofladen + 
                              vertrauen + 
                              modell + 
                              immerauf +
                              s_stadt + 
                              s_touris + 
                              s_nah + 
                              strasse + 
                              s_pendel +
                              Ländlichkeit_2016g + 
                              log(Milchviehbetriebe_2016) + 
                              log(Sek1_2019) + 
                              log(Durchschnittsalter_2019) + 
                              log(bquali_unifh_2020) +
                              log(EinkommenpP) +
                              log(Fahrzeit_Smarkt_2017) + 
                              log(automatendichte) +
                              log(dichte_G)
                            ,data = a_train)[, -1]

# Create matrix for test data 
a_test_rms <- model.matrix(log(a_test$ja_2019) ~ ja_2019 +
                             log(umsatzteil) +
                             log(kuehe) + 
                             log(alter_BL) + 
                             log(inbetrieb) +  
                             log(Milchpreis_2019_B) +
                             log(Pachtentgelt_2010) +
                             bio + 
                             log(preis) + 
                             produkte + 
                             gmaps + 
                             online + 
                             zeitung + 
                             schild + 
                             fb + 
                             hpage + 
                             automaten + 
                             hofladen + 
                             vertrauen + 
                             modell + 
                             immerauf +
                             s_stadt + 
                             s_touris + 
                             s_nah + 
                             strasse + 
                             s_pendel +
                             Ländlichkeit_2016g + 
                             log(Milchviehbetriebe_2016) + 
                             log(Sek1_2019) + 
                             log(Durchschnittsalter_2019) + 
                             log(bquali_unifh_2020) +
                             log(EinkommenpP) +
                             log(Fahrzeit_Smarkt_2017) + 
                             log(automatendichte) +
                             log(dichte_G)
                           ,data = a_test)[, -1]

# Create numeric variable with target variable in training set
a_train_y <- log(a_train_rms[,1])

# Create numeric variable with target variable in test set
a_test_y <- log(a_test_rms[,1])

# Remove first column from training matrix
a_train_rms <- a_train_rms[,-1]

# Remove first column from test matrix
a_test_rms <- a_test_rms[,-1]



### Functions for evaluation ---- 

eval_results_xgb <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  rsq <- 1 - SSE / SST
  adjrsq <- 1 - (((1 - rsq) * (nrow(df)-1)) / (nrow(df) - ncol(df) - 1))
  RMSE <- sqrt(SSE/nrow(df))
  MAE <- abs(sum(predicted - true)/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    MAE = MAE,
    rsq = rsq,
    adjrsq = adjrsq
  )
}

eval_results_lasso <- function(true, predicted, df, moDel) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  rsq <- 1 - SSE / SST
  adjrsq <- 1 - (((1 - rsq) * (nrow(df)-1)) / (nrow(df) - nrow(filter(as.data.frame(as.matrix(coef(moDel, s = "lambda.1se"))), as.data.frame(as.matrix(coef(moDel, s = "lambda.1se")))[[1]]>0)) - 1))
  RMSE <- sqrt(SSE/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    rsq = rsq,
    adjrsq = adjrsq
  )
} 



### XGBoost algorithm ----

# Hyperparameter settings (after fine-tuning)
params <- list(
  eta = 0.01,
  max_depth = 1,
  min_child_weight = 1,
  subsample = 0.65,
  colsample_bytree = 1
)

# Run training algorithm
set.seed(123)
xgb_rms <- xgboost(params = params, nrounds = 2000, objective = "reg:linear", verbose = 1,
                   data = a_train_rms, label = a_train_y)

# Predict raw milk sales for training data
pred_xgb_rms_tr <- predict(xgb_rms, a_train_rms)

# Predict raw milk sales for test data
pred_xgb_rms_te <- predict(xgb_rms, a_test_rms)

# Calculate training accuracies
xgb_rms_tr <- eval_results_xgb(a_train_y, pred_xgb_rms_tr, a_train_rms)

# Calculate test accuracies
xgb_rms_te <- eval_results_xgb(a_test_y, pred_xgb_rms_te, a_test_rms)

# Show accuaracies of training and test accuracies
xgb_rms_tr # prediction accuracies for training set
xgb_rms_te # prediction accuracies for test set



### SHAP values ----

# Calculate SHAP values
shap_values <- shap.values(xgb_model = xgb_rms, X_train = a_train_rms)
shap_values$mean_shap_score # mean SHAP values

# Create SHAP value summary
shap_long <- shap.prep(xgb_model = xgb_rms, X_train = a_train_rms)
shap.plot.summary(shap_long)

# Create SHAP dependence plots for rurality and population density in one plot
g1 <- shap.plot.dependence(data_long = shap_long, x = 'Ländlichkeit_2016g', y = 'Ländlichkeit_2016g', color_feature = 'Column_WV') + 
  xlab("rurality") + ylab("SHAP value")
g2 <- shap.plot.dependence(data_long = shap_long, x = 'log(dichte_G)', y = 'log(dichte_G)', color_feature = 'Column_WV') +  
  xlab("Population density") + ylab("SHAP value")
gridExtra::grid.arrange(g1, g2, ncol = 2)

# Create SHAP dependence plots for supermarkt accessibility and annual income per capita in one plot
g4 <- shap.plot.dependence(data_long = shap_long, x = 'log(Fahrzeit_Smarkt_2017)', y = 'log(Fahrzeit_Smarkt_2017)', color_feature = 'Column_WV') +  
  xlab("accessibility of a supermarket") + ylab("SHAP value")
g5 <- shap.plot.dependence(data_long = shap_long, x = 'log(EinkommenpP)', y = 'log(EinkommenpP)', color_feature = 'Column_WV') +  
  xlab("annual income per capita in the county") + ylab("SHAP value")
gridExtra::grid.arrange(g4, g5, ncol = 2)



### Linear regression ----

# Convert matrix training data into dataframe
df_a_train_rms <- as.data.frame(a_train_rms)

# Convert matrix test data into dataframe
df_a_test_rms <- as.data.frame(a_test_rms)

# Create corresponding column names
newnames <- c("umsatzteil", "kuehe", "alter_BL", "inbetrieb", "Milchpreis_2019_B", "Pachtentgelt_2010", 
              "bio", "preis", "produkte", "gmaps", "online", "zeitung", "schild", "fb", "hpage", "automaten", "hofladen", "vertrauen", "modell", "immerauf",
              "s_stadt", "s_touris", "s_nah", "strasse", "s_pendel", "Ländlichkeit_2016g", "Milchviehbetriebe_2016", "Sek1_2019", "Durchschnittsalter_2019", "bquali_unifh_2020",
              "EinkommenpP", "Fahrzeit_Smarkt_2017", "automatendichte", "dichte_G")

# Add column names to training dataframe
colnames(df_a_train_rms) <- newnames

# Add column names to test dataframe
colnames(df_a_test_rms) <- newnames

# Combine training dataframe and target variable for training data
df_a_train_rms <- cbind(df_a_train_rms, a_train_y)

# Combine test dataframe and target variable for test data
df_a_test_rms <- cbind(df_a_test_rms, a_test_y)

# Estimate linear regression model
lm_model <- lm(a_train_y ~ 
                 umsatzteil +
                 kuehe + 
                 alter_BL + 
                 inbetrieb +  
                 Milchpreis_2019_B + 
                 Pachtentgelt_2010 +
                 bio + 
                 preis + 
                 produkte + 
                 gmaps + 
                 online +
                 zeitung + 
                 schild + 
                 fb + 
                 hpage + 
                 automaten + 
                 hofladen + 
                 vertrauen + 
                 modell + 
                 immerauf +
                 s_stadt + 
                 s_touris + 
                 s_nah + 
                 strasse + 
                 s_pendel +
                 Ländlichkeit_2016g +
                 Milchviehbetriebe_2016 + 
                 Sek1_2019 + 
                 Durchschnittsalter_2019 + 
                 bquali_unifh_2020 +
                 EinkommenpP +
                 Fahrzeit_Smarkt_2017 + 
                 automatendichte,
               df_a_train_rms)

# Show estimation results as overview
summary(lm_model)

# Predict raw milk sales for training data
pred_lm_tr <- predict(lm_model, newdata = df_a_train_rms)

# Predict raw milk sales for test data
pred_lm_te <- predict(lm_model, newdata = df_a_test_rms)

# Calculate training accuracies
lm_tr <- eval_results_lasso(as.numeric(a_train_y), pred_lm_tr, df_a_train_rms, lm_model)

# Calculate test accuracies
lm_te <- eval_results_lasso(as.numeric(a_test_y), pred_lm_te, df_a_test_rms, lm_model)

# Show accuracies of training and test accuracies
lm_tr # prediction accuracies for training set
lm_te # prediction accuracies for test set

# Calculate variance inflation factor
vif_values <- vif(lm_model)
vif_values[vif_values>5]



### LASSO regression ---- 

# Estimate LASSO regression
lasso_rms <- cv.glmnet(a_train_rms, a_train_y, alpha = 1)

# Predict raw milk sales for training data
pred_lasso_train_rms <- predict(lasso_rms, s = "lambda.min", newx = a_train_rms)

# Predict raw milk sales for test data
pred_lasso_test_rms <- predict(lasso_rms, s = "lambda.min", newx = a_test_rms)

# Calculate training accuracies
lasso_rms_tr <- eval_results_lasso(a_train_y, pred_lasso_train_rms, a_train_rms, lasso_rms)

# Calculate test accuracies
lasso_rms_te <- eval_results_lasso(a_test_y, pred_lasso_test_rms, a_test_rms, lasso_rms)

# Show accuracies of training and test accuracies
lasso_rms_tr # prediction accuracies for training set
lasso_rms_te # prediction accuracies for test set

# Calculate variable importance
imp_lasso_rms <- vi(lasso_rms)
imp_lasso_rms

# Show coefficients from model results
coef(lasso_rms)



### Robustness check SHAP values ----

## w/o 3 least important variables -> no effect
# vertrauen
# hofladen
# produkte

# Prepare training data as matrix
a_train_rms <- model.matrix(log(a_train$ja_2019) ~ ja_2019 + 
                              log(umsatzteil) +
                              log(kuehe) + 
                              log(alter_BL) + 
                              log(inbetrieb) +  
                              log(Milchpreis_2019_B) +
                              log(Pachtentgelt_2010) +
                              bio + 
                              log(preis) + 
                              # produkte + 
                              gmaps + 
                              online + 
                              zeitung + 
                              schild + 
                              fb + 
                              hpage +
                              automaten +
                              # hofladen + 
                              # vertrauen + 
                              modell + 
                              immerauf +
                              s_stadt + 
                              s_touris + 
                              s_nah + 
                              strasse + 
                              s_pendel +
                              Ländlichkeit_2016g + 
                              log(Milchviehbetriebe_2016) + 
                              log(Sek1_2019) + 
                              log(Durchschnittsalter_2019) + 
                              log(bquali_unifh_2020) +
                              log(EinkommenpP) +
                              log(Fahrzeit_Smarkt_2017) + 
                              log(automatendichte) +
                              log(dichte_G)
                            ,data = a_train)[, -1]

# Prepare test data as matrix
a_test_rms <- model.matrix(log(a_test$ja_2019) ~ ja_2019 +
                             log(umsatzteil) +
                             log(kuehe) + 
                             log(alter_BL) + 
                             log(inbetrieb) +  
                             log(Milchpreis_2019_B) +
                             log(Pachtentgelt_2010) +
                             bio + 
                             log(preis) + 
                             # produkte + 
                             gmaps + 
                             online + 
                             zeitung + 
                             schild + 
                             fb + 
                             hpage +
                             automaten +
                             # hofladen + 
                             # vertrauen +
                             modell + 
                             immerauf +
                             s_stadt + 
                             s_touris + 
                             s_nah + 
                             strasse + 
                             s_pendel +
                             Ländlichkeit_2016g + 
                             log(Milchviehbetriebe_2016) + 
                             log(Sek1_2019) + 
                             log(Durchschnittsalter_2019) + 
                             log(bquali_unifh_2020) +
                             log(EinkommenpP) +
                             log(Fahrzeit_Smarkt_2017) + 
                             log(automatendichte) +
                             log(dichte_G)
                           ,data = a_test)[, -1]

# Prepare target variable data
a_train_y <- log(a_train_rms[,1])
a_test_y <- log(a_test_rms[,1])
a_train_rms <- a_train_rms[,-1]
a_test_rms <- a_test_rms[,-1]

# Run training algorithm
set.seed(123)
xgb_rms <- xgboost(params = params, nrounds = 2000, objective = "reg:linear", verbose = 1,
                   data = a_train_rms, label = a_train_y)

# Predict raw milk sales
pred_xgb_rms_tr <- predict(xgb_rms, a_train_rms)
pred_xgb_rms_te <- predict(xgb_rms, a_test_rms)

# Calculate accuracies
xgb_rms_tr_least <- eval_results_xgb(a_train_y, pred_xgb_rms_tr, a_train_rms)
xgb_rms_te_least <- eval_results_xgb(a_test_y, pred_xgb_rms_te, a_test_rms)
xgb_rms_tr_least
xgb_rms_te_least

shap_values_least <- shap.values(xgb_model = xgb_rms, X_train = a_train_rms)
shap_values_least$mean_shap_score


### w/o 3 most important variables -> effect
# log(dichte)
# log(umsatzteil)
# log(Fahrzeit SMarkt)

# Prepare training data as matrix
a_train_rms <- model.matrix(log(a_train$ja_2019) ~ ja_2019 +
                              # log(umsatzteil) +
                              log(kuehe) + 
                              log(alter_BL) + 
                              log(inbetrieb) +  
                              log(Milchpreis_2019_B) +
                              log(Pachtentgelt_2010) +
                              bio + 
                              log(preis) + 
                              produkte + 
                              gmaps + 
                              online + 
                              zeitung + 
                              schild + 
                              fb + 
                              hpage +
                              automaten +
                              hofladen +
                              vertrauen +
                              modell + 
                              immerauf +
                              s_stadt + 
                              s_touris + 
                              s_nah + 
                              strasse + 
                              s_pendel +
                              Ländlichkeit_2016g + 
                              log(Milchviehbetriebe_2016) + 
                              log(Sek1_2019) + 
                              log(Durchschnittsalter_2019) + 
                              log(bquali_unifh_2020) +
                              log(EinkommenpP) +
                              # log(Fahrzeit_Smarkt_2017) + 
                              log(automatendichte)
                            # + log(dichte_G)
                            ,data = a_train)[, -1]

# Prepare test data as matrix
a_test_rms <- model.matrix(log(a_test$ja_2019) ~ ja_2019 +
                             # log(umsatzteil) +
                             log(kuehe) + 
                             log(alter_BL) + 
                             log(inbetrieb) +  
                             log(Milchpreis_2019_B) +
                             log(Pachtentgelt_2010) +
                             bio + 
                             log(preis) + 
                             produkte + 
                             gmaps + 
                             online + 
                             zeitung + 
                             schild + 
                             fb + 
                             hpage +
                             automaten +
                             hofladen +
                             vertrauen +
                             modell + 
                             immerauf +
                             s_stadt + 
                             s_touris + 
                             s_nah + 
                             strasse + 
                             s_pendel +
                             Ländlichkeit_2016g + 
                             log(Milchviehbetriebe_2016) + 
                             log(Sek1_2019) + 
                             log(Durchschnittsalter_2019) + 
                             log(bquali_unifh_2020) +
                             log(EinkommenpP) +
                             # log(Fahrzeit_Smarkt_2017) + 
                             log(automatendichte) #+
                           # log(dichte_G)
                           ,data = a_test)[, -1]

# Prepare target variable data
a_train_y <- log(a_train_rms[,1])
a_test_y <- log(a_test_rms[,1])
a_train_rms <- a_train_rms[,-1]
a_test_rms <- a_test_rms[,-1]

# Run training algorithm 
set.seed(123)
xgb_rms <- xgboost(params = params, nrounds = 2000, objective = "reg:linear", verbose = 1,
                   data = a_train_rms, label = a_train_y)

# Predict raw milk sales
pred_xgb_rms_tr <- predict(xgb_rms, a_train_rms)
pred_xgb_rms_te <- predict(xgb_rms, a_test_rms)

# Calculate accuracies
xgb_rms_tr_most <- eval_results_xgb(a_train_y, pred_xgb_rms_tr, a_train_rms)
xgb_rms_te_most <- eval_results_xgb(a_test_y, pred_xgb_rms_te, a_test_rms)
xgb_rms_tr_most
xgb_rms_te_most

# Calculate SHAP values
shap_values_most <- shap.values(xgb_model = xgb_rms, X_train = a_train_rms)
shap_values_most$mean_shap_score


### random placebo as confounder -> no effect

# Create random placebo in training and test data
set.seed(123)
a_train$placebo <- runif(nrow(a_train))
a_test$placebo <- runif(nrow(a_test))

# Prepare training data as matrix
a_train_rms <- model.matrix(log(a_train$ja_2019) ~ ja_2019 +
                              log(umsatzteil) +
                              log(kuehe) + 
                              log(alter_BL) + 
                              log(inbetrieb) +  
                              log(Milchpreis_2019_B) +
                              log(Pachtentgelt_2010) +
                              bio + 
                              log(preis) + 
                              produkte + 
                              gmaps + 
                              online + 
                              zeitung + 
                              schild + 
                              fb + 
                              hpage +
                              automaten +
                              hofladen +
                              vertrauen +
                              modell + 
                              immerauf +
                              s_stadt + 
                              s_touris + 
                              s_nah + 
                              strasse + 
                              s_pendel +
                              Ländlichkeit_2016g + 
                              log(Milchviehbetriebe_2016) + 
                              log(Sek1_2019) + 
                              log(Durchschnittsalter_2019) + 
                              log(bquali_unifh_2020) +
                              log(EinkommenpP) +
                              log(Fahrzeit_Smarkt_2017) +
                              log(automatendichte) +
                              log(dichte_G) +
                              placebo
                            ,data = a_train)[, -1]

# Prepare test data as matrix
a_test_rms <- model.matrix(log(a_test$ja_2019) ~ ja_2019 +
                             log(umsatzteil) +
                             log(kuehe) + 
                             log(alter_BL) + 
                             log(inbetrieb) +  
                             log(Milchpreis_2019_B) +
                             log(Pachtentgelt_2010) +
                             bio + 
                             log(preis) + 
                             produkte + 
                             gmaps + 
                             online + 
                             zeitung + 
                             schild + 
                             fb + 
                             hpage +
                             automaten +
                             hofladen +
                             vertrauen +
                             modell + 
                             immerauf +
                             s_stadt + 
                             s_touris + 
                             s_nah + 
                             strasse + 
                             s_pendel +
                             Ländlichkeit_2016g + 
                             log(Milchviehbetriebe_2016) + 
                             log(Sek1_2019) + 
                             log(Durchschnittsalter_2019) + 
                             log(bquali_unifh_2020) +
                             log(EinkommenpP) +
                             log(Fahrzeit_Smarkt_2017) +
                             log(automatendichte) +
                             log(dichte_G) + 
                             placebo
                           ,data = a_test)[, -1]

# Prepare target variable data
a_train_y <- log(a_train_rms[,1])
a_test_y <- log(a_test_rms[,1])
a_train_rms <- a_train_rms[,-1]
a_test_rms <- a_test_rms[,-1]

# Run training algorithm
set.seed(123)
xgb_rms <- xgboost(params = params, nrounds = 2000, objective = "reg:linear", verbose = 1,
                   data = a_train_rms, label = a_train_y)

# Predict raw milk sales
pred_xgb_rms_tr <- predict(xgb_rms, a_train_rms)
pred_xgb_rms_te <- predict(xgb_rms, a_test_rms)

# Calculate accuracies
xgb_rms_tr_con <- eval_results_xgb(a_train_y, pred_xgb_rms_tr, a_train_rms)
xgb_rms_te_con <- eval_results_xgb(a_test_y, pred_xgb_rms_te, a_test_rms)
xgb_rms_tr_con
xgb_rms_te_con

# Calculate SHAP values
shap_values_con <- shap.values(xgb_model = xgb_rms, X_train = a_train_rms)
shap_values_con$mean_shap_score


### random placebo for outcome variable -> effect

# Prepare training data as matrix
a_train_rms <- model.matrix(log(a_train$placebo) ~ placebo +
                              log(umsatzteil) +
                              log(kuehe) + 
                              log(alter_BL) + 
                              log(inbetrieb) +  
                              log(Milchpreis_2019_B) +
                              log(Pachtentgelt_2010) +
                              bio + 
                              log(preis) + 
                              produkte + 
                              gmaps + 
                              online + 
                              zeitung + 
                              schild + 
                              fb + 
                              hpage +
                              automaten +
                              hofladen +
                              vertrauen +
                              modell + 
                              immerauf +
                              s_stadt + 
                              s_touris + 
                              s_nah + 
                              strasse + 
                              s_pendel +
                              Ländlichkeit_2016g + 
                              log(Milchviehbetriebe_2016) + 
                              log(Sek1_2019) + 
                              log(Durchschnittsalter_2019) + 
                              log(bquali_unifh_2020) +
                              log(EinkommenpP) +
                              log(Fahrzeit_Smarkt_2017) +
                              log(automatendichte) +
                              log(dichte_G) +
                              placebo
                            ,data = a_train)[, -1]

# Prepare test data as matrix
a_test_rms <- model.matrix(log(a_test$placebo) ~ placebo +
                             log(umsatzteil) +
                             log(kuehe) + 
                             log(alter_BL) + 
                             log(inbetrieb) +  
                             log(Milchpreis_2019_B) +
                             log(Pachtentgelt_2010) +
                             bio + 
                             log(preis) + 
                             produkte + 
                             gmaps + 
                             online + 
                             zeitung + 
                             schild + 
                             fb + 
                             hpage +
                             automaten +
                             hofladen +
                             vertrauen +
                             modell + 
                             immerauf +
                             s_stadt + 
                             s_touris + 
                             s_nah + 
                             strasse + 
                             s_pendel +
                             Ländlichkeit_2016g + 
                             log(Milchviehbetriebe_2016) + 
                             log(Sek1_2019) + 
                             log(Durchschnittsalter_2019) + 
                             log(bquali_unifh_2020) +
                             log(EinkommenpP) +
                             log(Fahrzeit_Smarkt_2017) +
                             log(automatendichte) +
                             log(dichte_G) + 
                             placebo
                           ,data = a_test)[, -1]

# Prepare target variable data
a_train_y <- log(a_train_rms[,1])
a_test_y <- log(a_test_rms[,1])
a_train_rms <- a_train_rms[,-1]
a_test_rms <- a_test_rms[,-1]

# Run training algorithm
set.seed(123)
xgb_rms <- xgboost(params = params, nrounds = 2000, objective = "reg:linear", verbose = 1,
                   data = a_train_rms, label = a_train_y)

# Predict of raw milk sales
pred_xgb_rms_tr <- predict(xgb_rms, a_train_rms)
pred_xgb_rms_te <- predict(xgb_rms, a_test_rms)

# Calculate accuracies
xgb_rms_tr_out <- eval_results_xgb(a_train_y, pred_xgb_rms_tr, a_train_rms)
xgb_rms_te_out <- eval_results_xgb(a_test_y, pred_xgb_rms_te, a_test_rms)
xgb_rms_tr_out
xgb_rms_te_out

# Calculate SHAP values
shap_values_out <- shap.values(xgb_model = xgb_rms, X_train = a_train_rms)
shap_values_out$mean_shap_score


### Compare R² results for different robustness tests
# Calculate R² for each robustness test
r_original <- cbind(xgb_rms_tr[3], xgb_rms_te[3])
r_least <- cbind(xgb_rms_tr[3], xgb_rms_te[3])
r_most <- cbind(xgb_rms_tr[3], xgb_rms_te[3])
r_placebo_con <- cbind(xgb_rms_tr_con[3], xgb_rms_te_con[3])
r_placebo_out <- cbind(xgb_rms_tr_out[3], xgb_rms_te_out[3])

# Combine R² to one dataframe
r_compare <- rbind(r_original, r_least, r_most, r_placebo_con, r_placebo_out)
rownames(r_compare) <- c("original", "least", "most", "placebo_con", "placebo_out")
colnames(r_compare) <- c("train", "test")
r_compare <- t(r_compare)
r_compare
