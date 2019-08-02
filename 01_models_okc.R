

library(recipes)
library(rsample)
library(parsnip)
library(purrr)
library(yardstick)

source('tools/ml_tools.R')

summary(okc)
head(okc)

# Clean data frame ----------------------------------------------------------------------------

# Delete couple examples with height NA
okc <- okc %>% filter(!is.na(height))

# Fill diet with NAs
okc$diet <- coalesce(okc$diet, 'na_diet')

# Create features from dates and cathegorical variables
okc_features <- recipe(okc, Class ~ .) %>% 
  step_date(date) %>% 
  step_other(diet, location, date_dow) %>% 
  step_dummy(diet, location, date_month) %>% 
  prep() %>% 
  juice()

okc_features %>% summary()

# Split train/test ----------------------------------------------------------------------------

split <- initial_split(okc_features)
train <- training(split)
test <- testing(split)

folds <- rsample::vfold_cv(train, v = 10)


# Train models -------------------------------------------------------------------------------

xgb <- boost_tree(mode = 'classification') %>% 
  set_engine('xgboost')

rf <- rand_forest(mode = 'classification', trees = 100) %>% 
  set_engine('ranger', num.threads = 4, verbose = T)

lasso <- logistic_reg(mode = 'classification', penalty = 0.001, mixture = 1) %>% 
  set_engine('glmnet')



folds <- folds %>% 
  mutate(
    xgb_split = map(splits, fit_model_split, xgb),
    lasso_split = map(splits, fit_model_split, lasso),
    rf_split = map(splits, fit_model_split, rf)
  )

# Predict on validation -----------------------------------------------------------------------

# Also, compute metrics
folds <- folds %>% 
  mutate(
    predictions_xgb = map2(splits, xgb_split, predict_model_split),
    predictions_lasso = map2(splits, lasso_split, predict_model_split),
    predictions_rf = map2(splits, rf_split, predict_model_split),
    roc_xgb = map(predictions_xgb, ~ roc_auc(., Class, .pred_stem)),
    roc_lasso = map(predictions_lasso, ~ roc_auc(., Class, .pred_stem)),
    roc_rf = map(predictions_rf, ~ roc_auc(., Class, .pred_stem))
  )

# Obtain metric in folds ----------------------------------------------------------------------

aucs <- folds %>% 
  unnest(roc_xgb, roc_lasso, roc_rf) %>% 
  select(id, roc_auc_xgb = .estimate, roc_auc_lasso = .estimate1, roc_auc_rf = .estimate2)

save(aucs, file = 'data/aucs.RData')
