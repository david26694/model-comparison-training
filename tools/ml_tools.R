fit_model_split <- function(split, model){
  
  train_split <- analysis(split)
  predictors <- select_if(train_split, is.numeric)
  outcome <- pull(train_split, Class)
  
  
  model_fit <- fit_xy(model, predictors, outcome)
  model_fit
  
}


predict_model_split <- function(split, model){
  
  val_split <- assessment(split)
  predictors <- select_if(val_split, is.numeric)
  outcome <- select(val_split, Class)
  
  model_predict <- predict.model_fit(model, predictors, type = 'prob')
  
  bind_cols(outcome, model_predict)
  
}