#--------------------------------------------------------------------------------
#REGRESSION MODELS
# Model: XGBOOST
xgb_model <- boost_tree(
  trees = tune(),
  tree_depth = tune(), 
  min_n = tune(),
  loss_reduction = tune(),                    
  sample_size = tune(), 
  mtry = tune(),        
  learn_rate = tune()) %>%
  set_engine("xgboost",  keep.inbag=TRUE, scale = F)

xgb_grid <- grid_latin_hypercube(trees(), tree_depth(), min_n(), loss_reduction(), 
                                 sample_size = sample_prop(), mtry(range = c(1, in.pcas)),
                                 learn_rate(), size = in.grid_size)

# Model: RandomForest
rf_model <- rand_forest(
  mtry = tune(), 
  trees = tune(), 
  min_n = tune()) %>%
  set_engine("ranger", importance = "impurity", keep.inbag=TRUE)

rf_grid <- grid_latin_hypercube(trees(), min_n(), mtry(range = c(1, in.pcas)), size = in.grid_size)  

#   * Model: LASSO l1 regression
lasso_model <- linear_reg(
  penalty = tune(),
  mixture = 1) %>%
  set_engine("glmnet", scale = F, standardize = FALSE)
lasso_grid <- grid_latin_hypercube(penalty(), size = grid_size_linear)

#   * Model: elastic net regression
elastic_model <- linear_reg(
  penalty = tune(), 
  mixture = tune()) %>%
  set_engine("glmnet", scale = F)
elastic_grid <- grid_latin_hypercube(penalty(), mixture(), size = grid_size_linear/20)

#   * Model: ridge regression
ridge_model <- linear_reg(
  penalty = tune(), 
  mixture = 0) %>%
  set_engine("glmnet", scale = F)
ridge_grid <- lasso_grid

#--------------------------------------------------------------------------------

models_lst <- list(
    'xgb' = list('model' = xgb_model, 'grid' = xgb_grid), 
    'rf' = list('model' = rf_model, 'grid' = rf_grid), 
    'lasso' = list('model' = lasso_model, 'grid' = lasso_grid), 
    'elastic' = list('model' = elastic_model, 'grid' = elastic_grid), 
    'ridge' = list('model' = ridge_model, 'grid' = ridge_grid) 
)

func_getmodel <- function(eng_mode, model){
  model_traits <- models_lst[[model]]
  engine <-model_traits$model %>% set_mode(eng_mode)
  grid <- model_traits$grid
  return(list('engine'= engine, 'grid' = grid))
}

