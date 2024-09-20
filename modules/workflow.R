
if (in.mode == 'regression'){
  in.ivs = c(in.metrics, col.fact)
  in.dv = in.measure
  in.metric = 'rmse'
  in.models  = names(models_lst)
} else {
  in.ivs = in.metrics
  in.dv = col.fact
  in.metric = 'roc_auc'
  in.models  = c('rf', 'xgb')
}

# Define formula
formula_preproc <- formula(paste(in.dv, '~', paste(c('bids_name', in.ivs), collapse = ' + ')))
recipe_preproc <- recipe(formula_preproc, data = train_dat)

if(in.mode == 'regression'){
  cat('\n______________________________\n MODE: ', in.mode, '\n\n______________________________')
  recipe_preproc <- recipe_preproc %>%
    update_role(paste0(col.fact), new_role = "ID")
}
# Define preprocessing recipe
recipe_preproc <- recipe_preproc %>%
  update_role(bids_name, new_role = "ID") %>%
  step_pca(all_predictors(), num_comp = in.pcas) %>%
  step_rm(all_predictors(), -starts_with("PC"))

# Find best cost
get_winner <- function(dat, col_name = in.metric){
  if (in.mode == 'regression') {
    y <- dat[which.min(dat[[col_name]]), ]
  } else {
    y <- dat[which.max(dat[[col_name]]), ]
  } 
  return(y)
}

func_comparemodels <- function(model) {
  #get model parameters 
  getmodel <- func_getmodel(in.mode, model = model)
  in_engine <- getmodel$engine
  in_grid <- getmodel$grid
  hyperparams <- names(in_grid)
  
  # Summarize tuning results
  summarize_tune_results <- function(object) {
    #get error for each HP configuration across a single bootstrap 
    set.seed(345)
    wf.model <- workflow() %>%
      add_recipe(recipe_preproc) %>%
      add_model(in_engine)
    
    tune_results <- 
      wf.model %>% 
      tune_grid(resamples = object, grid = in_grid,
        control <- control_grid(save_pred = TRUE, verbose = TRUE)
        )
    
    return(tune_results) 
  }
  
  # Inner loop hyperparameter tuning
  tune_list <- map(results$inner_resamples, summarize_tune_results)
  tuning_results <- map(tune_list, collect_metrics)
  
  best_tunes <- lapply(tune_list, show_best, metric = in.metric, n = 1)
  
  func_lastfit <- function(in_params) {
    final_wf <- workflow() %>%
      add_recipe(recipe_preproc) %>%
      add_model(in_engine) %>%
      finalize_workflow(in_params)
    final_fit <- final_wf %>% 
      last_fit(splt.predictor)
    
    final_metrics <- collect_metrics(final_fit)   
    in_params[[paste0('lastfit_', in.metric)]] <- final_metrics[final_metrics$.metric == in.metric, ]$.estimate
    return(list('results' = in_params, 'final_fit' = final_fit))
  }
  
  lastfits_run <- map(best_tunes, func_lastfit)
  lastfits_res <- map(lastfits_run, 'results')
  lastfits_res <- bind_rows(lastfits_res)
  
  win_params <- get_winner(lastfits_res, col_name = paste0('lastfit_', in.metric))
  
  if (in.mode == 'regression') {
    win_idx <- which.min(lastfits_res[[paste0('lastfit_', in.metric)]])
  } else {
    win_idx <- which.max(lastfits_res[[paste0('lastfit_', in.metric)]])
  } 
  
  win_eng <- lastfits_run[[win_idx]]$final_fit
  win_eng %>% collect_predictions()
  cat('\n Results: \n\t * ', paste0('lastfit_', in.metric), ': ', 
      win_params[[paste0('lastfit_', in.metric)]], 
      '\n\t * row count', nrow(win_eng %>% collect_predictions()), ': \n\n')
  return(list('model' = win_eng, 'params' = win_params))
}

# progressr: progress during parallel execution
model_results <- with_progress({
  p <- progressor(steps = length(in.models))
  future_map(in.models, ~{
    cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~\nRunning: ', .x, '\n')
    tic()
    tmp_df <- func_comparemodels(.x)
    tmp_df$params$model <- .x
    toc()
    p()
    cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
    return(tmp_df)
  }, .options = furrr_options(seed = 123, stdout = TRUE)) # Ensure reproducibility with seed
}, enable = TRUE)


# Combine results
model_results <- set_names(model_results, in.models)
models_res <- map(model_results, 'params')
models_res <- bind_rows(models_res)

win_params <- get_winner(models_res, col_name = paste0('lastfit_', in.metric))
if (in.mode == 'regression') {
  win_idx <- which.min(models_res[[paste0('lastfit_', in.metric)]])
} else {
  win_idx <- which.max(models_res[[paste0('lastfit_', in.metric)]])
}

win_eng <- model_results[[win_params$model]]$model
win_preds <- win_eng %>% collect_predictions()

file.rds_base <- paste0(toupper(in.mode), '_model-', win_params$model, '_', fileout.model)
saveRDS(win_eng, paste(lst_dir$models, file.rds_base, sep = '/'))

cat('\nPREDICTIONS:\n\n')
print(win_preds[c(unlist(lapply(c(col.fact, in.measure), grep, names(win_preds), value =T)), 
                  names(win_preds)[1:5])], n = 50)

