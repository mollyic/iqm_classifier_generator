
if (in.mode == 'regression'){
  in.ivs = c(in.metrics, col.fact)
  in.dv = in.measure
  #in.metric = 'rmse'
  in.models  = names(models_lst)
  in.metric_set = metric_set(rmse, rsq, ccc)
} else {
  in.ivs = in.metrics
  in.dv = col.fact
  # in.metric = 'roc_auc'
  in.models  = c('rf')
  in.metric_set = metric_set(roc_auc, pr_auc,
                             f_meas, bal_accuracy, brier_class)
}

# Define formula
formula_preproc <- formula(paste(in.dv, '~', paste(in.ivs, collapse = ' + ')))
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
get_winner <- function(dat, engine, col_name = in.metric) {
  metric_info <- choose_metric(engine, in.metric, call = call)
  direction <- metric_info$direction
  metric_name <- metric_info$metric

  cat('\nPerformance metric: ', metric_name, '\n\t * Direction: ', direction)
  idx <- if (direction == 'minimize') which.min(dat[[col_name]]) else which.max(dat[[col_name]])
  return(idx)
}

func_comparemodels <- function(model) {
  
  cat('\nRunning cross-validation loop:\n\t * Model: ', model, '\n')
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
    
    if (in.weights != 'unweighted'){
      wf.model <-wf.model %>% add_case_weights(!!sym(in.weights))}
    # * Within each bootstrap, each model (HP configuration) is run 
    tune_results <- 
      wf.model %>% 
      tune_grid(
        resamples = object,       #list of bootstraps for a single fold
        grid = in_grid,           #model configurations (n models = grid_size)
        metrics = in.metric_set,
        control = control_grid(save_pred = TRUE, verbose = T)
        )
    return(tune_results) 
  }
  
  # Inner loop hyperparameter tuning
  #     * Within each bootstrap, each model (HP configuration) is run 
  tune_list <- map(results$inner_resamples, summarize_tune_results)
  best_tunes <- lapply(tune_list, show_best, metric = in.metric, n = 1)

  func_all_metrics <- function(sample_results, metric){
    # * Retrieve the best classifier across resamples for a single fold and append all performance metrics
    best_sample <- sample_results %>% select_best(metric = metric)
    fold_metrics <- sample_results %>% collect_metrics() 
    
    best_metrics <- fold_metrics%>%
      filter(.config == best_sample$.config) %>%
      select(-any_of(c('.estimator', 'n'))) %>%
      pivot_wider(values_from = c('mean', 'std_err'), 
                  names_from = c('.metric'), 
                  names_glue= c('cv_{.metric}.{.value}'))
    return(best_metrics)
  }
  # DF of best classifiers per fold with all performance metrics
  fold_metrics_lst <- map(tune_list, func_all_metrics, in.metric)
  fold_metrics_df <- bind_rows(fold_metrics_lst)
  
  func_lastfit <- function(in_params) {
    set.seed(345)
    
    final_wf <- workflow() %>%
      add_recipe(recipe_preproc) %>%
      add_model(in_engine) %>%
      finalize_workflow(in_params)
    
    if (in.weights != 'unweighted'){
      final_wf <-final_wf %>% add_case_weights(!!sym(in.weights))}
    
    final_fit <- final_wf %>% 
      last_fit(splt.predictor, metrics = in.metric_set)
    
    lf_metrics <- collect_metrics(final_fit)    %>%
      select(-any_of(c('.config'))) %>%
      pivot_wider(values_from = c('.estimate', '.estimator'), 
                  names_from = c('.metric'), 
                  names_glue= c('lastfit_{.metric}{.value}')) %>% 
      rename(!!sym(paste0('lastfit_', in.metric)) := sym(paste0('lastfit_', in.metric,'.estimate'))) %>%
      relocate(all_of(c(paste0('lastfit_', in.metric))), .before = 1)

    tmp_preds <- final_fit %>% collect_predictions()
    col_key <- names(tmp_preds)[grepl('.pred$|.pred_class$', names(tmp_preds))]

    tmp_bootstrap <- int_pctl(final_fit) %>%
      pivot_wider(names_from = .metric,
                  values_from = c(.lower, .upper, .estimate),
                  names_glue = '{.metric}{.value}CI') %>%
      select(any_of(ends_with('CI')))
    
    if (in.mode == 'regression'){
      pred_levels <- unique(round(tmp_preds[[col_key]]))
    } else {pred_levels <- unique(tmp_preds[[col_key]])}
    
    out_results <-  tmp.run_details %>% 
      bind_cols(in_params) %>%
      bind_cols(lf_metrics) %>% 
      bind_cols(tmp_bootstrap) %>% 
      mutate(model = model, .before = !!sym(names(in_params)[1])) %>%
      mutate(trn_lvls = paste(levels(train_dat[[col.fact]]), collapse = ", "),
             pred_lvls = paste(pred_levels, collapse = ", "),
             test_lvls = paste(levels(testing(splt.predictor)[[col.fact]]), collapse = ", "),
             trn_lvls_n = length(levels(train_dat[[col.fact]])),
             test_lvls_n = length(levels(testing(splt.predictor)[[col.fact]])),
             pred_lvls_n = length(pred_levels)
             )
    cat('\n\n______________________________\n Metrics and HPs for top ',model,' model within single fold:\n')
    cat('\n\n DF lf_metrics: \n')
    print(lf_metrics)
    cat('\n\n DF in_params: \n')
    print(in_params)
    cat('\n\n DF tmp_preds: \n')
    print(tmp_preds)
    cat('\n\n DF tmp_bootstrap: \n')
    print(tmp_bootstrap)
    cat('\n\n Columns with factor counts : \n')
    print(out_results[c('trn_lvls','pred_lvls','test_lvls', 
                        'test_lvls_n', 'trn_lvls_n', 'pred_lvls_n')])
    cat('\n\n out_results:')
    print(out_results %>% select(-all_of(c('trn_lvls','pred_lvls','test_lvls', 
                        'test_lvls_n', 'trn_lvls_n', 'pred_lvls_n'))))
    cat('\n\n______________________________')
    
    return(list('results' = out_results, 'final_fit' = final_fit))
  }
  
  lastfits_run <- map(best_tunes, func_lastfit)
  lastfits_res <- map(lastfits_run, 'results')
  lastfits_res <- bind_rows(lastfits_res)
  lastfits_res <- lastfits_res %>%
    left_join(fold_metrics_df[names(fold_metrics_df)[grep('cv_', names(fold_metrics_df))]], by = c("mean" = paste0("cv_", in.metric, '.mean')))
  
  win_idx <- get_winner(lastfits_res, 
                        engine = lastfits_run[[1]]$final_fit,
                        col_name = paste0('lastfit_', in.metric))

  win_params <-  lastfits_res[win_idx, ]
  win_eng <- lastfits_run[[win_idx]]$final_fit
  
  cat('\n\n\n * Last fit estimates for all ',model,' models from each fold:\n')
  print(lastfits_res)
  
  cat('\n Winning metric score for', model,' model: \n\t * ', paste0('lastfit_', in.metric), ': ', 
      win_params[[paste0('lastfit_', in.metric)]], '\n\n')
  print(extract_workflow(win_eng))
  return(list('model' = win_eng, 'params' = win_params))
}

cat('\n\nRunning models: ', in.models, '\n')

if (isTRUE(run_future)){
  #progressr: progress during parallel execution
  model_results <- #with_progress({
  #p <- progressor(steps = length(in.models))
  future_map(in.models, ~{
    cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~\nRunning: ', .x, '\n')
    #tic()
    tmp_df <- func_comparemodels(.x)
    tmp_df$params$model <- .x
    #toc()
    #p()
    cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
    return(tmp_df)
  }, .options = furrr_options(seed = 123, stdout = TRUE)) # Ensure reproducibility with seed
#}, enable = TRUE)
} else {
#SEQUENTIAL
model_results <- list()  # Initialize an empty list to store results
for (i in seq_along(in.models)) {
    model_name <- in.models[[i]]
    cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~\nRunning: ', model_name, '\n')
    #tic()
    tmp_df <- func_comparemodels(model_name)
    tmp_df$params$model <- model_name
    #toc()
    #p()  # Update the progress bar
    cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
    model_results[[i]] <- tmp_df
  }
}
# Combine results
model_results <- set_names(model_results, in.models)
models_res <- map(model_results, 'params')
models_res <- bind_rows(models_res)

cat('\n\n\n * Best models from each model type:\n')
print(models_res %>% select(-any_of(c('.config')))) 

win_idx <- get_winner(models_res, 
                      engine = model_results[[in.models[[1]]]]$model,
                      col_name = paste0('lastfit_', in.metric))
models_res$winner <- 0
models_res[win_idx,]$winner <- 1


win_params <- models_res #models_res[win_idx,]
win_eng <- model_results[[models_res[win_idx,]$model]]$model
win_preds <- win_eng %>% collect_predictions()

#save models
for (model_name in names(model_results)){
  file.rds_base <- paste0(toupper(in.mode), '_model-', model_name, '_', fileout.model)
  
  tmp_eng <- model_results[[model_name]]$model
  saveRDS(tmp_eng, paste(lst_dir$models, file.rds_base, sep = '/'))
  cat('\n\nSaving ', toupper(model_name), 'classifier:', '\n\t * ', file.rds_base, '\n\n')
}

cat('\n\n\n * Best models from each model type:\n')
print(models_res)
cat('\n\n\n * Overall winning model:\n')
print(extract_workflow(win_eng))

cat('\nPREDICTIONS:\n\n')
print(win_preds %>% select(any_of(c('.pred','.pred_class', col.fact, in.measure))), n = 10)
