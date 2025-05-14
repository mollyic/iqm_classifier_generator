
gc()
start <-Sys.time()

cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~\n', toupper(in.seq) , ' workflow:',
    '\n\t * Rating:                 ', in.measure,
    '\n\t * Mode:                   ', in.mode,
    '\n\t * Weights:                ', in.weights,
    '\n\t * Grid:                   ', in.grid_size,
    '\n\t * Folds:                  ', in.cvfolds,
    '\n\t * CV reps:                ', in.cvreps,
    '\n\nRun details:',
    '\n\t * Debug:                  ', debug,
    '\n\t * Parallel:               ', run_paral,
    '\n\n START TIME:   ', format(start, "%X (Date: %b %d)"), 
    '\n\n~~~~~~~~~~~~~~~~~~~~~~~~~\n',
    sep='')
tmp.run_details <- data.frame(seq = in.seq, 
                              pccomps = in.pcas, 
                              cv_folds = in.cvfolds,
                              cv_reps = in.cvreps, 
                              grid_size_linear = grid_size_linear, 
                              pred_grid = in.grid_size)

#--------------------------------------------------------------------------------
#Data frame formatting
ind_vars <- c('bids_name', in.metrics)
#   * independent variables
col.fact <- paste0('factor_', sub('avg_','', in.measure))
#   * factor name
df.input <- func.format_df(df.iqms, in.seq, in.measure, col.fact, in.weights)
#   * format df for sequence and rating type

#args.feature_weights <- list()
if(in.weights != 'unweighted'){
  ind_vars <- c(in.weights, ind_vars)
  #args.feature_weights = list(case_weights = in.weights)
}

#--------------------------------------------------------------------------------
# Train-test split
set.seed(123)
# * first train-test split
splt.predictor <- initial_split(df.input, prop = in.split, strata = col.fact)
# * train subset for nested cv
train_dat <- training(splt.predictor)
# * nested cross-validation data splitting
results <- nested_cv(train_dat, outside = vfold_cv(v = in.cvfolds, 
                                                   repeats = in.cvreps, 
                                                   strata =col.fact), 
                     inside = bootstraps(times = in.bootstraps))
#--------------------------------------------------------------------------------
# Run model training workflow
source('modules/workflow.R')

df.top_models <- tmp.run_details %>% bind_cols(win_params) 
df.predictor_preds <-tmp.run_details %>% bind_cols(win_preds) 

# * write out results 
write.csv(df.top_models, paste(lst_dir$results, fileout.results, sep='/'))
write.csv(df.predictor_preds, paste(lst_dir$preds, fileout.preds, sep ='/'))

end_time <-  Sys.time() - start
time_unit <- attr(end_time, "units")
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~\n', toupper(in.seq) , ' workflow completed:',
    '\n\t * Rating:   ', in.measure,
    '\n\t * Weights:   ', in.weights,
    '\n\nStart:   ', format(start, "%X (Date: %b %d)"),
    '\nEnd:   ', format(Sys.time(), "%X (Date: %b %d)"),
    '\nDURATION:   ', paste(round(end_time, 2), time_unit),
    '\n~~~~~~~~~~~~~~~~~~~~~~~~~\n', sep ='')
