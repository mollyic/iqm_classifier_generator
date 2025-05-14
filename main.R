source('utils.R')
source('models.R')

gc()
start <-Sys.time()

if(isTRUE(sink_results)){sink(file.sink, split=TRUE)}
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
    '\n\t * Total scans:            ', nrow(df.seq),
    '\n\t * Filtered scans:         ', nrow(df.input),
    '\n\n START TIME:   ', format(start, "%X (Date: %b %d)"), 
    '\n\n~~~~~~~~~~~~~~~~~~~~~~~~~\n',
    sep='')
tmp.run_details <- data.frame(seq = in.seq, pccomps = in.pcas, cv_folds = in.cvfolds,
                              cv_reps = in.cvreps, grid_size_linear = grid_size_linear, 
                              pred_grid = in.grid_size)

ind_vars <- c('bids_name', in.metrics)
args.feature_weights <- list()
#############################################  
#WEIGHTING
#   * Create weighting col for ML
if(in.weights != 'unweighted'){
  lst.weights <- setNames(c(df.weights[[in.weights]]), df.weights[[col.fact]])
  df.input[[in.weights]] <- lst.weights[df.input[[col.fact]]]
  df.input <-df.input %>%
    mutate(!!sym(in.weights) := hardhat::importance_weights(!!sym(in.weights)))
  ind_vars <- c(in.weights, ind_vars)
  args.feature_weights = list(case_weights = in.weights)
}

#############################################  
#TRAIN-TEST SPLIT
#   * main split
##############################################  
#TRAIN-TEST SPLIT
set.seed(123)
splt.predictor <- initial_split(df.input, prop = in.split, strata = col.fact)
train_dat <- training(splt.predictor)

# Nested cross-validation
results <- nested_cv(train_dat, 
                     outside = vfold_cv(v = in.cvfolds, 
                                        repeats = in.cvreps, 
                                        strata =col.fact), 
                     inside = bootstraps(times = in.bootstraps))

source('nest_mix.R')
#_____________________________________________________________

df.top_models <- win_params
#df.predictor_preds <-tmp.run_details %>% bind_cols(win_preds) 


write.csv(df.top_models, paste(lst_dir$results, fileout.results, sep='/'))
#write.csv(df.predictor_preds, paste(lst_dir$preds, fileout.preds, sep ='/'))

end_time <-  Sys.time() - start
time_unit <- attr(end_time, "units")
cat('\n~~~~~~~~~~~~~~~~~~~~~~~~~\n', toupper(in.seq) , ' workflow completed:',
    '\n\t * Rating:   ', in.measure,
    '\n\t * Weights:   ', in.weights,
    '\n\t * Performance metric:   ', in.metric,
    '\n\nStart:   ', format(start, "%X (Date: %b %d)"),
    '\nEnd:   ', format(Sys.time(), "%X (Date: %b %d)"),
    '\nDURATION:   ', paste(round(end_time, 2), time_unit),
    '\n~~~~~~~~~~~~~~~~~~~~~~~~~\n', sep ='')

if(isTRUE(sink_results)){sink()}
