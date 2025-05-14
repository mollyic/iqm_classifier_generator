#----------------------------------------------------
#INPUT VALUES
#   * possible values
lst.seqs <- c("T2w", "FLAIR", "T1w")
lst.measures <- c("avg_motion", "avg_quality", "avg_flow_ghosting", "avg_susceptibility")
lst.pcas <- c(10, 15, 20)
cols.weights <-c('w_inverse','w_invsqr', 'unweighted')
lst.modes <- c('classification', 'regression')[1]
in.reg_metric <- ('rmse')
in.cls_metric <- ('f_meas') #roc_auc, pr_auc, f_meas, bal_accuracy, brier_class
in.models  = c('xgb', 'rf')#[2]

df.compare_ranks <- data.frame()
dir_paths <- list(script_dir ='work/run_files', 
                  console_dir = 'work/terminal_output',
                  models_dir ='results/models',
                  results_dir ='results/top', 
                  predictions = 'results/predictions'
                  )

for (dir_path in dir_paths) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

for (dgb_dir in c(dir_paths$models_dir, dir_paths$results_dir, dir_paths$predictions)){
  if (!dir.exists(paste0('debug/', dgb_dir))) {
    dir.create(paste0('debug/', dgb_dir), recursive = TRUE)
  }
}

for (in.seq in lst.seqs){                     #   * define sequence 
  for (in.measure in lst.measures){           #   * define rating type
    for (in.pcas in lst.pcas){              #   * define scaling type
      for (in.weights in cols.weights){       #   * define weighting type
        for (in.mode in lst.modes){       #   * define weighting type
          source('generate_runs.R')
        }
      }
    }
  }
}

