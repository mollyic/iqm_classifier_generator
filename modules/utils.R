library(pacman)
p_load(plyr, parsnip, tidymodels, rsample, recipes,
       tidyverse, furrr, progressr, tictoc)
source('config.R')
source('modules/models.R')


if (isTRUE(debug)){
  in.grid_size <- 3
  grid_size_linear <- 10
  in.cvfolds <- 2
  in.cvreps <- 1
  in.bootstraps <-4
  dir_results <- 'debug/results/'
}


#----------------------------------------------------
# Check if settings are configured for one classifier
check.one_run <- all(
  length(lst.seqs) == 1,
  length(lst.measures) == 1,
  length(lst.pcas) == 1,
  length(cols.weights) == 1,
  length(lst.modes) == 1
)

#----------------------------------------------------
#Cluster settings
if (isTRUE(run_paral)){
  handlers("progress")
  cores_m3 <- ifelse(isTRUE(debug), detectCores()-1, as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK", unset = 1)[1])-1)
  cat('\n\t *  Cluster Cores:', cores_m3, sep='')
  plan(multisession, workers = cores_m3 )# plan(multicore) for Unix-based systems
}

#----------------------------------------------------
#SINK FILE
#sink(file.sink, split=TRUE)
#----------------------------------------------------
#DF DETAILS
lst.mode_keys <- c('cls' ='classification', 'reg'='regression')
dot_brk <- '\n_______________________________________________________\n'
#----------------------------------------------------
#MRIQC iqm details 
cols.mriqc <- c('cjv', 'cnr', 'efc', 'fber', 'fwhm_avg', 'fwhm_x', 'fwhm_y', 
               'fwhm_z', 'icvs_csf', 'icvs_gm', 'icvs_wm', 'inu_med', 'inu_range', 
               'qi_1', 'qi_2', 'rpve_csf', 'rpve_gm', 'rpve_wm', 'size_x', 
               'size_y', 'size_z', 'snr_csf', 'snr_gm', 'snr_total', 'snr_wm', 
               'snrd_csf', 'snrd_gm', 'snrd_total', 'snrd_wm', 'spacing_x', 
               'spacing_y', 'spacing_z', 'summary_bg_k', 'summary_bg_mad', 
               'summary_bg_mean', 'summary_bg_median', 'summary_bg_n', 'summary_bg_p05', 
               'summary_bg_p95', 'summary_bg_stdv', 'summary_csf_k', 'summary_csf_mad', 
               'summary_csf_mean', 'summary_csf_median', 'summary_csf_n', 'summary_csf_p05', 
               'summary_csf_p95', 'summary_csf_stdv', 'summary_gm_k', 'summary_gm_mad', 
               'summary_gm_mean', 'summary_gm_median', 'summary_gm_n', 'summary_gm_p05', 
               'summary_gm_p95', 'summary_gm_stdv', 'summary_wm_k', 'summary_wm_mad', 
               'summary_wm_mean', 'summary_wm_median', 'summary_wm_n', 'summary_wm_p05', 
               'summary_wm_p95', 'summary_wm_stdv', 'tpm_overlap_csf', 'tpm_overlap_gm', 
               'tpm_overlap_wm', 'wm2max')
#   * all MRIQC metrics
exclude <- c("X", "size_x", "size_y", "size_z", "spacing_x", "spacing_y", "spacing_z", "summary_bg_p05")
in.metrics <- cols.mriqc[!(cols.mriqc %in% exclude)]
#   * filtered MRIQC metrics

#----------------------------------------------------
#Output folders and files
lst_dir <- list(script_dir ='work/run_files', 
                console_dir = 'work/terminal_output',
                models =paste0(dir_results, '/models'),
                results =paste0(dir_results, '/top'), 
                preds = paste0(dir_results, '/predictions'))

for (dir_path in lst_dir) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

df.iqms <- read.csv(csv.iqms)
df.iqms <- df.iqms[!(names(df.iqms) %in% exclude)]

#-----------------------------------------------
#Define dataframe parameters
#   * column names & keys
#id.ratings <- ifelse(in.measure == 'avg_quality', 'quality', 'artifact')
cols.ratings<- c(names(df.iqms)[grep('avg_', names(df.iqms))], 
                 names(df.iqms)[grep('factor_', names(df.iqms))])
#in.metrics <- names(df.iqms)[!(names(df.iqms) %in% c(cols.ids, cols.ratings, exclude))]

#----------------------------------------------------
# Factor rating labels
lst.labels <- list()
lst.labels[['artifact']] <- c('bad' =1, 'mild'=2, 'ok'=3, 'good'=4, 'outside'=5)
lst.labels[['quality']] <- c('bad' =1, 'mild'=2, 'ok'=3, 'good'=4, 'great' =5)
class.qual <- names(lst.labels[['quality']])
class.art <- names(lst.labels[['artifact']])

func.format_df <- function(df, sequence, rating, factor_col, weights){
  #df.input <- df.iqms[df.iqms$modality == in.seq & !is.na(df.iqms[[in.measure]]), ]
  
  if(rating !='avg_quality'){df_format <- df[df[[rating]] <=4, ]}
  df_format <- df %>%
    filter(modality == sequence, 
           !is.na(!!sym(rating))) %>%
    dplyr::mutate(
      across(
        .cols = c(factor_flow_ghosting, factor_susceptibility, factor_motion),
        .fns = ~ factor(.x, levels = class.art[class.art %in% unique(.x)])
      )
    )

  df_format$factor_quality <- factor(
    df_format$factor_quality, 
    levels = unique(df_format$factor_quality)[order(match(unique(df_format$factor_quality), class.qual))])
  
  if(weights != 'unweighted'){
    #   * Create weighting col for stratified sampling
    df_weights <- func.weight_df(df.iqms, factor_col)
  
    lst_weights <- setNames(c(df_weights[[weights]]), df_weights[[col.fact]])
    df_format[[weights]] <- lst_weights[df_format[[factor_col]]]
    df_format <-df_format %>%
      dplyr::mutate(!!sym(weights) := importance_weights(!!sym(weights)))
  }
  return(df_format)
}

func.weight_df <- function(df, factor_col){
  # function to determine weights for labels based on class frequency
  df_weights <- df %>%
    group_by(!!sym(factor_col)) %>%
    dplyr::summarise(count = n(), proportion = n() / nrow(df)) %>% 
    dplyr::mutate(
      w_inverse = 1 / proportion,
      w_invsqr = w_inverse^2)
  
  return(df_weights)
}