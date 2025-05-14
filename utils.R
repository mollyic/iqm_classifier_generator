library(plyr); library(parsnip); library(tidymodels); library(rsample); library(themis);
library(recipes); library(tidyverse); library(ggplot2); library(xgboost); library(RColorBrewer);
library(pROC); library(vip);library(progressr);
library(ranger); library(parallel);library('doParallel');library(furrr); library(tictoc); library('glmnet')

#testing?
debug <- F
run_paral = T
run_future = F
dir_results <- 'results/'
sink_results <- F

#CV PARAMETERS
in.split <- 0.8
grid_size_linear <- 1000
in.cvfolds <- 10
in.cvreps <- 1
in.bootstraps <- 25
in.grid_size <- 50

if (isTRUE(debug)){
  in.grid_size <- 5
  grid_size_linear <- 10
  in.cvfolds <- 3
  in.cvreps <- 1
  in.bootstraps <-2
  dir_results <- 'debug/results/'
}

#----------------------------------------------------
#M3 PARAMETERS

if (isTRUE(run_paral)){
  #handlers("progress")
  cores_m3 <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK", unset = 1)[1])-1
  cat('\n\t *  Cluster Cores:', cores_m3, sep='')
  plan(multicore, workers = cores_m3) # plan(multicore) for Unix-based systems
  options(future.globals.maxSize = 5 * 1024^3)
  }

#----------------------------------------------------
#SINK FILE
#----------------------------------------------------
#DF DETAILS
cols.ids <- c("aep_id", "modality", "avg_motion", "avg_quality", "avg_flow_ghosting", "avg_susceptibility", "bids_name")
lst.mode_keys <- c('cls' ='classification', 'reg'='regression')

#----------------------------------------------------
#IN DATA
lst_dir <- list(models =paste0(dir_results, '/models'),
                results =paste0(dir_results, '/top'), 
                preds = paste0(dir_results, '/predictions'))


dir.input <- 'input/'
df.iqms <- read.csv(paste0(dir.input, 'radiolqa_classifier_ratings-all_desc-cleanMBConly.csv'))
exclude <- c("X", "size_x", "size_y", "size_z", "spacing_x", "spacing_y", "spacing_z", "summary_bg_p05")
df.iqms <- df.iqms[!(names(df.iqms) %in% exclude)]

#-----------------------------------------------
#Define dataframe parameters
#   * column names & keys
id.ratings <- ifelse(in.measure == 'avg_quality', 'quality', 'artifact')
col.fact <- paste0('factor_', sub('avg_','', in.measure))
col.rnd <- paste0('rnd_', sub('avg_','', in.measure))
#   * column names
df.seq <- df.iqms[df.iqms$modality == in.seq & !is.na(df.iqms[[in.measure]]), ]

#----------------------------------------------------
#DF VARIABLES
cols.ratings<- c(names(df.iqms)[grep('avg_', names(df.iqms))], 
                 names(df.iqms)[grep('rnd_', names(df.iqms))],
                 names(df.iqms)[grep('factor_', names(df.iqms))])
in.metrics <- names(df.iqms)[!(names(df.iqms) %in% c(cols.ids, cols.ratings, exclude))]

#----------------------------------------------------
#FILTER INPUT DATAFRAME
df.input <- df.seq
if(in.measure !='avg_quality'){
  df.input <- df.input[df.input[[in.measure]] <=4, ]
}

#----------------------------------------------------
#FACTORING
lst.labels <- list()
lst.labels[['artifact']] <- c('bad' =1, 'mild'=2, 'ok'=3, 'good'=4, 'outside'=5)
lst.labels[['quality']] <- c('bad' =1, 'mild'=2, 'ok'=3, 'good'=4, 'great' =5)
class.qual <- names(lst.labels[['quality']])
class.art <- names(lst.labels[['artifact']])

df.input <- df.input %>%
  mutate(
    across(
      .cols = c(factor_flow_ghosting, factor_susceptibility, factor_motion),
      .fns = ~ factor(.x, levels = class.art[class.art %in% unique(.x)])
    )
  )

df.input$factor_quality <- factor(
  df.input$factor_quality, 
  levels = unique(df.input$factor_quality)[order(match(unique(df.input$factor_quality), class.qual))])

#----------------------------------------------------
#WEIGHTING DATAFRAME
df.weights <- df.input %>%
  group_by(!!sym(col.fact)) %>%
  dplyr::summarise(count = n(), proportion = n() / nrow(df.input)) %>% 
  mutate(w_inverse = 1 / proportion,
         w_invsqr =w_inverse^2
  )

