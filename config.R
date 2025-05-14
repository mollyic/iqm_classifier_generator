"
Input/output files, directories and parameters
  * configure settings for running classifier
"

dir_results <- 'results/'
# * final results directory
csv.iqms <- '/home/unimelb.edu.au/mollyi/Projects/repos/RADIOL_qa/code/classifier_work/data/merge_qaqc/radiolqa_classifier_ratings-all_desc-clean.csv'
# * input dataframe with mriqc results + ratings

#--------------------------------------------------------------------------------
#Classifier configuration (e.g. T2w classifier for motion)
lst.seqs <- c("T2w", "FLAIR", "T1w")[1]
# * input sequences with mriqc metrics
lst.measures <- c("avg_motion", "avg_quality", "avg_flow_ghosting", "avg_susceptibility")[1]
# * column names for scan ratings
lst.pcas <- c(10, 15, 20)[1]
# * number of PCAs inputted to model 
cols.weights <-c('w_inverse','w_invsqr', 'unweighted')[1]
# * weighting strategy for input data
lst.modes <- c('classification', 'regression')[1]
# * choice of classification or regression model

#--------------------------------------------------------------------------------
#Run type configurations
debug <- T
# * run in debug mode with minimal computations
run_paral = F
# * run in parallel with slurm script

#Nested cross-validation parameters ---------------------------------
in.split <- 0.8
# * train test split
in.grid_size <- 50
# * tuning grid size for non-linear models (random forest, xgboost)
grid_size_linear <- 1000
# * 1D tuning grid for linear algorithms (ridge regression, lasso regression, elastic net)
in.cvfolds <- 10
# * outer fold: cross-validation folds
in.cvreps <- 5
# *  outer fold: cross-validation repetitions
in.bootstraps <- 25
# * inner fold bootstraps
