#model_key <- ifelse(length(in.models) > 1, 'mix', in.models)

if (in.mode == 'regression'){
    in.metric = in.reg_metric
    }
else{
    in.metric = in.cls_metric
    }
    
script_str <- paste0(in.seq, '_pccomps-', in.pcas,
                     '_weights-', in.weights, 
                     '_type-', in.measure, 
                     '_mode-', in.mode, 
                     '_pfmetric-', in.metric)

script_path <- file.path(dir_paths[['script_dir']], paste0(script_str, '.R'))

base_script <- sprintf("
in.seq <-  '%s'
in.pcas <-  as.numeric('%s')
in.weights <- '%s'
in.measure <-  '%s'
in.mode <-  '%s'
in.metric <- '%s'
in.models <- c('%s')

fileout.results <- paste0('%s', '_results-top.csv')
fileout.allranked <- paste0('%s', '_results-allranked.csv')
fileout.preds <- paste0('%s', '_results-predictions.csv')
fileout.model <- paste0('%s', '.rds')
file.sink <- paste0('work/terminal_output/', '%s', '_output-console.txt')

source('main.R')
", in.seq, in.pcas, in.weights, in.measure, in.mode, in.metric, paste(in.models, collapse = "', '"),
                       script_str, script_str, script_str, script_str, script_str)

writeLines(base_script, script_path)
#source(script_path)
