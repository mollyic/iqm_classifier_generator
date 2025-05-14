script_str <- paste0(in.seq, '_pccomps-', in.pcas, '_weights-', in.weights, '_type-', in.measure, '_mode-', in.mode)
script_path <- file.path(lst_dir[['script_dir']], paste0(script_str, '.R'))

base_script <- sprintf("
in.seq <-  '%s'
in.pcas <-  as.numeric('%s')
in.weights <- '%s'
in.measure <-  '%s'
in.mode <-  '%s'

fileout.results <- paste0('%s', '_results-top.csv')
fileout.allranked <- paste0('%s', '_results-allranked.csv')
fileout.preds <- paste0('%s', '_results-predictions.csv')
fileout.model <- paste0('%s', '.rds')
file.sink <- paste0('work/terminal_output/', '%s', '_output-console.txt')

source('modules/main.R')
", in.seq, in.pcas, in.weights, in.measure, in.mode, 
                       script_str, script_str, script_str, script_str, script_str)

writeLines(base_script, script_path)
