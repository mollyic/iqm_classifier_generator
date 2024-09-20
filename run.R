source('modules/utils.R')

df.compare_ranks <- data.frame()

idx.scripts <- 0
for (in.seq in lst.seqs){                     #   * define sequence 
  for (in.measure in lst.measures){           #   * define rating type
    for (in.pcas in lst.pcas){                #   * define number of input PCs
      for (in.weights in cols.weights){       #   * define weighting type
        for (in.mode in lst.modes){           #   * regression or classification model
          source('modules/generate_runs.R')
          idx.scripts <- idx.scripts + 1
          if (isTRUE(check.one_run)){
            cat(dot_brk, '\nGenerated script: \n\t  * ', script_path)
            source(script_path)
            # user_input <- readline(prompt = "\nRun script? (y/n)\n")
            # if(tolower(user_input) == "y"){}
          }
        }
      }
    }
  }
}

if (!isTRUE(check.one_run)){
  cat(dot_brk, '\n Generated run files: \n\t  * Total: ', idx.scripts, 
      '\n\t * Run file directory: ', dirname(script_path), dot_brk)
}
