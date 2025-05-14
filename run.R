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
        }
      }
    }
  }
}

