
# Input is a matrix of Datasets X Default Configs
# We compute quantiles over the datasets
# q = 0.5 -> median, q = 1 -> max etc.
matrixAggregator = function(perf.mat, current.set = integer(0), q = 0.5) {
  if (length(current.set) == 0) {
    # Compute the quantile over the rows
    dsperfs = apply(perf.mat, 2, quantile = q)
    # Compute the min over the default values (we pick the best configuration)
    which.min(dsperfs)
  }
  
}
