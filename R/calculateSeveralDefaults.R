# Search for nth default
# @param surrogates list of models
# @param param.set parameter set
# @param defaults.perf = performances of defaults 1, ..., n-1.
# @param q Quantile we want to optimize
doFocusSearch = function (surrogates, defaults.perf = NULL, q = 0.5) {
  # Compute minimum of prediction and earlier defaults
  fn = function(prd, defaults.perf) {
    prd.min = apply(cbind(prd, defaults.perf), 1, min)
  }
  # Predict newdata, compute prediction
  f = function(x, surrogates, defaults.perf, q) {
    prds = sapply(surrogates$surrogates, function(surr) {
      prd = predict(surr, newdata = x)$data$response
      parmin = sapply(prd, fn, defaults.perf = defaults.perf)
      quantile(parmin, q = q)
    })
  }
  ctrl = makeFocusSearchControl(maxit = 5, restarts = 5, points = 100)
  z = focussearch(f, surrogates$param.set, ctrl, surrogates = surrogates, defaults.perf = defaults.perf, q = q)
  z$dsperfs = sapply(surrogates$surrogates, function(m) {predict(m, newdata = z$x)$data$response})
  return(z)
}

# doFocusSearch(surrogates)
#
LOOCV = function(i, surrogates) {
  surr = surrogates$surrogates[-i]
  # FIXME: How do we do the evaluation?
  # Option a: On surrogate
  # Option b: On real task
}

# Calculate default hyperparameter setting
# @param surrogates Surrogate models
calculateDefaultForward = function(surrogates, n.default = 10) {
  surr = surrogates$surrogates
  param.set = surrogates$param.set

  # LOOCV
  inds = seq_len(length(surrogates$surrogates))
  sapply(inds, function(x) LOOCV)
  

  # Best default in general
  # average_preds = apply(preds, 1, mean)
  # best = which(average_preds == max(average_preds))[1]
  # Now we obtain a matrix with the predictions.
  best = matrixAggregator(perf)
  default = rnd.points[best,, drop = FALSE]
  rownames(default) = NULL
  
  # Second best default
  
  for(i in 1:n.default) {
    print(paste("iteration", i, "of", n.default))
    average_preds_new = apply(preds, 1, function(x) mean(apply(rbind(preds[best,], x), 2, max)))
    best2 = which(average_preds_new == max(average_preds_new))[1]
    best = c(best, best2)
    default = rnd.points[best,, drop = FALSE]
    print(paste("Maximum", round(average_preds_new[which(average_preds_new == max(average_preds_new))], 4)))
  }
  list(default = default, result = preds[best, ])
  
  # Default calculation with LOOCV
}
