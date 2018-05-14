# Search for nth default
# @param surrogates list of models
# @param param.set parameter set
# @param defaults.perf = performances of defaults 1, ..., n-1.
# @param q Quantile we want to optimize
doFocusSearch = function (surrogates, param.set, defaults.perf = NULL, q = 0.5) {
  # Compute quantile of the predictions
  fn = function(prd, defaults.perf, q) {
    prd.min = apply(cbind(prd, defaults.perf), 1, min)
    quantile(prd.min, q = q)
  }
  f = function(x, surrogates, defaults.perf, q) {
    prds = sapply(surrogates$surrogates, function(surr) {
      browser()
      prd = predict(surr, newdata = x)$data$response
      qs = apply(prd, 1, fn, defaults.perf = defaults.perf, q = q)
    })
  }
  ctrl = makeFocusSearchControl(maxit = 2, restarts = 1, points = 10)
  z = focussearch(f, surrogates$param.set, ctrl, surrogates, defaults.perf = defaults.perf, q = q)
}

doFocusSearch(surrogates = )
#
LOOCV = function(i, surrogates) {
  surr = surrogates$surrogates[-i]
  calculateDefaultForward(surrogates, n.default = 10)
  # FIXME: How do we do the evaluation?
  # Option a: On surrogate
  # Option b: On real task
}

# Calculate default hyperparameter setting
# @param surrogates Surrogate models
calculateDefaultForward = function(surrogates, n.default = 10) {
  surr = surrogates$surrogates
  param.set = surrogates$param.set

  
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
