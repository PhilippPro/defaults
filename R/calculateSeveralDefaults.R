# Create an objective function that only requires inputs x and defaults.perf
makeObjFunction = function(surrogates, probs) {
  force(surrogates)
  force(probs)
  # Predict newdata, compute prediction
  function (x, defaults.perf = NULL) {
    # Compute predicitons for each surrogate
    prds = sapply(surrogates$surrogates, function(surr) {
      predict(surr, newdata = x)$data$response
    })
    # For each row in prds.
    ds_quantile = apply(prds, 1, function(x, defaults.perf, probs) {
      # Compute min of prd and defaults.perf
      parmin = apply(cbind(x, defaults.perf), 1, min)
      # and compute quantile over the datasets
      quantile(parmin, probs = probs)
    }, defaults.perf = defaults.perf, probs = probs)
    return(ds_quantile)
  }
}


# Search for nth default
# @param surrogates list of models
# @param param.set parameter set
# @param defaults.perf = performances of defaults 1, ..., n-1.
# @param probs Quantile we want to optimize
doFocusSearch = function (surrogates, defaults.perf = NULL, probs = 0.5) {
  
  ctrl = makeFocusSearchControl(maxit = 5, restarts = 5, points = 10)
  z = focussearch(pfun, surrogates$param.set, ctrl, show.info = TRUE, defaults.perf = defaults.perf)
  z$dsperfs = sapply(surrogates$surrogates, function(m) {predict(m, newdata = z$x)$data$response})
  return(z)
}

# # Do a single Leave-One-Out CV Iteration
# @param i Surrogate id to drop in this iteration
# @param surrogates List of surrogates
# @param n.defaults How many defaults to learn
loocvIter = function(i, surrogates, n.defaults, probs = 0.5) {
  hout.surr = surrogates$surrogates[[i]]
  # Drop the i-th surrogate
  pfun = makeObjFunction(surrogates$surrogates[-i], probs = 0.5)
  defaults.perf = NULL
  defaults = NULL
  for (j in seq_len(n.defaults)) {
    # Search for optimal points given previous defaults
    z = doFocusSearch(pfun, defaults.perf = defaults.perf)
    catf("New best y: %f found for x: %s \n", z$y, paste0(z$x, collapse = ","))
    # Add optimal point to defaults
    defaults.perf = cbind(defaults.perf, z$dsperfs)
    defaults = cbind(defaults, z$x)
  }
  # FIXME: How do we do the evaluation?
  # Option a: On surrogate
  # Option b: On real task
  predict(hout.surr, newdata = z$x)$data$response
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
