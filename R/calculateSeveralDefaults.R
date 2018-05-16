# Calculate default hyperparameter setting
# @param surrogates Surrogate models
# @param par.set Parameter set
# @param n.defauls How many defaults
# @param probs Quantile to optimize
defaultForward = function(surrogates, par.set, n.defaults = 10, probs = 0.5) {
  # Create the objective function
  pfun = makeObjFunction(surrogates, probs)
  
  # Instantiate default parameters and respective performances
  defaults.perf = NULL
  defaults.params = NULL
  
  # Compute n.defaults  default parameters iteratively
  # Earlier found defaults influence later performances
  for (j in seq_len(n.defaults)) {
    # Search for optimal points given previous defaults
    z = focusSearchDefaults(pfun, surrogates, par.set, defaults.perf = defaults.perf)
    catf("New best y: %f found for x: %s", z$y, paste0(z$x, collapse = ","))
    # Add optimal point to defaults
    defaults.perf = cbind(defaults.perf, z$dsperfs)
    defaults.params = rbind(defaults.params, z$x)
  }
  return(defaults.params)
}

# Calculate LOOCV Performance of forward focusSearch
# @param surrogates Surrogate models
# @param n.defauls How many defaults
# @param probs Quantile to optimize
defaultLOOCV = function(surrogates, n.defaults = 10, probs = 0.5) {  
  # Do LOOCV
  inds = seq_len(length(surrogates$surrogates))
  parallelMap::parallelStartMulticore(4)
  lst = parallelMap::parallelLapply(inds, loocvIter, surrogates, n.defaults, probs)
  paralellMap::parallelStop()
  perfs = extractSubList(lst, "y")
  # cummulative min (we choose the best default from a set of n defaults)
  y.cummin = apply(perfs, 2, cummin)
  # Compute mean over resample iters
  y.mean = setNames(apply(y.cummin, 1, mean), paste0("def", seq_len(n.defaults)))
  catf("End LOOCV iter, y.mean: %f", paste0(y.mean, collapse = " "))
  return(list("y.mean" = y.mean, "params" = extractSubList(lst, "x"), "full.y" = perfs))
}

# # Do a single Leave-One-Out CV Iteration
# @param i Surrogate id to drop in this iteration
# @param surrogates List of surrogates
# @param n.defaults How many defaults to learn
# @param probs Quantile we want to optimize
loocvIter = function(i, surrogates, n.defaults, probs) {
  catf("LOOCV iter: %i", i)
  # Do the forward search
  defaults.params = defaultForward(surrogates$surrogates[-i], surrogates$param.set, n.defaults, probs)
  # FIXME: How do we do the evaluation?
  # Now: On surrogate / Option b: On real task
  prd.hout = predict(surrogates$surrogates[[i]], newdata = defaults.params)$data$response
  return(list("y" = prd.hout, "x" = defaults.params))
}

# Create an objective function that only requires inputs x (algorithm hyperpars) and defaults.perf (other defaults)
# @param surrogates List of surrogates
# @param probs Quantile we want to optimize
makeObjFunction = function(surrogates, probs) {
  force(surrogates)
  force(probs)
  # Predict newdata, compute prediction
  function (x, defaults.perf = NULL) {
    # Compute predicitons for each surrogate
    prds = sapply(surrogates, function(surr) {
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
# @param pfun Objective function
# @param param.set Parameter set
# @param defaults.perf = performances of defaults 1, ..., n-1.
focusSearchDefaults = function (pfun, surrogates, param.set, defaults.perf) {
  points = 10^5
  # For knn do not search the full param space
  if (getParamIds(param.set)[1] == "k") points = 30
  ctrl = makeFocusSearchControl(maxit = 4, restarts = 10, points = points)
  z = focussearch(pfun, param.set, ctrl, show.info = FALSE, defaults.perf = defaults.perf)
  z$dsperfs = sapply(surrogates, function(m) {predict(m, newdata = z$x)$data$response})
  return(z)
}



