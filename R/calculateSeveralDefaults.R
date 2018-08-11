# Search for n.defaults hyperparameter configs
# @param surrogates Surrogate models
# @param par.set Parameter set
# @param n.defauls How many defaults
# @param probs Quantile to optimize
searchDefaults = function(surrogates_train, par.set, n.defaults = 10, probs = 0.5) {

  # Create the objective function we want to optimize
  pfun = makeObjFunction(surrogates_train, probs)

  # Instantiate default parameters and respective performances
  defaults.perf = NULL
  defaults.params = NULL

  # Compute n.defaults  default parameters iteratively
  # Earlier found defaults influence later performances
  for (j in seq_len(n.defaults)) {
    if (probs == "cycle") {
      # Cycle through different results
      cycle.probs = c(0.5, 0.9, 0.1)

      pfun = makeObjFunction(surrogates_train, cycle.probs[(j %% 3) + 1])
    }

    # Search for optimal points given previous defaults
    z = focusSearchDefaults(pfun, surrogates_train, par.set, defaults.perf = defaults.perf)
    catf("New best y: %f found for x: %s", z$y, paste0(z$x, collapse = ", "))
    # Add optimal point to defaults
    defaults.perf = cbind(defaults.perf, z$dsperfs)
    defaults.params = rbind(defaults.params, z$x)
  }
  return(defaults.params)
}


# Create an objective function that only requires inputs x (algorithm hyperpars) and
# defaults.perf (defaults found in earlier iterations)
# @param surrogates List of surrogates
# @param probs Quantile we want to optimize
makeObjFunction = function(surrogates_train, probs) {
  force(surrogates)
  force(probs)

  # Helper function, that first computes the minimum over all
  # defaults and then the quantile. This is the core idea of reducing
  # default search to a single number , which can be optimized
  defaultsMinQuantile = function(x, defaults.perf, probs) {
    # Compute min of prd and defaults.perf for each dataset
    parmin = apply(cbind(x, defaults.perf), 1, min)
    # and compute quantile over the datasets
    if (is.numeric(probs)) {
      quantile(parmin, probs = probs)
    } else if (probs == "mean") {
      mean(parmin)
    }
  }

  # Predict newdata, compute prediction
  function (x, defaults.perf = NULL) {
      # Compute predictions for each surrogate model
      prds = sapply(surrogates_train, function(surr) {
        predict(surr, newdata = x)$data$response
      })
      # For each randomly sampled config:
      # defaults.perf are the defaults from iterations 1, ... , n-1
      apply(prds, 1, defaultsMinQuantile,
        defaults.perf = defaults.perf, probs = probs)
  }
}



# Search for nth default
# @param pfun Objective function
# @param param.set Parameter set
# @param defaults.perf = performances of defaults 1, ..., n-1.
focusSearchDefaults = function (pfun, surrogates_train, param.set, defaults.perf) {

  # Do the focussearch
  ctrl = makeFocusSearchControl(maxit = 1, restarts = 3, points = 10^4)
  # For debugging:
  # ctrl = makeFocusSearchControl(1, 1, points = 30)

  # For knn search the full param space
  if (getParamIds(param.set)[1] == "k")
    ctrl = makeFocusSearchControl(maxit = 1, restarts = 2, points = 120)

  z = focussearch(pfun, param.set, ctrl, defaults.perf = defaults.perf, show.info = FALSE)
  # Add performance on the individual datasets
  z$dsperfs = sapply(surrogates_train, function(m) {predict(m, newdata = z$x)$data$response})
  return(z)
}


# Calculate performance for a given set of param in train and test split
# @param surrogates Surrogate models
# @param par.set Parameter set
# @param n.defauls How many defaults
# @param probs Quantile to optimize
getDefaultPerfs = function(surrogates, defaults.params, train.inds = train_split()) {

  # Which split do we want to predict on? (train or test datasets)
  # Predict on each split
  prd = sapply(surrogates, function(x) {
    predict(x, newdata = defaults.params)$data$response
  })
  prd = as.data.frame(prd)
  colnames(prd) = ifelse(colnames(prd) %in% colnames(prd)[train.inds], paste0(colnames(prd), "_train"),
                         paste0(colnames(prd), "_test"))
  return(prd)
}


# Randomsearch hyperParameters (Used to compare agains found defaults)
# @param surrogates Surrogate models
# @param par.set Parameter set
# @param multiplier Multiplier to compare to
# @param points Defaults to compare to
# @param probs Quantile to optimize
randomSearch = function(surrogates, par.set, multiplier, points, seed = 199) {
  set.seed(seed)

  newdesign = generateRandomDesign(multiplier * points, par.set, trafo = TRUE)
  newdesign = deleteNA(newdesign)
  newdesign = convertDataFrameCols(newdesign, ints.as.num = TRUE,  logicals.as.factor = TRUE)
  # Make sure we have enough points in newdesign and not too many
  while (nrow(newdesign) < multiplier * points) {
    n2 = generateRandomDesign(multiplier * points, par.set, trafo = TRUE)
    n2 = deleteNA(n2)
    n2 = convertDataFrameCols(n2, ints.as.num = TRUE,  logicals.as.factor = TRUE)
    newdesign = rbind(newdesign, n2)
  }
  newdesign = newdesign[seq_len(multiplier * points), ]

  zs = sapply(surrogates, function(x) {predict(x, newdata = newdesign)$data$response})
  # zs = apply(zs, 2, min)
  names(zs) = names(surrogates)
  return(zs)
}





# -------------------------------------------------------------------------------
# Maybe use this later for CV

# # Calculate LOOCV Performance of forward focusSearch
# # @param surrogates Surrogate models
# # @param n.defauls How many defaults
# # @param probs Quantile to optimize
# # @param houtsets Number of datasets to hold out
# defaultCV = function(surrogates, n.defaults = 10, probs = 0.5, houtsets = 5, parallel = TRUE) {
#   # Create CV Splits
#   inds = seq_len(length(surrogates$surrogates))
#   inds = split(inds, ceiling(seq_along(inds) / houtsets))
#
#   # Parallelize CV
#   if (parallel) {
#     parallelMap::parallelStartMulticore(8)
#     lst = parallelMap::parallelLapply(inds, cvIter, surrogates, n.defaults, probs)
#     parallelMap::parallelStop()
#   } else {
#     lst = lapply(inds, cvIter, surrogates, n.defaults, probs)
#   }
#
#   # Extract relevant results (mean over OOB datasets)
#   perfs = sapply(extractSubList(lst, "y"), function(x) {apply(x, 1, mean)})
#   # cummulative min (we choose the best default from a set of n defaults)
#   y.cummin = apply(perfs, 2, cummin)
#   # Compute mean over resample iters
#   y.mean = setNames(apply(y.cummin, 1, mean), paste0("def", seq_len(n.defaults)))
#   catf("End LOOCV iter, y.mean: %s", paste0(y.mean, collapse = " "))
#   return(list("y.mean" = y.mean, "params" = extractSubList(lst, "x"), "full.y" = perfs))
# }
#
# # # Do a single Leave-One-Out CV Iteration
# # @param i Surrogate id to drop in this iteration
# # @param surrogates List of surrogates
# # @param n.defaults How many defaults to learn
# # @param probs Quantile we want to optimize
# cvIter = function(i, surrogates, n.defaults, probs) {
#   catf("CV iter: Datasets %s \n", paste0(min(i), ":", max(i), collapse = ""))
#   # Do the forward search
#   defaults.params = defaultForward(surrogates$surrogates[-i], surrogates$param.set, n.defaults, probs)
#   # FIXME: How do we do the evaluation?
#   # Now: On surrogate / Option b: On real task
#   prd.hout = sapply(surrogates$surrogates[i], function(x) {
#     predict(x, newdata = defaults.params)$data$response
#   })
#   return(list("y" = prd.hout, "x" = defaults.params))
# }

