# Search for n.defaults hyperparameter configs
# @param surrogates Surrogate models
# @param par.set Parameter set
# @param n.defauls How many defaults
# @param probs Quantile to optimize
searchDefaults = function(surrogates_train, par.set, n.defaults = 10, probs = 0.5, fs.config = NULL) {

  # Create the objective function we want to optimize
  pfun = makeObjFunction(surrogates_train, probs)

  # Instantiate default parameters and respective performances
  defaults.perf = NULL
  defaults.params = NULL

  # Compute n.defaults  default parameters iteratively
  # Earlier found defaults influence later performances
  for (j in seq_len(n.defaults)) {

    # This is only used in the "cycle" experiment:
    # if (stri_detect_fixed(probs, "cycle")) {
    #  # Cycle through different results
    #  cycle.probs = c(0.5, 0.9, 0.1)
    #  pfun = makeObjFunction(surrogates_train, cycle.probs[(j %% 3) + 1])
    # }

    # Search for optimal points given previous defaults
    z = focusSearchDefaults(pfun, surrogates_train, par.set, defaults.perf = defaults.perf, fs.config)
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
    } else if (probs == "hodges-lehmann") {
      0.33 * max(parmin) + 0.67 * mean(parmin)
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
focusSearchDefaults = function (pfun, surrogates_train, param.set, defaults.perf, fs.config = NULL) {

  # Do the focussearch.
  if (is.null(fs.config)) {
    ctrl = makeFocusSearchControl(maxit = 10, restarts = 1, points = 5*10^4)
  } else {
    ctrl = makeFocusSearchControl(maxit = fs.config[1, 2], restarts = fs.config[1, 3], points = fs.config[1, 1])
  }
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
  prd = sapply(surrogates, function(x) {predict(x, newdata = defaults.params)$data$response})
  prd = as.data.frame(prd)
  colnames(prd) = ifelse(colnames(prd) %in% colnames(prd)[train.inds],
   paste0(colnames(prd), "_train"),
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
