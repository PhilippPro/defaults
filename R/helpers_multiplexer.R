if (FALSE) {
lrn.mm.ps = getModelMultiplexer()$ps
all.surrogates = readAllSurrogates()
fs.config = data.frame(iters = 100, depth = 1, reps = 1)

# Defaults
defs.file = paste0("defaultLOOCV/", "mean_defaults_", "multiplexer.RDS")

registerDoMC(19)
# Compute defaults if not yet available
if (!file.exists(defs.file)) {
  set.seed(199)
  # Iterate over ResampleInstance and its indices
  defs = foreach(it = seq_len(38)) %dorng% {
    # Search for defaults
    defs = searchMMDefaults(
      all.surrogates, # training surrogates (L-1-Out-CV)
      lrn.mm.ps, # parameter space to search through
      n.defaults = 32, # Number of defaults we want to find
      fs.config = fs.config,
      iter = it) # Quantile we want to optimize
    return(defs)
  }
  # Save found defaults as RDS
  saveRDS(list("defaults" = defs), defs.file)
}
}

searchMMDefaults = function(surrogates_train, par.set, n.defaults = 10, fs.config = NULL, iter = NULL) {

  # Create the objective function we want to optimize
  pfun = makeMMObjFunction(surrogates_train, iter)

  # Instantiate default parameters and respective performances
  defaults.perf = NULL
  defaults.params = NULL

  # Compute n.defaults  default parameters iteratively
  # Earlier found defaults influence later performances
  for (j in seq_len(n.defaults)) {

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
makeMMObjFunction = function(surrogates, iter) {
  force(surrogates)
  force(iter)

  # Helper function, that first computes the minimum over all
  # defaults and then the quantile. This is the core idea of reducing
  # default search to a single number , which can be optimized
  defaultsMinMean = function(x, defaults.perf) {
    # Compute min of prd and defaults.perf for each dataset
    parmin = apply(cbind(x, defaults.perf), 1, min)
    mean(parmin)
  }

  # Predict newdata, compute prediction
  function (x, defaults.perf = NULL) {

      # x is a dataframe with randomly drawn points from the param space.
      # We query surrogates for each learner
      prds = foreach(lrn = unique(x$selected.learner), .combine = "rbind") %do% {
        lrn.lst = prepSurrParamsPredict(surrogates, lrn, x, iter)
        # Compute predictions for each surrogate model
        prds = sapply(lrn.lst$lrn.surr, function(surr) {
          predict(surr, newdata = lrn.lst$x)$data$response
        })
        rownames(prds) = lrn.lst$rows
        prds
      }
      prds = prds[order(as.numeric(rownames(prds))), ]

      # For each randomly sampled config:
      # defaults.perf are the defaults from iterations 1, ... , n-1
      apply(prds, 1, defaultsMinMean,
        defaults.perf = defaults.perf)
  }
}

prepSurrParamsPredict = function(surrogates, lrn, x, iter) {
  pars = x[x$selected.learner == lrn, , drop = FALSE]
  rw = rownames(pars)
  if (is.null(iter)) {
    lrn.surr = surrogates[[lrn]]
  } else {
    lrn.surr = surrogates[[lrn]][-iter]
  }
  # na.cols = sapply(pars, function(x) all(is.na(x)))
  this.lrn.cols = stri_detect_fixed(colnames(pars), lrn)
  pars = pars[, this.lrn.cols & names(x) != "selected.learner", drop = FALSE]
  names(pars) = stri_replace_first_fixed(names(pars), paste0(lrn, "."), "")
  if(lrn == "classif.xgboost") # Reorder columns for xgb
    pars = pars[, lrn.surr[[1]]$features]
  return(list("lrn.surr" = lrn.surr, "x" = pars, rows = rw))
}


getModelMultiplexer = function(lrn.inds = seq_len(6)) {

lrn.par.sets = getLearnerParSets()
lrn.ps = extractSubList(lrn.par.sets, "param.set", simplify = FALSE)
names(lrn.ps) = stri_sub(names(lrn.par.sets), to = -5)

learner.names = stri_sub(stri_sub(stri_paste("mlr.", names(lrn.par.sets)), 1, -5), from = 5)
lrns = sapply(learner.names, makeLearner, predict.type = "prob")
lrnMM = makeModelMultiplexer(lrns)
psMM  = makeModelMultiplexerParamSet(lrnMM,
  "classif.glmnet" = lrn.ps[[1]],
  "classif.rpart" = lrn.ps[[2]],
  "classif.kknn" = lrn.ps[[3]],
  "classif.svm" = lrn.ps[[4]],
  "classif.ranger" = lrn.ps[[5]],
  "classif.xgboost" = lrn.ps[[6]])

list(lrn = lrnMM, ps = psMM)
}


readAllSurrogates = function() {
  lrn.par.sets = getLearnerParSets()
  learner.names = stri_sub(stri_paste("mlr.", names(lrn.par.sets)), 1, -5)
  surrogates = sapply(learner.names, FUN = function(name) {
      readRDS(stri_paste("surrogates/",
        files[grep(stri_sub(name, from = 5), x = files)])[1])
      }, simplify = FALSE)
  surrogates = extractSubList(surrogates, "surrogates", simplify = FALSE)
  names(surrogates) = stri_sub(learner.names, from = 5)
  names(surrogates)[3] = "classif.kknn"
  surrogates
}



