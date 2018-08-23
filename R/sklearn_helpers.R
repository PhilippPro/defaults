getSkLearnParamsets = function() {
  list(
    "adaboost" = makeParamSet(
      makeDiscreteLearnerParam("algorithm", values = c("SAMME", "SAMME.R")),
      makeDiscreteLearnerParam("strategy", values = c("mean", "median", "mode")),
      makeIntegerLearnerParam("n_estimators", lower = 50, upper = 500),
      makeNumericLearnerParam("learning_rate", lower = -6.643856, upper = 1, trafo = 2^x),
      makeNumericLearnerParam("max_depth", lower = 1, upper = 10)
    ),
    "randomForest" = makeParamSet(
      makeLogicalLearnerParam("bootstrap"),
      makeDiscreteLearnerParam("criterion", values = c("entropy", "gini")),
      makeNumericLearnerParam("max_features", from = 0.001, to = 1),
      makeNumericLearnerParam("min_samples_leaf", lower = 1, upper = 20),
      makeNumericLearnerParam("min_samples_split", lower = 2, upper = 20),
      makeDiscreteLearnerParam("strategy", values = c("mean", "median", "most_frequent"))
    ),
    "libsvm_svc"= makeParamSet(
      makeNumericLearnerParam("C", lower = -5, upper = 15, trafo = 2^x),
      makeNumericLearnerParam("coef0", from = -1, to = 1),
      makeIntegerLearnerParam("degree", from = 1, to = 5),
      makeNumericLearnerParam("gamma", lower = -15, upper = 3, trafo = 2^x),
      makeDiscreteLearnerParam("kernel", values = c("poly", "sigmoid", "rbf")),
      makeDiscreteLearnerParam("strategy", values = c("mean", "median", "most_frequent")),
      makeDiscreteLearnerParam("shrinking"),
      makeLogicalLearnerParam("tol", lower = -5, upper = -1, trafo = 10^x)
    )
  )
}


searchDefaultsOML100 = function(surrogates, par.set, n.defaults) {
  
  # Hardcode fs.config
  fs.config = data.frame(iters = c(10^4), depth = c(1), reps = c(1))
  
  # Create the objective function we want to optimize
  pfun = makeObjFunction(surrogates, probs)
  
  # Instantiate default parameters and respective performances
  defaults.perf = NULL
  defaults.params = NULL
  
  # Compute n.defaults  default parameters iteratively
  # Earlier found defaults influence later performances
  for (j in seq_len(n.defaults)) {
    # Search for optimal points given previous defaults
    z = focusSearchDefaults(pfun, surrogates,  par.set, defaults.perf = defaults.perf,
      fs.config)
    catf("New best y: %f found for x: %s", z$y, paste0(z$x, collapse = ", "))
    # Add optimal point to defaults
    defaults.perf = cbind(defaults.perf, z$dsperfs)
    defaults.params = rbind(defaults.params, z$x)
  }
  return(defaults.params)
}