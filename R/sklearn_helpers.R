getSkLearnParamsets = function() {
  list(
    "adaboost" = makeParamSet(
      makeDiscreteParam("algorithm", values = c("SAMME", "SAMME.R")),
      makeDiscreteParam("strategy", values = c("mean", "median", "most_frequent")),
      makeIntegerParam("n_estimators", lower = 50, upper = 500),
      makeNumericParam("learning_rate", lower = -6.643856, upper = 1, trafo = function(x) 2^x),
      makeNumericParam("max_depth", lower = 1, upper = 10)
    ),
    "random_forest" = makeParamSet(
      makeDiscreteParam("bootstrap", values = c("True", "False")),
      makeDiscreteParam("criterion", values = c("entropy", "gini")),
      makeNumericParam("max_features", lower = 0.01, upper = 1),
      makeIntegerParam("min_leaf", lower = 1, upper = 20),
      makeIntegerParam("min_split", lower = 2, upper = 20),
      makeDiscreteParam("strategy", values = c("mean", "median", "most_frequent"))
    ),
    "libsvm_svc"= makeParamSet(
      makeNumericParam("C", lower = -5, upper = 15, trafo = function(x) 2^x),
      makeNumericParam("coef0", lower = -1, upper = 1),
      makeIntegerParam("degree", lower = 1, upper = 5),
      makeNumericParam("gamma", lower = -15, upper = 3, trafo = function(x) 2^x),
      makeDiscreteParam("kernel", values = c("poly", "sigmoid", "rbf")),
      makeDiscreteParam("strategy", values = c("mean", "median", "most_frequent")),
      makeDiscreteParam("shrinking", values = c("True", "False")),
      makeNumericParam("tol", lower = -5, upper = -1, trafo = function(x) 10^x)
    )
  )
}


searchDefaultsOML100 = function(surrogates, par.set, n.defaults) {

  # Hardcode fs.config
  fs.config = data.frame(iters = c(10^4), depth = c(1), reps = c(1))

  # Create the objective function we want to optimize
  pfun = makeObjFunction(surrogates, 0.5)

  # Instantiate default parameters and respective performances
  defaults.perf = NULL
  defaults.params = NULL

  # Compute n.defaults  default parameters iteratively
  # Earlier found defaults influence later performances
  for (j in seq_len(n.defaults)) {
    # Search for optimal points given previous defaults
    z = focusSearchDefaults(pfun, surrogates,  par.set, defaults.perf = defaults.perf,
      fs.config)
    catf("Def %i: New best y: %f found for x: %s \n", j, z$y, paste0(z$x, collapse = ", "))
    # Add optimal point to defaults
    defaults.perf = cbind(defaults.perf, z$dsperfs)
    defaults.params = rbind(defaults.params, z$x)
  }
  return(defaults.params)
}

preprocess_omldata = function(df, sklearner) {
  if(sklearner == "random_forest")
    df = df %>% mutate(
      min_samples_leaf = as.integer(as.character(min_samples_leaf)),
      min_samples_split = as.integer(as.character(min_samples_split))
      ) %>%
      rename(
        min_leaf = min_samples_leaf,
        min_split = min_samples_split
        )
  return(df)
}
