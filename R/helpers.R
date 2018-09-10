# ----------------------------------------------------------------------------------------
# Misc Helpers

stringBuilder = function(folder, what, learner.name, cfg = NULL) {
  files = list.files("surrogates")[grep(x = list.files("surrogates"), "regr.*_classif")]
  out = stri_paste(folder, "/", what, gsub("regr.", "", files[grep(stri_sub(learner.name, from = 5), x = files)]))
  if (!is.null(cfg))
    out = paste0(stri_sub(out, to = -6), "_", cfg, ".RDS")
  return(out)
}

load_surrogates = function(learner.name) {
  files = list.files("surrogates")[grep(x = list.files("surrogates"), "regr.cubist_classif")]
  catf("Loading surrogate for learner: %s", learner.name)
  # Read surrogates from Hard Drive
  surrogates = readRDS(stri_paste("surrogates/", files[grep(stri_sub(learner.name, from = 5), x = files)]))
  return(surrogates)
}

# ----------------------------------------------------------------------------------------
# Parameter Sets and Helpers for Param Sets

getSimpleLearners = function(){
  # Simple learner param set
  simple.lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.glmnet", predict.type = "prob"),
    param.set = makeParamSet(
      makeNumericParam("alpha", lower = 0, upper = 1, default = 1),
      makeNumericVectorParam("lambda", len = 1L, lower = -10, upper = 10, default = 0 ,trafo = function(x) 2^x)))

  simple.lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.rpart", predict.type = "prob"),
    param.set = makeParamSet(
      makeNumericParam("cp", lower = 0, upper = 1, default = 0.01),
      makeIntegerParam("maxdepth", lower = 1, upper = 30, default = 30),
      makeIntegerParam("minbucket", lower = 1, upper = 60, default = 1),
      makeIntegerParam("minsplit", lower = 1, upper = 60, default = 20)),
    lrn.ps.sets = simple.lrn.par.set)

  return(simple.lrn.par.set)
}

getLearnerParSets = function(){
  simple.lrn.par.set = getSimpleLearners()

  # increase to a general param set
  lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.kknn", predict.type = "prob"),
    param.set = makeParamSet(
      makeIntegerParam("k", lower = 1, upper = 30)),
    lrn.ps.sets = simple.lrn.par.set)

  lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.svm", predict.type = "prob"),
    param.set = makeParamSet(
      makeDiscreteParam("kernel", values = c("linear", "polynomial", "radial")),
      makeNumericParam("cost", lower = -10, upper = 10, trafo = function(x) 2^x),
      makeNumericParam("gamma", lower = -10, upper = 10, trafo = function(x) 2^x, requires = quote(kernel == "radial")),
      makeIntegerParam("degree", lower = 2, upper = 5, requires = quote(kernel == "polynomial"))),
    lrn.ps.sets = lrn.par.set)

  lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.ranger", predict.type = "prob"),
    param.set = makeParamSet(
      makeIntegerParam("num.trees", lower = 1, upper = 2000),
      makeLogicalParam("replace"),
      makeNumericParam("sample.fraction", lower = 0.1, upper = 1),
      makeNumericParam("mtry", lower = 0, upper = 1),
      makeLogicalParam(id = "respect.unordered.factors"),
      makeNumericParam("min.node.size", lower = 0, upper = 1)),
    lrn.ps.sets = lrn.par.set)

  lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.xgboost", predict.type = "prob"),
    param.set = makeParamSet(
      makeIntegerParam("nrounds", lower = 1, upper = 5000),
      makeDiscreteParam("booster", values = c("gbtree", "gblinear")),
      makeNumericParam("eta", lower = -10, upper = 0, trafo = function(x) 2^x),
      makeNumericParam("subsample",lower = 0.1, upper = 1, requires = quote(booster == "gbtree")),
      makeIntegerParam("max_depth", lower = 1, upper = 15, requires = quote(booster == "gbtree")),
      makeNumericParam("min_child_weight", lower = 0, upper = 7, requires = quote(booster == "gbtree"),
        trafo = function(x) 2^x),
      makeNumericParam("colsample_bytree", lower = 0, upper = 1, requires = quote(booster == "gbtree")),
      makeNumericParam("colsample_bylevel", lower = 0, upper = 1, requires = quote(booster == "gbtree")),
      makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x,
        requires = quote(booster == "gblinear")),
      makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x,
        requires = quote(booster == "gblinear"))),
    lrn.ps.sets = lrn.par.set)

  return(lrn.par.set)
}

makeLrnPsSets = function(learner, param.set, lrn.ps.sets = NULL,
  id = paste0(learner$id, ".set"), overwrite = FALSE) {

  assertClass(learner, "Learner")
  assertClass(param.set, "ParamSet")
  par.match = names(param.set$pars) %in% names(learner$par.set$pars)
  if(all(par.match)){
    ls = list(learner = learner, param.set = param.set)
  } else {
    stop(paste("The following parameters in param.set are not included in learner:",
      paste(names(param.set$pars[par.match == FALSE]), collapse = ", ")))
  }

  if(is.null(lrn.ps.sets)){
    lrn.ps.sets = list()
    lrn.ps.sets[[id]] = ls
    attr(lrn.ps.sets, "class") = "LrnPsSet"
  } else {
    assertClass(lrn.ps.sets, "LrnPsSet")

    if(id %in% names(lrn.ps.sets) & overwrite == FALSE){
      stop("tune.pair already contains id: \"", id, "\". Please specify a new id or set overwrite = TRUE.")
    } else {
      lrn.ps.sets[[id]] = ls
    }
  }

  return(lrn.ps.sets)
}

deleteNA = function(task.data) {
  for(i in 1:ncol(task.data)) {
    if(is.numeric(task.data[, i]))
      task.data[is.na(task.data[, i]), i] = -10 - 1
    if(is.factor(task.data[, i])) {
      task.data[, i] = addNA(task.data[, i])
      task.data[, i] = droplevels(task.data[, i])
    }
    if(is.logical(task.data[, i]))
      task.data[, i] = as.factor(task.data[, i])
  }
  task.data
}

makePerTaskParamSet = function() {
  list(
    ps.adaboost = makeParamSet(
      makeDiscreteParam(id = "algorithm", values = c("SAMME", "SAMME.R")),
      makeNumericParam(id = "learning_rate", lower = 0.01, upper = 2.0),
      makeIntegerParam(id = "max_depth", lower = 1, upper = 20),
      makeIntegerParam(id = "n_estimators", lower = 50, upper = 500),
      makeDiscreteParam(id = "strategy", values = c("median", "most_frequent", "mean"))
    ),
    ps.svm_svc = makeParamSet(
      makeDiscreteParam(id = "kernel", values = c("poly", "sigmoid", "rbf")),
      makeNumericParam(id = "coef0", lower = -1, upper = 1),
      makeNumericParam(id = "gamma", lower = 0.00003, upper = 7.999292),
      makeNumericParam(id = "tol", lower = 10^-5, upper = 10^-1),
      makeIntegerParam(id = "degree", lower = 1, upper = 5),
      makeLogicalParam("shrinking"),
      makeDiscreteParam(id = "strategy", values = c("median", "most_frequent", "mean"))
    ),
    ps.random_forest = makeParamSet(
      makeDiscreteParam(id = "criterion", values = c("entropy", "gini")),
      makeNumericParam(id = "max_features", lower = 0.1, upper = 0.9),
      makeIntegerParam(id = "min_samples_leaf", lower = 1, upper = 20),
      makeIntegerParam(id = "min_samples_split", lower = 1, upper = 20),
      makeDiscreteParam(id = "strategy", values = c("median", "most_frequent", "mean"))
    )
  )
}

convertParamType = function(x, param_type) {
  if(param_type %in% c("integer", "numeric", "numericvector"))
    x = as.numeric(x)
  if(param_type %in% c("character", "logical", "factor", "discrete"))
    x = as.factor(x)
  return(x)
}
