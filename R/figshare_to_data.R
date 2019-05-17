#' Preprocess figshare data
#'
#' Obtains data from figshare, saves a (long) data.frame with the following columns:
#' - performance: Performance value
#' - measure:     Measure (auc, acc, brier)
#' - learner_id:  Learner (see get_baselearners())
#' - ...          The whole parameter set across all learners.
#' @param bot_data Path to save the result to.
#' @export
figshare_to_data = function(bot_data = "data/input/oml_bot_data.RDS") {
  load(url("https://ndownloader.figshare.com/files/10462297"))
  lps = getLearnerParSets()
  tbl.metaFeatures = tbl.metaFeatures %>% filter(quality %in% c("NumberOfFeatures", "NumberOfInstances"))

  # Get a data.frame with all hpar <-> performance combinations
  learner_feats_list = tbl.hypPars %>%
    group_by(fullName) %>%
    do({
      param.set = lps[[substr(paste0(unique(.$fullName), ".set"), 5, 10^3)]]$param.set
      hpvals = spread(., name, value)

      # Ensure correct data.types for params
      params = getParamIds(param.set)
      param_types = getParamTypes(param.set)
      for(i in seq_along(params))
        hpvals[, params[i]] = convertParamType(hpvals[[params[i]]], param_types[i])
      bot.table = tbl.results %>%
        dplyr::rename(acc = accuracy) %>%
        gather("measure", "performance", -setup, -run_id, -task_id, -data_id) %>%
        inner_join(hpvals, by = "setup") %>%
        select(., -setup)

      # Scale mtry and min.node.size in random forest
      if (unique(.$fullName) == "mlr.classif.ranger") {
        bot.table = bot.table %>%
          inner_join(
            filter(tbl.metaFeatures, quality == "NumberOfFeatures") %>% select(., -quality),
            by = "data_id") %>%
          mutate(mtry = mtry / as.numeric(value)) %>%
          select(., -value) %>%
          inner_join(
            filter(tbl.metaFeatures, quality == "NumberOfInstances") %>% select(., -quality),
            by = "data_id") %>%
          mutate(min.node.size = log(min.node.size, 2) / log(as.numeric(value), 2)) %>%
          select(., -value)
      }
      bot.table %>% rename(learner_id = fullName)
  })
  learner_feats_list = learner_feats_list %>%
    left_join(tbl.runTime) %>%
    select(-run_id) %>%
    group_by(learner_id) %>%
    select(-fullName)
  saveRDS(file = bot_data, learner_feats_list)
}

# Some helper functions for figshare_to_data.
getLearnerParSets = function(){

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

convertParamType = function(x, param_type) {
  if(param_type %in% c("integer", "numeric", "numericvector"))
    x = as.numeric(x)
  if(param_type %in% c("character", "logical", "factor", "discrete"))
    x = as.factor(x)
  return(x)
}
