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


#' Return a list of specified surrogates
#' @param oml_task_ids  A vector of oml task ids for which we want to create surrogates. Defaults to all in get_oml_task_ids().
#' @param baselearners  A vector of baselearners for which we want to create surrogates. Defaults to all in get_baselearners().
#' @param measures      A vector of measures for which we want to create surrogates. Defaults to all in get_measures().
#' @param surrogate_lrn A mlr [Learner]. Defaults to "regr.fixcubist".
#' @export
make_surrogates_omlbot = function(oml_task_ids, baselearners, measures, surrogate_lrn) {

  if (missing(surrogate_lrn)) {
    # Obtain fixed cubist from the internet
    source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
    surrogate_lrn = mlr::makeLearner("regr.fixcubist", committees = 20, extrapolation = 0)
  }
  if (missing(oml_task_ids))
    oml_task_ids = get_oml_task_ids()
  if (missing(baselearners))
    baselearners = get_baselearners()
  if (missing(measures))
    measures = get_measures()

  surrs = foreach(measure_name = measures, .combine = "c") %:%
    foreach(oml_task_id = oml_task_ids, .combine = "c") %:%
      foreach(baselearner_name = baselearners, .combine = "c") %dopar% {
        s = surrogates::SurrogateFromRDS$new(
          oml_task_id = oml_task_id,
          baselearner_name = baselearner_name,
          data_source = "data/input/oml_bot_data.RDS",
          measure_name = measure_name,
          surrogate_learner = surrogate_lrn,
          handle_prefix = "data/intermediate/")
        s$train()
        return(s)
      }
  sc = surrogates::SurrogateCollection$new(surrs)
  return(sc)
}


#' Train all surrogates for a given surrogate collection.
#' @param sc [SurrogateCollection] if missing, calls make_surrogate_omlbot().
#' @param overwrite should existing surrogates be overwritten?
#' @export
train_surrogates_omlbot = function(sc, overwrite = FALSE) {
  if (overwrite) unlink("surrogates", recursive = TRUE, force = TRUE)
  require(doParallel)
  registerDoParallel(12L)
  sc = make_surrogates_omlbot()
  not_trained = seq_along(sc$surrogates)[sapply(sc$surrogates, function(x) !x$is_trained)]
  foreach(s = sc$surrogates[not_trained]) %dopar% {
    gc()
    s$train()
    NULL
  }
  invisible(NULL)
}


#' Substitute NA's with out of bounds data.
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

# Convert wide to long
to_long = function(res, lrn) {
  requireNamespace("tidyr")
  res = data.frame(res)
  res$id = seq_len(32)
  long = tidyr::gather(res, "task", "auc", -id)
  long$learner = lrn
  long
}

#' Fix names of the predictions.
fix_prds_names = function(x) {
  colnames(x) = stringi::stri_sub(stringi::stri_extract_all_regex(colnames(x), ".*_"), to = -2)
  return(x)
}

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
