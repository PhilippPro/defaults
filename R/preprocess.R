#' Preprocess figshare data
#'
#' Obtains data from figshare, saves a (long) data.frame with the following columns:
#' - performance: Performance value
#' - measure:     Measure (auc, acc, brier)
#' - learner_id:  Learner (see get_baselearners())
#' - ...          The whole parameter set across all learners.
#' @param bot_data Path to save the result to.
figshare_to_data = function(bot_data = "data/oml_bot_data.RDS") {
  load(url("https://ndownloader.figshare.com/files/10462297"))
  lps = getLearnerParSets()
  tbl.metaFeatures = tbl.metaFeatures %>% filter(quality %in% c("NumberOfFeatures", "NumberOfInstances"))

  browser()
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
      bot.table %>%
       rename(learner_id = fullName) %>%
       select(-run_id)
    })
  saveRDS(file = bot_data, learner_feats_list %>% group_by(learner_id) %>% select(-fullName))
}


#' Return a list of specified surrogates
#' @param oml_task_ids  A vector of oml task ids for which we want to create surrogates. Defaults to all in get_oml_task_ids().
#' @param baselearners  A vector of baselearners for which we want to create surrogates. Defaults to all in get_baselearners().
#' @param measures      A vector of measures for which we want to create surrogates. Defaults to all in get_measures().
#' @param surrogate_lrn A mlr [Learner]. Defaults to "regr.fixcubist".
make_surrogates_omlbot = function(oml_task_ids, baselearners, measures, surrogate_lrn) {

  if(missing(surrogate_lrn))
    # Obtain fixed cubist from the internet
    source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
    surrogate_lrn = mlr::makeLearner("regr.fixcubist", committees = 20, extrapolation = 20)

  if (missing(oml_task_ids))
    oml_task_ids = get_oml_task_ids()

  if (missing(baselearners))
    baselearners = get_baselearners()
  if (missing(measures))
    measures = get_measures()

  surrs = foreach(measure_name = "auc", .combine = "c") %:%
    foreach(oml_task_id = oml_task_ids, .combine = "c") %:%
      foreach(baselearner_name = baselearners, .combine = "c") %do% {
        s = SurrogateFromRDS$new(
        oml_task_id = oml_task_id,
        baselearner_name = baselearner_name,
        data_source = "data/oml_bot_data.RDS",
        measure_name = measure_name,
        surrogate_learner = surrogate_lrn)
      }
  sc = SurrogateCollection$new(surrs)
  return(sc)
}


#' Train all surrogates for a given surrogate collection.
#' @param sc [SurrogateCollection] if missing, calls make_surrogate_omlbot().
#' @param overwrite should existing surrogates be overwritten?
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

