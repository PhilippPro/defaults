# Obtains data from figshare, saves a data.frame
figshare_to_data = function(bot_data = "data/oml_bot_data.RDS") {
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
      bot.table %>%
       rename(learner_id = fullName) %>%
       select(-run_id)
    })
  saveRDS(file = bot_data, learner_feats_list %>% group_by(learner_id) %>% select(-fullName))
}


make_surrogates_omlbot = function() {
  # Obtain fixed cubist from the internet
  source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
  SURROGATE_LRN = mlr::makeLearner("regr.fixcubist", committees = 20, extrapolation = 20)

  OML_TASK_IDS = c(3L, 31L, 37L, 43L, 49L, 219L, 3485L, 3492L, 3493L, 3494L, 3889L,
    3891L, 3896L, 3899L, 3902L, 3903L, 3913L, 3917L, 3918L, 3954L,
    34536L, 14971L, 14965L, 10093L, 10101L, 9980L, 9983L, 9970L,
    9971L, 9976L, 9977L, 9978L, 9952L, 9957L, 9967L, 9946L, 9914L,
    14966L, 34539L, 34537L)
  BASE_LEARNERS = c("glmnet", "rpart", "svm", "ranger", "xgboost")
  MEASURES = c("auc", "acc", "brier")

  surrs = foreach(measure_name = "auc", .combine = "c") %:%
    foreach(oml_task_id = OML_TASK_IDS, .combine = "c") %:%
      foreach(baselearner_name = BASE_LEARNERS, .combine = "c") %do% {
        s = SurrogateFromRDS$new(
        oml_task_id = oml_task_id,
        baselearner_name = baselearner_name,
        data_source = "data/oml_bot_data.RDS",
        measure_name = measure_name,
        surrogate_learner = SURROGATE_LRN)
      }
  return(surrs)
}

train_surrogates_omlbot = function() {
  require(doParallel)
  registerDoParallel(12L)
  surrs = make_surrogates()
  not_trained = seq_along(surrs)[sapply(surrs, function(x) !x$is_trained)]
  doParallel::registerDoParallel(12)
  foreach(s = surrs[not_trained]) %dopar% {
    gc()
    s$train()
    NULL
  }
  invisible(NULL)
}

