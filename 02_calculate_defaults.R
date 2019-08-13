# This file computes the defaults for a given learner.

# The computation is quite expensive, thus we parallelize it to 19 cores.
library(devtools)
library(doParallel)
library(surrogates)
load_all()
load_all("../surrogates")
registerDoParallel(8)
registerDoSEQ()

# -----------  Constants  -------------------------------------------------------
n_defaults = 16
measures = "auc"
aggfun = "median"
oml_task_ids = get_oml_task_ids()

# -----------  Optimizing AUC for different learners:----------------------------
# xgboost
sc_xgb = make_surrogates_omlbot(baselearners = "xgboost", measures = measures)
res_xgb = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_xgb, n_defaults, oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# svm
sc_svm = make_surrogates_omlbot(baselearners = "svm", measures = measures)
res_svm = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_svm, n_defaults, oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# ranger
sc_ranger = make_surrogates_omlbot(baselearners = "ranger", measures = measures)
res_ranger = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_ranger, n_defaults, oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  ds$evaluate_defaults_holdout()
}

# glmnet
sc_glmnet = make_surrogates_omlbot(baselearners = "glmnet", measures = measures)
res_glmnet = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_glmnet, n_defaults, oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# Defaults across all learners
sc_all = make_surrogates_omlbot(baselearners = c("svm", "xgboost", "ranger", "glmnet", "rpart"), measures = measures)
res_all = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_all, n_defaults, holdout_task_id = oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}



# -----------  Optimizing AUC scaled by runtime for different learners:-------------------

# xgboost
sc_xgbt = make_surrogates_omlbot(baselearners = "xgboost", measures = measures, timecrit = TRUE)
res_xgbt = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_xgbt, n_defaults, oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  get_holdout_perf(sc_xgbt, ds$defaults.params, oml_task_id)
}

# svm ~21 minutes
sc_svmt = make_surrogates_omlbot(baselearners = "svm", measures = measures, timecrit = TRUE)
res_svmt = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_svmt, n_defaults, oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  get_holdout_perf(sc_svm, ds$defaults.params, oml_task_id)
}


# ranger
sc_rangert = make_surrogates_omlbot(baselearners = "ranger", measures = measures, timecrit = TRUE)
res_rangert = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_rangert, n_defaults, oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  ds$evaluate_defaults_holdout()
}

# glmnet
sc_glmnett = make_surrogates_omlbot(baselearners = "glmnet", measures = measures, timecrit = TRUE)
res_glmnett = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_glmnett, n_defaults, oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  get_holdout_perf(sc_glmnet, ds$defaults.params, oml_task_id)
}

# Defaults across all learners
sc_allt = make_surrogates_omlbot(baselearners = c("svm", "xgboost", "ranger", "glmnet", "rpart"), measures = measures, timecrit = TRUE)
res_allt = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_allt, n_defaults, holdout_task_id = oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  get_holdout_perf(sc_all, ds$defaults.params, oml_task_id)
}



# -----------  Runtime Prediction:--------------------------------------------------------
sc_xgb_runtime = make_surrogates_omlbot(baselearners = "xgboost", measures = "runtime")
ds = DefaultSearch$new(sc_xgb_runtime, n_defaults, holdout_task_id = NULL, aggfun)
res_xgb_rs_t = foreach(n_points = seq_len(16), .combine = "rbind") %dopar% {
      res = replicate(30, { # 30 Replications to reduce stochasticity
        ds$ctrl$points = n_points
        ds$do_random_search()$opt.prds
      })
      apply(res, 2, mean)
}

res_xgb_tc_t = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_xgbt, n_defaults, oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  # Get holdout performance
  df = data.frame(unlist(sc_xgb_runtime$predict(ds$defaults.params, oml_task_id)))
  colnames(df) = paste0(oml_task_id, "_auc")
  return(df)
}

res_xgb_def_t = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_xgb, n_defaults, oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  # Get holdout performance
  df = data.frame(unlist(sc_xgb_runtime$predict(ds$defaults.params, oml_task_id)))
  colnames(df) = paste0(oml_task_id, "_auc")
  return(df)
}
