# This file computes the defaults for a given learner.

# The computation is quite expensive, thus we parallelize it to 19 cores.
library(devtools)
library(doParallel)
library(surrogates)
load_all()
load_all("../surrogates")
registerDoParallel(8)


# -----------  Optimizing AUC for different learners:-------------------------------------
# Surrogate collection across all learners
sc_all = make_surrogates_omlbot(baselearners = c("svm", "xgboost", "ranger", "glmnet", "rpart"), measures = "auc")

# xgboost
sc_xgb = make_surrogates_omlbot(baselearners = "xgboost", measures = "auc")
res_xgb = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc_xgb, 16L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# svm
sc = make_surrogates_omlbot(baselearners = "svm", measures = "auc")
res_svm = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 16L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# ranger
sc = make_surrogates_omlbot(baselearners = "ranger", measures = "auc")
res_ranger = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 16L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$evaluate_defaults_holdout()
}

# glmnet
sc = make_surrogates_omlbot(baselearners = "glmnet", measures = "auc")
res_glmnet = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 16L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# Defaults across all learners
res_all = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc_all, 16L, holdout_task_id = oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# -----------  Optimizing AUC scaled by runtime for different learners:-------------------
# xgboost
sc_xgbt = make_surrogates_omlbot(baselearners = "xgboost", measures = "auc", timecrit = TRUE)
sc_xgb  = make_surrogates_omlbot(baselearners = "xgboost", measures = "auc") # Get OOB Performance
res_xgb_timesense = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc_xgbt, 16L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  # Get holdout performance
  df = data.frame(unlist(sc_xgb$predict(ds$defaults.params, oml_task_id)))
  colnames(df) = paste0(oml_task_id, "_auc")
  return(df)
}

# svm
sc = make_surrogates_runtime_omlbot(baselearners = "svm", measures = "auc")
res_svm = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 8L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# ranger
sc = make_surrogates_runtime_omlbot(baselearners = "ranger", measures = "auc")
res_ranger = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$evaluate_defaults_holdout()
}

# glmnet
sc = make_surrogates_runtime_omlbot(baselearners = "glmnet", measures = "auc")
res_glmnet = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# Defaults across all learners
sc = make_surrogates_runtime_omlbot(baselearners = c("svm", "xgboost", "ranger", "glmnet", "rpart"), measures = "auc")
res_all = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, holdout_task_id = oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# -----------  Random Search:-------------------------------------
sc_xgb = make_surrogates_omlbot(baselearners = "xgboost", measures = "auc")
ds = DefaultSearch$new(sc_xgb, 16L, holdout_task_id = NULL, "median")

res_xgb_rs = foreach(n_points = seq_len(16), .combine = "rbind") %dopar% {
      res = replicate(30, { # 30 Replications to reduce stochasticity
        ds$ctrl$points = n_points
        ds$do_random_search()$opt.prds
      })
      apply(res, 2, mean)
}

res_xgb_rs2 = foreach(n_points = seq_len(16)*2, .combine = "rbind") %dopar% {
      res = replicate(30, { # 30 Replications to reduce stochasticity
        ds$ctrl$points = n_points
        ds$do_random_search()$opt.prds
      })
      apply(res, 2, mean)
}

res_xgb_rs4 = foreach(n_points = seq_len(16)*4, .combine = "rbind") %dopar% {
      res = replicate(30, {
        ds$ctrl$points = n_points
        ds$do_random_search()$opt.prds
      })
      apply(res, 2, mean)
}


# -----------  Runtime Prediction:--------------------------------------------------------
sc_xgb_runtime = make_surrogates_omlbot(baselearners = "xgboost", measures = "runtime")

ds = DefaultSearch$new(sc_xgb_runtime, 16L, holdout_task_id = NULL, "median")
res_xgb_rs_t = foreach(n_points = seq_len(16), .combine = "rbind") %dopar% {
      res = replicate(30, { # 30 Replications to reduce stochasticity
        ds$ctrl$points = n_points
        ds$do_random_search()$opt.prds
      })
      apply(res, 2, mean)
}

res_xgb_tc_t = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc_xgbt, 16L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  # Get holdout performance
  df = data.frame(unlist(sc_xgb_runtime$predict(ds$defaults.params, oml_task_id)))
  colnames(df) = paste0(oml_task_id, "_auc")
  return(df)
}

res_xgb_def_t = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc_xgb, 16L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  # Get holdout performance
  df = data.frame(unlist(sc_xgb_runtime$predict(ds$defaults.params, oml_task_id)))
  colnames(df) = paste0(oml_task_id, "_auc")
  return(df)
}
