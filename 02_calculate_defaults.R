# This file computes the defaults for a given learner.

# The computation is quite expensive, thus we parallelize it to 19 cores.
library(devtools)
library(doParallel)
library(surrogates)
load_all()
load_all("../surrogates")
registerDoParallel(8)
registerDoSEQ()


# -----------  Optimizing AUC for different learners:-------------------------------------
# xgboost
sc = make_surrogates_omlbot(baselearners = "xgboost", measures = "auc")
res_xgb = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 8L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# svm
sc = make_surrogates_omlbot(baselearners = "svm", measures = "auc")
res_svm = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# ranger
sc = make_surrogates_omlbot(baselearners = "ranger", measures = "auc")
res_ranger = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$evaluate_defaults_holdout()
}

# glmnet
sc = make_surrogates_omlbot(baselearners = "glmnet", measures = "auc")
res_glmnet = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# Defaults across all learners
sc = make_surrogates_omlbot(baselearners = c("svm", "xgboost", "ranger", "glmnet", "rpart"), measures = "auc")
res_all = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, holdout_task_id = oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}


# -----------  Optimizing AUC scaled by runtime for different learners:-------------------
# xgboost
sc = make_surrogates_omlbot(baselearners = "xgboost", measures = "auc", timecrit = TRUE)
res_xgb_timesense = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# svm
sc = make_surrogates_runtime_omlbot(baselearners = "svm", measures = "auc")
res_svm = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
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

