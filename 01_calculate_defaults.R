# This file computes the defaults for a given learner.

# The computation is quite expensive, thus we parallelize it.
load_all()
library(doParallel)
library(surrogates)
registerDoParallel(19)

# svm, xgboost
sc = make_surrogates_omlbot(baselearners = c("svm", "xgboost"))
res1 = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 10 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# xgboost
sc = make_surrogates_omlbot(baselearners = "xgboost")
res_xgb = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 10 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# svm
sc = make_surrogates_omlbot(baselearners = "svm")
res_svm = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 10 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# ranger
sc = make_surrogates_omlbot(baselearners = "ranger")
res_ranger = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 10 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$evaluate_defaults_holdout()
}

# glmnet
sc = make_surrogates_omlbot(baselearners = "glmnet")
res_glmnet = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 10 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# all
sc = make_surrogates_omlbot(baselearners = c("svm", "xgboost", "ranger", "glmnet", "rpart"))
res_all = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 10 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, holdout_task_id = oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# xgboost with < 100 iterations
sc = make_surrogates_omlbot(baselearners = "xgboost", measures = "auc")
res_xgb_sub100 = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 32 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median", prefix = "sub100")
  ds$ps[["xgboost"]]$pars$nrounds$upper = 100L # We set the upper nrounds to 100L
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}
