# This file computes the defaults for a given learner.
# The computation is quite expensive, thus we parallelize it to 19 cores.
library(devtools)
library(doParallel)
load_all("../surrogates")
load_all()
registerDoParallel(8)

# -----------  Constants  ----------------------------------------------------------------
n_defaults = 16
measures = "auc"
aggfun = "mix"
oml_task_ids = get_oml_task_ids()


# -----------  Optimizing AUC for different learners:-------------------------------------
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
sc_xgbt = make_surrogates_omlbot(baselearners = "xgboost", measures = measures, scaler = ScalerTimeCrit$new())
res_xgbt = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_xgbt, n_defaults, oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  get_holdout_perf(sc_xgb, ds$defaults.params, oml_task_id)
}

# svm ~21 minutes
sc_svmt = make_surrogates_omlbot(baselearners = "svm", measures = measures, scaler = ScalerTimeCrit$new())
res_svmt = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_svmt, n_defaults, oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  get_holdout_perf(sc_svm, ds$defaults.params, oml_task_id)
}


# ranger
sc_rangert = make_surrogates_omlbot(baselearners = "ranger", measures = measures, scaler = ScalerTimeCrit$new())
res_rangert = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_rangert, n_defaults, oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  get_holdout_perf(sc_ranger, ds$defaults.params, oml_task_id)
}

# glmnet
sc_glmnett = make_surrogates_omlbot(baselearners = "glmnet", measures = measures, scaler = ScalerTimeCrit$new())
res_glmnett = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_glmnett, n_defaults, oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  get_holdout_perf(sc_glmnet, ds$defaults.params, oml_task_id)
}

# Defaults across all learners
sc_allt = make_surrogates_omlbot(baselearners = c("svm", "xgboost", "ranger", "glmnet", "rpart"),
  measures = measures, scaler = ScalerTimeCrit$new())
res_allt = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_allt, n_defaults, holdout_task_id = oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  get_holdout_perf(sc_all, ds$defaults.params, oml_task_id)
}



# -----------  Runtime Prediction  -------------------------------------------------------
sc_xgb_runtime = make_surrogates_omlbot(baselearners = "xgboost", measures = "runtime")

ds = DefaultSearch$new(sc_xgb_runtime, n_defaults, holdout_task_id = NULL, aggfun)
res_xgb_rs_t = foreach(n_points = seq_len(16), .combine = "rbind") %dopar% {
      res = replicate(10, { # Replications to reduce stochasticity
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
  colnames(df) = paste0(oml_task_id, "_runtime")
  return(df)
}

res_xgb_def_t = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_xgb, n_defaults, oml_task_id, aggfun)
  ds$search_defaults()
  ds$save_to_disk()
  # Get holdout performance
  df = data.frame(unlist(sc_xgb_runtime$predict(ds$defaults.params, oml_task_id)))
  colnames(df) = paste0(oml_task_id, "_runtime")
  return(df)
}

# -----------  SKLEARN   -----------------------------------------------------------------
# adaboost
sc_ada = make_surrogates_sklearn(oml_task_ids = get_sklearn_task_ids(), base_learners = "adaboost")
res_ada = foreach(oml_task_id = get_sklearn_task_ids(), .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_ada, n_defaults, oml_task_id, aggfun)
  ds$ctrl$points = 10^3
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# libsvm_svc
sc_libsvm_svc = make_surrogates_sklearn(oml_task_ids = get_sklearn_task_ids(), base_learners = "libsvm_svc")
res_libsvm_svc = foreach(oml_task_id = get_sklearn_task_ids(), .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_libsvm_svc, n_defaults, oml_task_id, aggfun)
  ds$ctrl$points = 10^3
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

# random_forest (results are only available for ~90/100 datasets)
df_rf = farff::readARFF("data/input/sklearn_oml100/random_forest.arff")
oml_task_ids = get_sklearn_task_ids()[get_sklearn_task_ids() %in% unique(df_rf$task_id)]
sc_random_forest = make_surrogates_sklearn(oml_task_ids = oml_task_ids, base_learners = "random_forest")
res_random_forest = foreach(oml_task_id = oml_task_ids, .combine = "cbind") %dopar% {
  # Search  Defaults, hold out task x
  ds = DefaultSearch$new(sc_random_forest, n_defaults, oml_task_id, aggfun)
  ds$ctrl$points = 10^3
  ds$search_defaults()
  ds$save_to_disk()
  ds$get_holdout_performance()
}

