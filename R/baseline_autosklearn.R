library("OpenML")
library("reticulate")
library("fail")
library("mlr")
library("BBmisc")
library("foreach")
library("doParallel")
use_python("/usr/bin/python3")
source("R/RLearner_classif_autosklearn.R")


make_autosklearner = function(n, baselearner, setting = "metalearn") {
  n_meta = ifelse(setting == "metalearn", as.integer(n), 0L)
  makeLearner("classif.autosklearn",
    time_left_for_this_task = as.integer(n) * 60L * 60L,
    per_run_time_limit = 60L * 60L,
    initial_configurations_via_metalearning = n_meta,
    ensemble_size = 1L,
    ensemble_nbest = as.integer(n),
    smac_scenario_args = list("runcount_limit" = as.integer(n)),
    include_estimators = as.list(baselearner),
    include_preprocessors = list("no_preprocessing"),
    resampling_strategy="cv",
    resampling_strategy_arguments=list("folds" = 3L)
  )
}

run_single_cfg = function(baselearner, task_id, n, baseline_setting, fail_handle) {
  cfg = paste("run", baselearner, task_id, n, baseline_setting, sep = "_")
  if (!(cfg %in% fail_handle$ls())) {
    lrn = make_autosklearner(n, baselearner = baselearner)
    lrn = makeDummyFeaturesWrapper(lrn)
    tsk = getOMLTask(task_id)
    run = OpenML::runTaskMlr(tsk, lrn)
    fail_handle$put(run, keys = cfg)
  } else {
    run = fail_handle$get(cfg)
  }
  return(run)
}


compute_baselines = function() {
  # Set up target directory and list of tasks
  oml100 = listOMLTasks(tag="OpenML100")
  TASK_IDS = oml100$task.id
  BASELINE_SETTINGS = "smac" # c("metalearn", "smac")
  BASELEARNERS = "libsvm_svc"
  N_EVALS = c(2, 4) #c(8, 16)


  fail_handle = fail("baselines/results")
  registerDoParallel(n  = 15)
  foreach(baseline_setting = BASELINE_SETTINGS) %:%
    foreach(baselearner = BASELEARNERS) %:%
      foreach(n = N_EVALS) %:%
        foreach(task_id = TASK_IDS) %dopar% {
          run_single_cfg(baselearner, task_id, n, baseline_setting, fail_handle)
        }

  registerDoParallel(n  = 16)
  foreach(baseline_setting = BASELINE_SETTINGS) %:%
    foreach(baselearner = BASELEARNERS) %:%
      foreach(n = N_EVALS) %:%
        foreach(task_id = TASK_IDS[6:91]) %dopar% {
          run_single_cfg(baselearner, task_id, n, baseline_setting, fail_handle)
        }
  }
