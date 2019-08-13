library("OpenML")
library("reticulate")
library("fail")
library("mlr")
library("BBmisc")
library("batchtools")
library("doParallel")
# library("doParallel")
# setOMLConfig(cachedir = "/naslx/projects/pr74ze/di57qoy2/oml_cache")
setOMLConfig(cachedir="/home/flo/Documents/projects/oml_cache")
source("R/RLearner_classif_autosklearn.R")

make_autosklearner = function(n, baselearner, setting = "metalearn") {
  n_meta = ifelse(setting == "metalearn", as.integer(n), 0L)
  makeLearner("classif.autosklearn",
    time_left_for_this_task = as.integer(n) * 60L * 60L + 1L,
    per_run_time_limit = 60L * 60L,
    initial_configurations_via_metalearning = n_meta,
    ensemble_size = 1L,
    ensemble_nbest = as.integer(n),
    smac_scenario_args = list("runcount_limit" = as.integer(n)),
    include_estimators = as.list(baselearner),
    include_preprocessors = list("no_preprocessing"),
    resampling_strategy = "cv",
    resampling_strategy_arguments = list("folds" = 3L)
  )
}

run_single_cfg = function(baselearner, task_id, n, baseline_setting, fail_handle) {
  cfg = paste("run", baselearner, task_id, n, baseline_setting, sep = "_")
  if (!(cfg %in% fail_handle$ls())) {
    sprintf("Computing %s", cfg)
    lrn = make_autosklearner(n, baselearner = baselearner)
    lrn = makeDummyFeaturesWrapper(lrn)
    lrn = makeRemoveConstantFeaturesWrapper(lrn)
    browser()
    lrn = setLearnerId(lrn,
      paste("autosklearn", baseline_setting, n, paste0(baselearner, collapse = "_"), sep = "_"))
    tsk = getOMLTask(task_id)
    run = OpenML::runTaskMlr(tsk, lrn)
    fail_handle$put(run, keys = cfg)
    sprintf("Saved %s", cfg)
  } else {
    run = fail_handle$get(cfg)
  }
  return(run)
}


compute_baselines = function() {
  # Set up target directory and list of tasks
  oml100 = listOMLTasks(tag = "OpenML100")
  TASK_IDS = oml100$task.id
  BASELEARNERS = "random_forest" # c("random_forest", "adaboost", "libsvm_svc")
  BASELINE_SETTINGS = "smac" # c("metalearn", "smac")
  N_EVALS = c(1, 2) # , 4, 8) # 16, 32)

  fail_handle = fail::fail("baselines/results")
  foreach(baseline_setting = BASELINE_SETTINGS) %:%
    foreach(baselearner = BASELEARNERS) %:%
      foreach(n = N_EVALS) %:%
        foreach(task_id = TASK_IDS) %do% {
          parallelMap::parallelStartMulticore(10, level = "mlr.resample")
          parallelMap::parallelExport("make_autosklearner")
          parallelMap::parallelLibrary("mlr")
          parallelMap::parallelSource("R/RLearner_classif_autosklearn.R")
          run_single_cfg(baselearner, task_id, n, baseline_setting, fail_handle)
          parallelMap::parallelStop()
        }
}

compute_baselines()


# BATCHTOOLS --------------------------------------------------------------------------------------
compute_baselines_batchtools = function() {
  REG_FILE_DIR = "baselines/autsklearn_baseline"

  # Experiment config
  TASK_IDS = listOMLTasks(tag="OpenML100")$task.id
  BASELEARNERS = c("random_forest", "adaboost", "libsvm_svc")
  BASELINE_SETTINGS = c("metalearn", "smac")
  N_EVALS = c(1, 2, 4, 8, 16, 32)

  reg = loadRegistry(REG_FILE_DIR, writeable = TRUE)
  for (task_id in TASK_IDS) {
    addProblem(reg = reg, name = as.character(task_id), data = list(task_id = task_id))
  }
  addAlgorithm(name = "auto_sklearn", fun = run_task, reg = reg)
  ades = list(
    auto_sklearn = expand.grid(
      baselearner = BASELEARNERS,
      n = N_EVALS,
      baseline_setting = BASELINE_SETTINGS,
      stringsAsFactors = FALSE)
  )
  addExperiments(algo.designs = ades, repls = 1L, reg = reg)
  return(reg)
}

init_new_registry = function() {
  require(batchtools)
  # Delete old registry and create a new one
  unlink(REG_FILE_DIR, recursive = TRUE, force = TRUE)
  # Create a new registry
  reg = makeExperimentRegistry(file.dir = REG_FILE_DIR,
    source = "R/RLearner_classif_autosklearn.R",
    packages = c("farff", "mlr", "data.table", "reticulate", "R6", "OpenML", "BBmisc", "parallelMap"),
    seed = 44445)
}

# Copy from Compute Cluster --------------------------------------------------------------

library(fail)
# fh = fail::fail("baselines/results")
saved = character(0)
files = setdiff(
  list.files("~/lrz/defaults_baselines/autsklearn_baseline/results", full.names = TRUE),
  saved)

saved = foreach(fp = files) %do% {
  f = readRDS(fp)
  tsk_id = f$run$task.id
  pv = getLearnerParVals(f$bmr$learners[[1]])
  nb = pv$ensemble_nbest
  met = pv$initial_configurations_via_metalearning
  setting = ifelse(met == nb, "metalearn", "smac")
  lrn = pv$include_estimators[[1]]
  path = paste("run", lrn, tsk_id, nb, setting, sep = "_")
  if (!(path %in% fh$ls())) fh$put(f, keys = path)
  return(fp)
}

# Run on Compute Cluster --------------------------------------------------------------
reg = loadRegistry("baselines/autsklearn_baseline", writeable = TRUE)
removeProblems(3561)
jt = getJobTable()
jt$baselearner = sapply(jt$algo.pars, function(x) x$baselearner)
jt$baseline_setting = sapply(jt$algo.pars, function(x) x$baseline_setting)
jt$n = sapply(jt$algo.pars, function(x) x$n)
jt = jt[!(sapply(jt$algo.pars, function(x) x$baselearner) == "svm_svc"), ] # Delte those later, its libsvm_svc
jt = jt[!(jt$baselearner == "adaboost" & jt$n %in% c(1, 2)), ] # Computed locally
submitJobs(findNotSubmitted(jt)[1:100])


# Local results
fail_handle = fail("baselines/results")
res = lapply(fail_handle$ls(), function(x) {
  fh = fail_handle$get(x)
  bmr = fh$bmr
  agg = getBMRAggrPerformances(bmr, as.df = TRUE)
  pv = getLearnerParVals(bmr$learner[[1]])
  agg$baselearn = paste0(pv$include_estimators, collapse= "_")
  agg$task.id = fh$run$task.id
  agg$n_evals = pv$ensemble_nbest
  agg$n_meta = pv$initial_configurations_via_metalearning
  return(agg)
})
df = do.call("rbind", res)
library(ggplot2)
ggplot(df, aes(x = as.factor(n_evals), y = acc.test.join, fill = baselearn)) + geom_boxplot() + facet_wrap(~n_meta)
