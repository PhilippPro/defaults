# Eval Hyperparams using OpenML datasets
# @param task OpenML Task Id | OMLTask
# @param lrn Learner
# @param pars Single param Configuration
evalParsOpenML = function(task, lrn, pars) {
  # Convert factor params to character
  pars = lapply(pars, function(x) {if (is.factor(x)) x = as.character(x); x})
  # Filter invalid params
  ps = getParamSet(lrn)
  pars = pars[sapply(names(pars), function(x) {isFeasible(ps$pars[[x]], pars[[x]])})]
  
  lrn = setHyperPars(lrn, par.vals = pars)
  
  # Download task if required
  if (is.numeric(task)) {
    task = getOMLTask(task)
  } else if (inherits(task, "OMLTask")) {
    task = task
  }
  
  # Run Task
  run = runTaskMlr(task, lrn)
  
  # FIXME: Eventually upload the run
  return(getBMRAggrPerformances(run$bmr, as.df = TRUE))
}

# Eval Hyperparams using OpenML datasets
# @param task.ids  Vector of OpenML Task Ids
# @param lrn Learner
# @param defaults Set of default parameters
evalDefaultsOpenML = function(task.ids, lrn, defaults) {
  
  if (is.character(task.ids)) task.ids = as.numeric(task.ids)
  
  # Get Tasks if not available
  tasks = foreach(task.id = task.ids) %do% getOMLTask(task.id)
  
  n = seq_len(nrow(defaults))
  perf = foreach(task = tasks, .combine = "rbind") %:%
    foreach(par = n, .combine = "rbind") %dopar% {
      evalParsOpenML(task, lrn, defaults[par, , drop = FALSE])
    }
  perf$n = rep(n, length(tasks))
  
  return(perf)
}