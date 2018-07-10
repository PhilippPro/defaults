# Eval Hyperparams using OpenML datasets
# @param task OpenML Task Id | OMLTask
# @param lrn Learner
# @param pars Single param Configuration
evalParsOpenML = function(task, lrn) {
  
  # Run Task
  run = runTaskMlr(task, lrn, measures = list(auc, f1))
  
  # FIXME: Eventually upload the run
  perf = getBMRAggrPerformances(run$bmr, as.df = TRUE)
  perf$task.id = as.character(perf$task.id)
  perf$learner.id = as.character(perf$learner.id)
  
  return(perf)
}

# Eval Hyperparams using OpenML datasets
# @param task.ids  Vector of OpenML Task Ids
# @param lrn Learner
# @param defaults Set of default parameters
evalDefaultsOpenML = function(task.ids, lrn, defaults, ps, n) {
  
  if (is.character(task.ids)) task.ids = as.numeric(task.ids)

  # The names of the surrogates are "OpenML Data Id's". We need "OpenML Task Id's.
  data_task_match = read.csv("oml_data_task.txt", sep = " ")
  tasks = foreach(task.id = task.ids) %do% getOMLTask(data_task_match[data_task_match$data.id == task.id, "task.id"])
  
  # In an Wrapper that selects the best default:
  lrn = setPredictType(lrn, "prob")
  
  # Loop over Hold-Out Datasets
  res = lapply(seq_len(length(tasks)), function(i) {
    # Define inner Resampling Scheme
    inner.rdesc = hout
    
    # Only take the first 'n' defaults
    defaults = defaults[seq_len(n), ]
    
    # Search over the n defaults
    defaults = fixDefaultsForWrapper(defaults, lrn)
    lrn.def = makeTuneWrapper(lrn, inner.rdesc, auc, par.set = ps, makeTuneControlDesign(design = defaults))
    res.def = evalParsOpenML(tasks[[i]], lrn.def)
    
    # Search randomly (2x randomsearch)
    lrn.rnd2 = makeTuneWrapper(lrn, inner.rdesc, auc, par.set = ps, makeTuneControlRandom(maxit = 2 * nrow(defaults)))
    res.rndx2 = evalParsOpenML(tasks[[i]], lrn.rnd2)
    
    # Search randomly (4x randomsearch)
    lrn.rnd4 = makeTuneWrapper(lrn, inner.rdesc, auc, par.set = ps, makeTuneControlRandom(maxit = 4 * nrow(defaults)))
    res.rndx4 = evalParsOpenML(tasks[[i]], lrn.rnd4)
    
    # Return a data.frame
    df = bind_rows("default" = res.def, "rndX2" = res.rndx2, "rndX4" = res.rndx4, .id = "search.type")
    df$n.defaults = n
    return(df)
  })
  do.call("bind_rows", res)
}

# Convert factors to character, eventually filter invalid params
fixDefaultsForWrapper = function(pars, lrn) {
  # Convert factor params to character
  pars = sapply(pars, function(x) {if (is.factor(x)) x = as.character(x); x})
  # Filter invalid params
  # ps = getParamSet(lrn)
  # pars = pars[sapply(names(pars), function(x) {isFeasible(ps$pars[[x]], pars[[x]])})]
  return(data.frame(pars))
}