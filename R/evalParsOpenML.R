# Eval Hyperparams using OpenML datasets
# @param task OpenML Task Id | OMLTask
# @param lrn Learner
# @param pars Single param Configuration
evalParsOpenML = function(task, lrn) {
  
  # Run Task
  run = runTaskMlr2(task, lrn, measures = list(auc, f1))
  
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
evalDefaultsOpenML = function(task.ids, lrn, defaults, ps, it, n) {
  
  # The names of the surrogates are "OpenML Data Id's". We need "OpenML Task Id's.
  data_task_match = read.csv("oml_data_task.txt", sep = " ")
  if (is.character(task.ids)) task.ids = as.numeric(task.ids)
  tasks = foreach(task.id = task.ids) %do% getOMLTask(data_task_match[data_task_match$data.id == task.id, "task.id"])
  
  # In an Wrapper that selects the best default:
  lrn = setPredictType(lrn, "prob")
  
  # Loop over Hold-Out Datasets
  res = lapply(seq_len(length(tasks)), function(i) {
    # Define inner Resampling Scheme
    inner.rdesc = hout # cv10
    
    # Only take the first 'n' defaults
    defaults = defaults[[it]][seq_len(n), ]
    
    # Search over the n defaults
    defaults = fixDefaultsForWrapper(defaults, lrn)
    lrn.def = makeTuneWrapper(lrn, inner.rdesc, auc, par.set = ps, makeTuneControlDesign(design = defaults))
    res.def = evalParsOpenML(tasks[[i]], lrn.def)
    
    if (TRUE) {
      # Search randomly (2x randomsearch)
      lrn.rnd2 = makeTuneWrapper(lrn, inner.rdesc, auc, par.set = ps, makeTuneControlRandom(same.resampling.instance = FALSE,
        maxit = 2 * nrow(defaults)))
      res.rndx2 = evalParsOpenML(tasks[[i]], lrn.rnd2)
      
      # Search randomly (4x randomsearch)
      lrn.rnd4 = makeTuneWrapper(lrn, inner.rdesc, auc, par.set = ps, makeTuneControlRandom(maxit = 4 * nrow(defaults)))
      res.rndx4 = evalParsOpenML(tasks[[i]], lrn.rnd4)
      
      # Search randomly (8x randomsearch)
      lrn.rnd8 = makeTuneWrapper(lrn, inner.rdesc, auc, par.set = ps, makeTuneControlRandom(maxit = 8 * nrow(defaults)))
      res.rndx8 = evalParsOpenML(tasks[[i]], lrn.rnd8)
      
      # Return a data.frame
      df = bind_rows("defaults" = res.def, "X2" = res.rndx2, "X4" = res.rndx4,
        "X8" = res.rndx8, .id = "search.type")
      df$n.defaults = n
    } else {
      df = NULL
    }
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


# Copy of runTaskMlr from Package OpenML without annoying set.seed behaviour.
runTaskMlr2 = function(task, learner, measures = NULL, verbosity = NULL,
  scimark.vector = NULL, models = TRUE, ...) {
  assert(checkString(learner), checkClass(learner, "Learner"))
  if (is.character(learner))
    learner = mlr::makeLearner(learner)
  assertClass(task, "OMLTask")
  assertChoice(task$task.type, c("Supervised Classification", "Supervised Regression"))
  if (!is.null(scimark.vector))
    assertNumeric(scimark.vector, lower = 0, len = 6, finite = TRUE, any.missing = FALSE, all.missing = FALSE)
  
  # create parameter list
  parameter.setting = makeOMLRunParList(learner)
  
  # set default evaluation measure for classification and regression
  if (task$input$evaluation.measures == "") {
    if (task$task.type == "Supervised Classification")
      task$input$evaluation.measures = "predictive_accuracy"
    else
      task$input$evaluation.measures = "root_mean_squared_error"
  }
  
  # get mlr show.info from verbosity level
  if (is.null(verbosity))
    verbosity = getOMLConfig()$verbosity
  show.info = (verbosity > 0L)
  
  # create Flow
  flow = convertMlrLearnerToOMLFlow(learner)
  
  # Create mlr task with estimation procedure and evaluation measure
  z = convertOMLTaskToMlr(task, measures = measures, verbosity = verbosity, ...)
  
  # Create OMLRun
  bmr = mlr::benchmark(learner, z$mlr.task, z$mlr.rin, measures = z$mlr.measures,
    models = models, show.info = show.info)
  res = bmr$results[[1]][[1]]
  
  # add error message
  tr.err = unique(res$err.msgs$train)
  pr.err = unique(res$err.msgs$predict)
  if (any(!is.na(tr.err))) {
    tr.msg = paste0("Error in training the model: \n ", collapse(tr.err, sep = "\n "))
  } else {
    tr.msg = NULL
  }
  if (any(!is.na(pr.err))) {
    pr.msg = paste0("Error in making predictions: \n ", collapse(pr.err, sep = "\n "))
  } else {
    pr.msg = NULL
  }
  msg = paste0(tr.msg, pr.msg)
  
  # create run
  run = makeOMLRun(task.id = task$task.id,
    error.message = ifelse(length(msg) == 0, NA_character_, msg))
  run$predictions = OpenML:::reformatPredictions(res$pred$data, task)
  

  if (!is.null(scimark.vector)) {
    run$scimark.vector = scimark.vector
  }
  makeS3Obj("OMLMlrRun", run = run, bmr = bmr, flow = flow)
}