# Eval Hyperparams using OpenML datasets
# @param task OpenML Task Id | OMLTask
# @param lrn Learner
# @param pars Single param Configuration
evalParsOpenML = function(task, lrn, fun = runTaskMlr2) {
  
  # Run Task in a separate process
  # tryCatch(
  #   run = callr::r(
  #     function(task, lrn, measures, fun) {do.call(fun, list(task, lrn, measures))},
  #     list(task = task, lrn = lrn, measures =  list(auc, f1), fun = fun),
  #     error = "stack", show = TRUE),
  #   error = function(e) print(e$stack)
  # )
  run = runTaskMlr2(task, lrn, measures = list(auc, f1))
  
  # FIXME: Eventually upload the run
  perf = getBMRAggrPerformances(run$bmr, as.df = TRUE)
  perf$task.id = as.character(perf$task.id)
  perf$learner.id = as.character(perf$learner.id)
  
  return(perf)
}

evalDefaultsOpenML = function(task.ids, lrn, defaults, ps, it, n, overwrite = FALSE) {
  defaults = defaults[[it]][seq_len(n), , drop = FALSE]
  evalOpenML("design", task.ids, lrn, defaults, ps, it, n, overwrite)
}

evalRandomSearchOpenML = function(task.ids, lrn, defaults, ps, it, n, overwrite = FALSE) {
  evalOpenML("random", task.ids, lrn, defaults, ps, it, n, overwrite)
}

# Eval Hyperparams using OpenML datasets
# @param task.ids  Vector of OpenML Task Ids
# @param lrn Learner
# @param defaults Set of default parameters
evalOpenML = function(ctrl, task.ids, lrn, defaults, ps, it, n, overwrite = FALSE) {
  
  filepath = stringBuilder("defaultLOOCV/save", stri_paste(ctrl, n, it, "perf", sep = "_"), lrn$id)
  
  if (!file.exists(filepath) | overwrite) {
    # The names of the surrogates are "OpenML Data Id's". We need "OpenML Task Id's.
    data_task_match = read.csv("oml_data_task.txt", sep = " ")
    if (is.character(task.ids)) task.ids = as.numeric(task.ids)
    tasks = foreach(task.id = task.ids) %do% getOMLTask(data_task_match[data_task_match$data.id == task.id, "task.id"])
    
    # In an Wrapper that selects the best default:
    lrn = setPredictType(lrn, "prob")
    
    # Loop over Hold-Out Datasets
    res = lapply(seq_len(length(tasks)), function(i) {
      
      # Define inner Resampling Scheme
      inner.rdesc = cv5 # cv10
      task = tasks[[i]]
      
      # Only take the first 'n' defaults
      if (getLearnerPackages(lrn) == "e1071") {
        lrn = setHyperPars(lrn, "type" = "C-classification")
        defaults$type = "C-classification"
      }
      # Only use one thread for xbg
      if (getLearnerPackages(lrn) == "xgboost") {
        lrn = setHyperPars(lrn, "nthread" = 1L)
        defaults$nthread = 1L
      }
      
      # Get Paramset on original scale, drop unused params 
      # and make sure they are in the same order as the design
      lrn.ps = fixParamSetForDesign(defaults, lrn)
      # Search over the n defaults
      defaults = fixDefaultsForWrapper(defaults, lrn, lrn.ps)
      
      
      if (getLearnerPackages(lrn) == "xgboost") {
        lrn = makeDummyFeaturesWrapper(lrn)
      }
      if (ctrl == "design") {
        tune.ctrl = makeTuneControlDesign(same.resampling.instance = TRUE, design = defaults)
      } else if (ctrl == "random") {
        lrn.ps = ps # We want to tune over the optimized param space
        tune.ctrl = makeTuneControlRandom(same.resampling.instance = TRUE, maxit = n)
      }
      lrn.tune = makeTuneWrapper(lrn, inner.rdesc, auc, par.set = lrn.ps, tune.ctrl)
      res = evalParsOpenML(task, lrn.tune)
    })
    res = do.call("rbind", res)
    res$search.type = ctrl
    res$n = n
    saveRDS(res, filepath)
  } else {
    res = readRDS(filepath)
  }
  return(res)
}


# Convert factors to character, eventually filter invalid params
fixDefaultsForWrapper = function(pars, lrn, ps, check.feasible = TRUE) {
  
  # Convert factor params to character
  pars = data.frame(lapply(pars, function(x) {if (is.factor(x)) x = as.character(x); x}), stringsAsFactors = FALSE)
  
  # Filter invalid params
  if (check.feasible) {
    ps = getParamSet(lrn)
    for (nm in names(pars)) {
      for(n in seq_len(nrow(pars))) {
        if(!isFeasible(ps$pars[[nm]], pars[n, nm], use.defaults = TRUE, filter = TRUE))
          pars[n, nm] = NA
      }
    }
  }
  return(pars)
}

fixParamSetForDesign = function(defaults, lrn) {
  # Get ParamSet and reduce to relevant ones.
  lrn.ps = getParamSet(lrn)
  lrn.ps$pars = lrn.ps$pars[colnames(defaults)]
  # Add NA for hierarchical params
  lrn.ps$pars = lapply(lrn.ps$pars, function(x) {
    x$special.vals = list(NA)
    x$len = 1L
    return(x)
  })
  return(lrn.ps)
}

# Copy of runTaskMlr from Package OpenML without annoying set.seed behaviour.
runTaskMlr2 = function(task, learner, measures = NULL,  scimark.vector = NULL, models = TRUE, ...) {
  checkmate::assert(checkmate::checkString(learner), checkmate::checkClass(learner, "Learner"))
  if (is.character(learner))
    learner = mlr::makeLearner(learner)
  checkmate::assertClass(task, "OMLTask")
  checkmate::assertChoice(task$task.type, c("Supervised Classification", "Supervised Regression"))
  if (!is.null(scimark.vector))
    checkmate::assertNumeric(scimark.vector, lower = 0, len = 6, finite = TRUE, any.missing = FALSE, all.missing = FALSE)
  
  # create parameter list
  parameter.setting = OpenML::makeOMLRunParList(learner)
  
  # set default evaluation measure for classification and regression
  if (task$input$evaluation.measures == "") {
    if (task$task.type == "Supervised Classification")
      task$input$evaluation.measures = "predictive_accuracy"
    else
      task$input$evaluation.measures = "root_mean_squared_error"
  }
  
  # create Flow
  flow = OpenML::convertMlrLearnerToOMLFlow(learner)
  
  # Create mlr task with estimation procedure and evaluation measure
  z = OpenML::convertOMLTaskToMlr(task, measures = measures, ...)
  
  # Create OMLRun
  bmr = mlr::benchmark(learner, z$mlr.task, z$mlr.rin, measures = z$mlr.measures,
    models = models)
  res = bmr$results[[1]][[1]]
  
  # add error message
  tr.err = unique(res$err.msgs$train)
  pr.err = unique(res$err.msgs$predict)
  if (any(!is.na(tr.err))) {
    tr.msg = paste0("Error in training the model: \n ", dplyr::collapse(tr.err, sep = "\n "))
  } else {
    tr.msg = NULL
  }
  if (any(!is.na(pr.err))) {
    pr.msg = paste0("Error in making predictions: \n ", dplyr::collapse(pr.err, sep = "\n "))
  } else {
    pr.msg = NULL
  }
  msg = paste0(tr.msg, pr.msg)
  
  # create run
  run = OpenML::makeOMLRun(task.id = task$task.id,
    error.message = ifelse(length(msg) == 0, NA_character_, msg))
  run$predictions = OpenML:::reformatPredictions(res$pred$data, task)
  
  
  if (!is.null(scimark.vector)) {
    run$scimark.vector = scimark.vector
  }
  BBmisc::makeS3Obj("OMLMlrRun", run = run, bmr = bmr, flow = flow)
}