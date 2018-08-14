evalDefaultsOpenML = function(task.ids, lrn, defaults, ps, it, n, aggr.fun = NULL, overwrite = FALSE) {
  defaults = defaults[[it]][seq_len(n), , drop = FALSE]
  if (!(task.ids %in% c("1486", "4134"))) {
    if (!is.null(defs$aggr.fun)) {
      defs.aggr.fun = switch(defs$aggr.fun,
        "mean" = "defaults_mean",
        "cycle" = "defaults_cycle")
    } else {
      defs.aggr.fun = "design" # Median
    }
    evalOpenML(defs.aggr.fun, task.ids, lrn, defaults, ps, it, n, overwrite)
  }
}

evalRandomSearchOpenML = function(task.ids, lrn, defaults, ps, it, n, overwrite = FALSE) {
  defaults = defaults[[it]]
  if (!(task.ids %in% c("1486", "4134")))
    evalOpenML("random", task.ids, lrn, defaults, ps, it, n, overwrite)
}

evalPackageDefaultOpenML = function(task.ids, lrn, defaults, ps, it, n, overwrite = FALSE) {
  defaults = defaults[[it]]
  evalOpenML("package-default", task.ids, lrn, defaults, ps, it, n, overwrite)
}

evalMBOOpenML = function(task.ids, lrn, defaults, ps, it, n, overwrite = FALSE) {
  defaults = defaults[[it]]
  evalOpenML("mbo", task.ids, lrn, defaults, ps, it, n, overwrite)
}

#' Evaluate RandomSearch on RandomBotData
#' # n.rs = number of randomSearch Iters
#' # reps = number of repetition
#' # i learner.names[i]
evalRandomBotData = function(measure = auc, i, n.rs = c(4, 8, 16, 32, 64), reps = 100, overwrite = FALSE) {

  # Create the learner
  lrn = makeLearner(gsub(x = learner.names[i], "mlr.", "", fixed = TRUE))

  # Make sure we do not recompute stuff: Save results to a file and check if file exists
  filepath = stringBuilder("defaultLOOCV/save", stri_paste("randomBotData","all", "perf", sep = "_"), lrn$id)
  if (!file.exists(filepath) | overwrite) {
    data_task_match = read.csv("oml_data_task.txt", sep = " ")
    # Get the data into the right format
    task.data = makeBotTable(measure, learner.names[i], tbl.results, tbl.metaFeatures, tbl.hypPars,
      lrn.par.sets[[i]]$param.set, data.ids = sort(unique(tbl.results$data_id)),
      scale_before = TRUE, scaling = "none")
    # Do different randomSearch iterations
    td = foreach(n = n.rs, .combine = "bind_rows") %:%
      # Repeat random search 100 times
      foreach(reps = seq_len(reps), .combine = "bind_rows") %do% {
        task.data %>%
          group_by(data_id, fullName) %>%
          sample_n(n) %>%
          arrange(measure.value) %>%
          filter(row_number() == 1) %>%
          arrange(data_id) %>%
          select(measure.value, data_id, fullName) %>%
          mutate(n = n)
      }
    # Compute mean and add metadata
    td = td %>%
      group_by(data_id, fullName, n) %>%
      summarise(measure.value = mean(measure.value))
    td[mlr:::measureAggrName(measure)] = 1 - td$measure.value
    res = td %>%
      mutate(task.id = data_task_match[data_task_match$data.id == data_id, "task.name"]) %>%
      rename(learner.id = fullName) %>%
      select(-measure.value)
    if (unique(res$learner.id) == "mlr.classif.xgboost") {
      res$learner.id = "classif.xgboost.dummied.tuned"
    }
    res$task.id = as.factor(res$task.id)
    res$search.type = "randomBotData"
    saveRDS(res, filepath)
  } else {
    res = readRDS(filepath)
  }
  return(res)
}



# Eval Hyperparams using OpenML datasets
# @param task.ids  Vector of OpenML Task Ids
# @param lrn Learner
# @param defaults Set of default parameters
evalOpenML = function(ctrl, task.ids, lrn, defaults, ps, it, n, overwrite = FALSE) {

  filepath = stringBuilder("defaultLOOCV/save", stri_paste(ctrl, n, it, "perf", sep = "_"), lrn$id)
  # For now we skip task.id %in% c("1486") as they are very big
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
      if (ctrl %in% c("design", "defaults_mean", "defaults_cycle")) {
        tune.ctrl = makeTuneControlDesign(same.resampling.instance = TRUE, design = defaults)
        lrn.tune = makeTuneWrapper(lrn, inner.rdesc, mlr::auc, par.set = lrn.ps, tune.ctrl)
      } else if (ctrl == "random") {
        lrn.ps = ps # We want to tune over a nicer param space (with trafos)
        tune.ctrl = makeTuneControlRandom(same.resampling.instance = TRUE, maxit = n)
        lrn.tune = makeTuneWrapper(lrn, inner.rdesc, mlr::auc, par.set = lrn.ps, tune.ctrl)
      } else if (ctrl == "mbo") {
        lrn.ps = ps # We want to tune over a nicer param space (with trafos)
        tune.ctrl = makeTuneControlMBO(same.resampling.instance = TRUE, budget = n)
        lrn.tune = makeTuneWrapper(lrn, inner.rdesc, mlr::auc, par.set = lrn.ps, tune.ctrl)
      } else if (ctrl == "package-default") {
        if (getLearnerPackages(lrn) == "xgboost") {
          lrn = setHyperPars(lrn, "nrounds" = 100L)
        }
        lrn.tune = lrn
        lrn.tune$id = stri_paste(lrn.tune$id, ".tuned")
      }
      evalParsOpenML(task, lrn.tune)
    })
    
    res = do.call("rbind", res$perf)
    res$search.type = ctrl
    res$n = n
    saveRDS(res, filepath)
    saveRDS(res$bmr, stringBuilder("defaultLOOCV/bmrs", stri_paste(ctrl, n, it, "perf", sep = "_"), lrn$id))
  } else {
    res = readRDS(filepath)
  }
  return(res)
}

# Eval Hyperparams using OpenML datasets
# @param task OpenML Task Id | OMLTask
# @param lrn Learner
# @param pars Single param Configuration
evalParsOpenML = function(task, lrn, fun = runTaskMlr2) {
  
  run = runTaskMlr2(task, lrn, measures = list(mlr::auc, mlr::f1))
  
  # Extract performances
  perf = getBMRAggrPerformances(run$bmr, as.df = TRUE)
  perf$task.id = as.character(perf$task.id)
  perf$learner.id = as.character(perf$learner.id)
  
  return(list(perf = perf, bmr = run$bmr))
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
