#' Return a list of specified surrogates.
#' If surrogates do not exist, trains and saves them, else loads them from disk.
#' @param oml_task_ids  A vector of oml task ids for which we want to create surrogates. Defaults to all in get_oml_task_ids().
#' @param baselearners  A vector of baselearners for which we want to create surrogates. Defaults to all in get_baselearners().
#' @param measures      A vector of measures for which we want to create surrogates. Defaults to all in get_measures().
#' @param surrogate_lrn A mlr [Learner]. Defaults to "regr.fixcubist".
#' @export
make_surrogates_omlbot = function(
  oml_task_ids = get_oml_task_ids(),
  baselearners = get_baselearners(),
  measures = get_measures(),
  surrogate_lrn,
  data_source = "data/input/oml_bot_data.RDS", save_path = "data/intermediate/",
  scaler = surrogates::Scaler$new()) {

  if (missing(surrogate_lrn)) {
    # Obtain fixed cubist from the internet
    # source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
    # surrogate_lrn = mlr::makeLearner("regr.fixcubist", committees = 20, extrapolation = 0)
    surrogate_lrn = mlr::makeLearner("regr.ranger", num.trees = 40L)
  }

  # Make sure we aggregate correctly in case we have multiple baselearners,
  # use a global list of min/max performances for each task.
  ranges = readRDS("data/input/oml_mlr_ranges.RDS")

  surrs = foreach(measure_name = measures, .combine = "c") %:%
    foreach(oml_task_id = oml_task_ids, .combine = "c") %:%
      foreach(base_learner = baselearners, .combine = "c") %dopar% {
        if (measure_name != "runtime")
          scaler_ms = scaler$clone()$set_values(ranges[[measure_name]][ranges[[measure_name]]$task_id == oml_task_id, c("min", "max"),])
        surrogates::Surrogate$new(
          oml_task_id = oml_task_id,
          base_learner = base_learner,
          data_source = data_source,
          eval_measure = measure_name,
          surrogate_learner = surrogate_lrn,
          load_fun = load_from_rds,
          param_set = get_param_set(paste0("classif.", base_learner)),
          save_path = save_path,
          scaler = scaler_ms)$train()
  }
  sc = surrogates::SurrogateCollection$new(surrs)
  return(sc)
}


#' Return a list of specified surrogates.
#' If surrogates do not exist, trains and saves them, else loads them from disk.
#' @param oml_task_ids  A vector of oml task ids for which we want to create surrogates. Defaults to all in get_oml_task_ids().
#' @param base_learners  A vector of baselearners for which we want to create surrogates. Defaults to all in get_baselearners().
#' @param measures      A vector of measures for which we want to create surrogates. Defaults to all in get_measures().
#' @param surrogate_lrn A mlr [Learner]. Defaults to "regr.fixcubist".
#' @export
make_surrogates_sklearn = function(oml_task_ids, base_learners, surrogate_lrn,
  data_source = "data/input/sklearn_oml100", save_path = "data/intermediate/") {

  if (missing(surrogate_lrn)) {
    # Obtain fixed cubist from the internet
    # source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
    # surrogate_lrn = mlr::makeLearner("regr.fixcubist", committees = 20, extrapolation = 0)
    surrogate_lrn = mlr::makeLearner("regr.ranger", num.trees = 40L)
  }
  assert_true(all(base_learners %in% c("adaboost", "libsvm_svc", "random_forest")))

 # Make sure we aggregate correctly in case we have multiple baselearners:
  if (length(base_learners) > 1L) {
    stop("Not implemented")
    # ranges = get_ranges_multi_baselearners(data_source, base_learners, measures, oml_task_ids)
  }

  surrs = foreach(oml_task_id = oml_task_ids, .combine = "c") %:%
      foreach(base_learner = base_learners, .combine = "c") %do% {
        surrogates::Surrogate$new(
          oml_task_id = oml_task_id,
          base_learner = base_learner,
          data_source = paste0(data_source, "/", base_learner, ".arff"),
          eval_measure = "y",
          surrogate_learner = surrogate_lrn,
          load_fun = load_from_arff,
          param_set = get_param_set(base_learner),
          save_path = "data/intermediate/",
          scaler = Scaler$new())$train()
  }
  sc = surrogates::SurrogateCollection$new(surrs)
  return(sc)
}
