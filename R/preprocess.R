#' Return a list of specified surrogates
#' @param oml_task_ids  A vector of oml task ids for which we want to create surrogates. Defaults to all in get_oml_task_ids().
#' @param baselearners  A vector of baselearners for which we want to create surrogates. Defaults to all in get_baselearners().
#' @param measures      A vector of measures for which we want to create surrogates. Defaults to all in get_measures().
#' @param surrogate_lrn A mlr [Learner]. Defaults to "regr.fixcubist".
#' @export
make_surrogates_omlbot = function(oml_task_ids, baselearners, measures, surrogate_lrn,
  data_source = "data/input/oml_bot_data.RDS", save_path = "data/intermediate/",
  timecrit = FALSE) {

  if (missing(surrogate_lrn)) {
    # Obtain fixed cubist from the internet
    # source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
    # surrogate_lrn = mlr::makeLearner("regr.fixcubist", committees = 20, extrapolation = 0)
    surrogate_lrn = mlr::makeLearner("regr.ranger", num.trees = 40L)
  }
  if (missing(oml_task_ids))
    oml_task_ids = get_oml_task_ids()
  if (missing(baselearners))
    baselearners = get_baselearners()
  if (missing(measures))
    measures = get_measures()

 # Make sure we aggregate correctly in case we have multiple baselearners:
  if (length(baselearners) > 1L) {
    ranges = get_ranges_multi_baselearners(data_source, baselearners, measures, oml_task_ids)
  }

  assert_flag(timecrit)
  if (!timecrit) scaler = Scaler$new()
  else scaler = ScalerTimeCrit$new()

  surrs = foreach(measure_name = measures, .combine = "c") %:%
    foreach(oml_task_id = oml_task_ids, .combine = "c") %:%
      foreach(base_learner = baselearners, .combine = "c") %dopar% {
        surrogates::Surrogate$new(
          oml_task_id = oml_task_id,
          base_learner = base_learner,
          data_source = data_source,
          eval_measure = measure_name,
          surrogate_learner = surrogate_lrn,
          load_fun = load_from_rds,
          param_set = get_param_set(paste0("classif.", base_learner)),
          save_path = "data/intermediate/",
          scaler = scaler)$train()

  }
  sc = surrogates::SurrogateCollection$new(surrs)
  return(sc)
}
