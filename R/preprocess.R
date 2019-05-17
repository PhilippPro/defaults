


#' Return a list of specified surrogates
#' @param oml_task_ids  A vector of oml task ids for which we want to create surrogates. Defaults to all in get_oml_task_ids().
#' @param baselearners  A vector of baselearners for which we want to create surrogates. Defaults to all in get_baselearners().
#' @param measures      A vector of measures for which we want to create surrogates. Defaults to all in get_measures().
#' @param surrogate_lrn A mlr [Learner]. Defaults to "regr.fixcubist".
#' @export
make_surrogates_omlbot = function(oml_task_ids, baselearners, measures, surrogate_lrn,
  data_source = "data/input/oml_bot_data.RDS", save_path = "data/intermediate/") {

  if (missing(surrogate_lrn)) {
    # Obtain fixed cubist from the internet
    source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
    surrogate_lrn = mlr::makeLearner("regr.fixcubist", committees = 20, extrapolation = 0)
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

  surrs = foreach(measure_name = measures, .combine = "c") %:%
    foreach(oml_task_id = oml_task_ids, .combine = "c") %:%
      foreach(baselearner_name = baselearners, .combine = "c") %dopar% {
        s = surrogates::SurrogateFromRDS$new(
          oml_task_id = oml_task_id,
          baselearner_name = baselearner_name,
          data_source = data_source,
          measure_name = measure_name,
          surrogate_learner = surrogate_lrn,
          handle_prefix = save_path)

        # In case we have multiple baselearners, we set a different scale fun that
        # correctly aggregates accross all learners
        if (length(baselearners) > 1L) {
          range = ranges[[measure_name]][ranges[[measure_name]]$task_id == oml_task_id, c("min", "max")]
          s$scale_fun_pars = list(min = range$min, max = range$max)
        }
        s$train()
        return(s)
  }
  sc = surrogates::SurrogateCollection$new(surrs)
  return(sc)
}



#' Train all surrogates for a given surrogate collection.
#' @param sc [SurrogateCollection] if missing, calls make_surrogate_omlbot().
#' @param overwrite should existing surrogates be overwritten?
#' @export
train_surrogates_omlbot = function(sc, overwrite = FALSE) {
  if (overwrite) unlink("surrogates", recursive = TRUE, force = TRUE)
  require(doParallel)
  registerDoParallel(12L)
  sc = make_surrogates_omlbot()
  not_trained = seq_along(sc$surrogates)[sapply(sc$surrogates, function(x) !x$is_trained)]
  foreach(s = sc$surrogates[not_trained]) %dopar% {
    gc()
    s$train()
    NULL
  }
  invisible(NULL)
}


#' Substitute NA's with out of bounds data.
deleteNA = function(task.data) {
  for(i in 1:ncol(task.data)) {
    if(is.numeric(task.data[, i]))
      task.data[is.na(task.data[, i]), i] = -10 - 1
    if(is.factor(task.data[, i])) {
      task.data[, i] = addNA(task.data[, i])
      task.data[, i] = droplevels(task.data[, i])
    }
    if(is.logical(task.data[, i]))
      task.data[, i] = as.factor(task.data[, i])
  }
  task.data
}

# Convert wide to long
to_long = function(res, lrn) {
  requireNamespace("tidyr")
  res = data.frame(res)
  res$id = seq_len(32)
  long = tidyr::gather(res, "task", "auc", -id)
  long$learner = lrn
  long
}

#' Fix names of the predictions.
fix_prds_names = function(x) {
  colnames(x) = stringi::stri_sub(stringi::stri_extract_all_regex(colnames(x), ".*_"), to = -2)
  return(x)
}

get_ranges_multi_baselearners = function(data_source, baselearners, measures, oml_task_ids) {
  d = readRDS(data_source) %>% filter(!is.na(performance)) %>% ungroup()
  ranges = lapply(measures, function(measure) {
    d %>%
      filter(measure == measure) %>%
      filter(learner_id %in% paste0("mlr.classif.", baselearners)) %>%
      filter(task_id %in% oml_task_ids) %>%
      group_by(task_id) %>%
      summarise(min = min(performance), max = max(performance))
  })
  names(ranges) = measures
  return(ranges)
}

# scale_fun_multi_baselearners = function(x) {
#   self$scaling = "normalize_multi"
#   x = (x - self$scale_fun_pars$min) / (self$scale_fun_pars$max - self$scale_fun_pars$min)
#   self$rescale_fun = function(x) {
#     if (min(x) == max(x)) {
#       self$scale_fun_pars$min
#     } else {
#       (x * (self$scale_fun_pars$max - self$scale_fun_pars$min)) + self$scale_fun_pars$min
#     }
#   }
#   return(x)
# }
