#' Convert wide to long
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

# Convert Param Types for use with mlr
convertParamType = function(x, param_type) {
  if(param_type %in% c("integer", "numeric", "numericvector"))
    x = as.numeric(x)
  if(param_type %in% c("character", "logical", "factor", "discrete"))
    x = as.factor(x)
  return(x)
}

# Impute all NA's using an out-of-paramset value (lower - 1)
# This works well for RF based surrogates, as tree's can split this away.
out_of_parset_imputer = function(data, ps) {
  nacols = colnames(data)[sapply(data, anyNA)]
  # Get imputation values
  out_vals = sapply(ps$pars[nacols], function(x) {
    if (x$type %in% c("integer", "numeric"))
      return(x$lower - 1)
    else if (x$type %in% c("factor", "logical"))
      return("_NA_")
  })
  if (!is.null(nacols)) {
    data = data.frame(data)
    data[, nacols] = sapply(nacols, function(x) {
      # Convert logicals to factors
      if (is.logical(data[[x]])) data[[x]] = as.character(data[[x]])
      # Replace NA's
      data[is.na(data[[x]]), x] = out_vals[x]
      # character -> factor
      if (is.character(data[[x]])) data[[x]] = as.factor(data[[x]])
      return(data[[x]])
    })
  }
  return(data)
}

#' Get holdout performance from unscaled surrogates
get_holdout_perf = function(sc, defaults.params, oml_task_id) {
  assert_class(sc, "SurrogateCollection")
  df = data.frame(unlist(sc$predict(defaults.params, oml_task_id)))
  colnames(df) = paste0(oml_task_id, "_auc")
  return(df)
}

#' Get performance ranges across multiple base learners
get_ranges_multi_baselearners = function(data_source, baselearners, measures, oml_task_ids) {
  requireNamespace("dplyr")
  d = readRDS(data_source)
  d = d %>% dplyr::filter(!is.na(performance)) %>% dplyr::ungroup()
  ranges = d %>%
      dplyr::filter(learner_id %in% paste0("mlr.classif.", baselearners)) %>%
      dplyr::filter(task_id %in% oml_task_ids) %>%
      dplyr::group_by(task_id, measure) %>%
      dplyr::summarise(min = min(performance), max = max(performance))
  return(ranges)
}

#' @title Reading data
#'
#' @description
#' Loading data from RDS
#'
#' @param self :: `R6()`
# export
load_from_rds = function(self) {
  requireNamespace("data.table")
  # Load and rename column
  data = data.table::as.data.table(readRDS(self$data_source))
  colnames(data)[colnames(data) == self$eval_measure] = "performance"
  # Scale performance column
  data$performance[data$task_id == self$oml_task_id] = self$scaler$scale(data, oml_task_id = self$oml_task_id, runtime = "runtime")
  # Subset columns, only relevant data
  self$param_names = intersect(getParamIds(self$param_set), colnames(data))
  data = data[(data$task_id == self$oml_task_id) & (data$learner_id == paste0("mlr.classif.", self$base_learner)),  c("performance", self$param_names), with = FALSE]
  # Impute NA's with out-of-paramset values
  data = out_of_parset_imputer(data, self$param_set)
  return(data)
}

#' @title Reading data
#'
#' @description
#' Loading data from farff
#'
#' @param self :: `R6()`
# export
load_from_arff = function(self) {
  # Load and rename column
  data = data.table::as.data.table(farff::readARFF(self$data_source))
  colnames(data)[colnames(data) == self$eval_measure] = "performance"
  # Scale performance column
  data$performance[data$task_id == self$oml_task_id] = self$scaler$scale(data, oml_task_id = self$oml_task_id)
  # Subset columns, only relevant data
  self$param_names = intersect(getParamIds(self$param_set), colnames(data))
  data = data[data$task_id == self$oml_task_id, c("performance", self$param_names), with = FALSE]
  # Impute NA's with out-of-paramset values
  data = out_of_parset_imputer(data, self$param_set)
  return(data)
}
