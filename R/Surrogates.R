#'@title SurrogateWithRuntimeFromRDS
#'
#' @name SurrogateWithRuntimeFromRDS
#' @format [R6Class] object
#' @description
#'
#' Inherits from [Surrogate].
#'
#' Allows for the construction of surrogates from a given meta-data dataset
#' of hyperparameters and a given performance.
#' Additonally scales the performance with runtime by computing
#' perf / (log(runtime + self$offset, base = self$base4)^self$power
SurrogateWithRuntimeFromRDS = R6::R6Class("SurrogateWithRuntimeFromRDS",
  inherit = Surrogate,
  public = list(
    data_source = NULL,
    scaling = "normalize_time",
    scale_fun_pars = NULL,

    # Parameters that scale the performance with runtime
    offset = 10,
    base = 10,
    power = 0.15,

    initialize = function(data_source, ...) {
      super$initialize(...)
      self$data_source = assert_string(data_source)
    },
    scale_with_runtime = function(perf, runtime) {
      perf / log(runtime + self$offset, base = self$base)^self$power
    },
    scale_fun = function(x) {
      x = x[!is.na(x)]
      if (is.null(self$scale_fun_pars)) {
        # FIXME: Implement different options for this or expose BBmisc::normalize args
        self$scale_fun_pars = list(min = min(x), max = max(x))
        x = BBmisc::normalize(x, "range")
      } else {
        # In case we want to set the normalization from outside
        x = BBmisc::normalize(x, "range", range = c(self$scale_fun_pars$min, self$scale_fun_pars$max))
      }
      return(x)
    },
    rescale_fun = function(x) {
      if (min(x) == max(x)) {
        self$scale_fun_pars$min
      } else {
        (x * (self$scale_fun_pars$max - self$scale_fun_pars$min)) + self$scale_fun_pars$min
      }
    }
  ),
  private = list(
    get_data_from_source = function() {
      require(dplyr)
      d = readRDS(self$data_source)
      d %>%
        ungroup() %>%
        filter(measure == self$measure_name) %>%
        filter(task_id == self$oml_task_id) %>%
        filter(learner_id == paste0("mlr.classif.", self$baselearner_name)) %>%
        mutate(performance = self$scale_fun(performance)) %>%                   # Scale to [0,1]
        mutate(performance = self$scale_with_runtime(performance, runtime)) %>% # Scale by runtime
        select(one_of(c("performance", self$param_names))) %>%
        as.data.frame()
    }
  )
)
