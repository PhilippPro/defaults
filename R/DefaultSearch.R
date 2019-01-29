#' DefaultSearch [R6Class]
#'
#' Greedily searches for defaults.
#' @param sc = SurrogateCollection
#' @param n_defaults Number of defaults
#' @param holdout_task_id Which task should be held out?
#' @param fail_handle Path for fail()
DefaultSearch = R6Class("DefaultSearch",

  public = list(
    # Surrogates
    sc = NULL,
    fail_handle = NULL,
    ctrl = makeFocusSearchControl(maxit = 1, restarts = 1, points = 10^5),
    show.info = FALSE,
    holdout_task_id = NULL,

    n_defaults = NULL,
    defaults.perf = NULL,
    defaults.params = list(),
    prd_aggregator = NULL,
    maximize = TRUE,
    y = NULL,
    best.y = - Inf,
    aggfun = NULL,

    initialize = function(sc, n_defaults = 10L, holdout_task_id, aggfun = "mean", fail_handle) {
      self$sc = assert_class(sc, "SurrogateCollection")$clone()
      self$n_defaults = assert_int(n_defaults)
      self$holdout_task_id = assert_int(holdout_task_id)
      self$sc$set_holdout_task(self$holdout_task_id)
      self$aggfun = assert_choice(aggfun, choices = c("mean", "median"))
      self$fail_handle = if(missing(fail_handle)) fail::fail(self$fail_path()) else assert_path_for_output(fail_handle)
    },

    # Search defaults as specified.
    search_defaults = function(overwrite = FALSE) {
      # Search for optimal points given previous defaults
      assert_class(self$ctrl, "FocusSearchControl")

      # Load defaults if saved.
      if (overwrite) self$clear()
      if (length(self$defaults.params) == 0L)
        self$acquire_defaults()
      # Compute n_defaults  default parameters iteratively
      # Defaults from earlier iterations influence later ones.
      for (j in seq_len(self$n_defaults)) {
        if (length(self$defaults.params) >= self$n_defaults) {
          messagef("Already found %s defaults!", self$n_defaults)
          break
        }

        for (restart.iter in seq_len(self$ctrl$restarts)) {
          if (self$show.info)
            catf("Multistart %i of %i \n", restart.iter, control$restarts)
          for (local.iter in seq_len(self$ctrl$maxit)) {
            z = self$do_random_search()
            if (z$y > self$best.y) {
              if (self$show.info)
                catf("New best y: %f found for x: %s \n", z$y, paste0(z$opt.params, collapse = ", "))
              self$best.y = z$y
              self$y = c(self$y, z$y)
            }
          }
        }
        # Save found defaults and their performances
        self$defaults.perf = rbind(self$defaults.perf, z$opt.prds)
        self$defaults.params = c(self$defaults.params, z$opt.params)
      }
      if (self$show.info) print(defaults.params)
    },

    # Do a random search on the surrogates
    do_random_search = function() {
      pts = self$generate_random_points(self$ctrl$points, unique(self$sc$baselearners))
      prds = self$sc$predict(pts)
      prds = lapply(prds, self$fix_prds_names)
      # Compute the aggregation
      prds.agg = self$objfun(prds)
      assert_true(all(names(prds.agg) == names(prds)))

      # Extract best base-learner and save the parameters
      best.bl = names(which.max(sapply(prds.agg, max)))
      best.idx = which.max(prds.agg[[best.bl]])
      list(
        y = prds.agg[[best.bl]][best.idx],
        opt.prds = prds[[best.bl]][best.idx, , drop = FALSE],
        opt.params = setNames(list(pts[[best.bl]][best.idx, , drop = FALSE]), best.bl)
      )
    },

    # Draw random configurations for a set of baselearners
    generate_random_points = function(npoints, baselearners) {
      bl_props = round(npoints / length(baselearners))
      nd = lapply(baselearners, function(x) {
        ps = get_param_set(x)
        # Generate random points for a given baselearner.
        newdesign = generateRandomDesign(bl_props, ps, trafo = TRUE)
        newdesign = deleteNA(newdesign)
        newdesign = convertDataFrameCols(newdesign, ints.as.num = TRUE,  logicals.as.factor = TRUE)
        # Make sure we have enough points in newdesign and not too many
        while (nrow(newdesign) < bl_props) {
          n2 = generateRandomDesign(bl_props, ps, trafo = TRUE)
          n2 = deleteNA(n2)
          n2 = convertDataFrameCols(n2, ints.as.num = TRUE,  logicals.as.factor = TRUE)
          newdesign = rbind(newdesign, n2)
        }
        newdesign = newdesign[seq_len(bl_props), ]
      })
      # Always return a named list (names = baselearner names)
      if (length(baselearners) == 1L)
        nd = as.list(nd)
      names(nd) = baselearners
      return(nd)
    },

    # Objective function for aggregation
    objfun = function(prds) {
      fun = switch(self$aggfun, "mean" = mean, "median" = median)
      lapply(prds, function(x) {
        # Invert perfs if measure is not maximized
        if (!self$maximize) x = -x
        # Parallel minimum with current best
        pmax = apply(x, 1, function(x) pmax(x, self$best.perfs))
        # Compute mean
        pmed = apply(pmax, 2, fun)
      })
    },

    evaluate_defaults_holdout = function() {
      out = self$sc$evaluate_holdout_task(self$defaults.params)
      res = do.call("rbind", lapply(out, self$fix_prds_names))
      self$fail_handle$put(keys = "holdout.perfs", res)
      return(res)
    },

    get_holdout_performance = function(overwrite = FALSE) {
      if ("holdout.perfs" %in% self$fail_handle$ls() && !overwrite) {
        self$fail_handle$get("holdout.perfs")
      } else {
        self$evaluate_defaults_holdout()
      }
    },

    fix_prds_names = function(x) {
      colnames(x) = stri_sub(stri_extract_all_regex(colnames(x), ".*_"), to = -2)
      return(x)
    },

    fail_path = function() {
      lrns = paste0(unique(self$sc$baselearners), collapse = "_")
      meas = paste0(unique(self$sc$measures), collapse = "_")
      slrn = paste0(unique(self$sc$surrogate_learner), collapse = "_")
      scale = paste0(unique(self$sc$scaling), collapse = "_")
      paste("defaults", lrns,
        paste0(slrn, "_surrogate"),
        paste(meas, scale, self$aggfun, sep = "_"),
        self$sc$holdout_task_id, sep = "/")
    },

    acquire_defaults = function() {
      if ("params" %in% self$fail_handle$ls()) {
        self$defaults.params = self$fail_handle$get("params")
        self$defaults.perf = self$fail_handle$get("perfs")
      }
    },
    save_to_disk = function() {
      self$fail_handle$put(keys = "params", self$defaults.params)
      self$fail_handle$put(keys = "perfs", self$defaults.perfs)
    },
    clear = function() {
      self$fail_handle$remove(c("params", "perfs"))
    }
  ),

  active = list(
    # Get a vector of optimal performances on each dataset from saved performances
    best.perfs = function() {
      if (is.null(self$defaults.perf)) {
        ifelse(self$maximize, -Inf, Inf)
      } else {
        apply(self$defaults.perf, 2, max)
      }
    }
  )
)
