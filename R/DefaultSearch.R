#' DefaultSearch [R6Class]
#'
#' Greedily searches for defaults.
#' @param sc [`surrogates::SurrogateCollection`]
#' @param n_defaults Number of defaults to search for.
#' @param holdout_task_id Which task should be held out? Does not hold out any task if `NULL`.
#' @param aggfun [`character(1)`] Aggregation function. Either "mean" or "median".
#' @param fail_handle [`character(1)`] Path for fail(). Creates a fail_handle if NULL.
#' @param prefix [`character(1)`] Prefix for the fail_path (Optional).
#'   Allows to train different versions for a given combination.
#' @export
DefaultSearch = R6::R6Class("DefaultSearch",

  public = list(
    # Surrogates
    sc = NULL,
    fail_handle = NULL,
    prefix = NULL,
    holdout_task_id = NULL,
    n_defaults = NULL,
    show.info = FALSE,
    ctrl = focussearch::makeFocusSearchControl(maxit = 1, restarts = 1, points = 10^5),
    ps = NULL,

    defaults.perf = NULL,
    defaults.params = list(),
    prd_aggregator = NULL,
    maximize = TRUE,

    y = NULL,
    best.y = - Inf,
    aggfun = NULL,

    initialize = function(sc, n_defaults = 10L, holdout_task_id, aggfun = "mean", fail_handle = NULL, save_folder = "data/intermediate", learner_prefix = NULL) {
      self$sc = assert_class(sc, "SurrogateCollection")$clone()
      self$n_defaults = assert_int(n_defaults)

      self$holdout_task_id = assert_int(holdout_task_id, null.ok = TRUE)
      if (!is.null(self$holdout_task_id))
        self$sc$set_holdout_task(self$holdout_task_id)

      self$aggfun = assert_choice(aggfun, choices = c("mean", "median"))
      self$fail_handle = if(is.null(fail_handle)) fail::fail(self$fail_path(save_folder, learner_prefix)) else assert_path_for_output(fail_handle)
      self$ps = setNames(lapply(unique(self$sc$baselearners), get_param_set), unique(self$sc$baselearners))
    },
    print = function(...) {
      cat("Default Search for %s defaults", self$n_defaults)
      if(!is.null(self$defaults.perf)) cat("Performances:\n"); print(self$defaults.perf)
      if(!is.null(self$defaults.params))  cat("Defaults:\n"); print(self$defaults.params)
    },
    defaults_to_csv = function() {
      stop("Not implemented yet!")
      if(is.null(self$defaults.params)) {
        stop("Defaults not yet available! run .$search_defaults")
      } else {
        path = paste0(save_folder, "/output")
        write.csv(self$defaults.params)
      }
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
        } else {
          if (self$show.info)
            catf("Searching for default %i/%i:", j, self$n_defaults)
        }

        for (restart.iter in seq_len(self$ctrl$restarts)) {
          if (self$show.info & self$ctrl$restarts > 1)
            catf("Multistart %i of %i \n", restart.iter, self$ctrl$restarts)
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
      if (self$show.info) print(self$defaults.params)
    },

    # Do a random search on the surrogates
    do_random_search = function() {
      pts = self$generate_random_points(self$ctrl$points, unique(self$sc$baselearners))
      prds = self$sc$predict(pts)
      prds = lapply(prds, fix_prds_names)
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

        # Generate random points for a given baselearner.
        newdesign = generateRandomDesign(bl_props, self$ps[[x]], trafo = TRUE)
        newdesign = deleteNA(newdesign)
        newdesign = convertDataFrameCols(newdesign, ints.as.num = TRUE,  logicals.as.factor = TRUE)
        # Make sure we have enough points in newdesign and not too many
        while (nrow(newdesign) < bl_props) {
          n2 = generateRandomDesign(bl_props, self$ps[[x]], trafo = TRUE)
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
      if (is.null(self$holdout_task_id))
        stop("No holdout performance available, trained on all datasets!")
      out = self$sc$evaluate_holdout_task(self$defaults.params)
      res = do.call("rbind", lapply(out, fix_prds_names))
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

    fail_path = function(folder, prefix) {
      assert_character(folder, null.ok = TRUE)
      assert_character(prefix, null.ok = TRUE)
      lrns = paste0(unique(self$sc$baselearners), collapse = "_")
      meas = paste0(unique(self$sc$measures), collapse = "_")
      slrn = paste0(unique(self$sc$surrogate_learner), collapse = "_")
      scale = paste0(unique(self$sc$scaling), collapse = "_")
      if (!is.null(prefix)) prefix = paste0(prefix, "_")

      if(is.null(self$holdout_task_id)) holdout_task_id = "full"
      else holdout_task_id = self$sc$holdout_task_id

      path = paste(folder, "defaults",
        paste0(prefix, lrns),
        paste0(slrn, "_surrogate"),
        paste(meas, scale, self$aggfun, sep = "_"),
        holdout_task_id, sep = "/")
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
