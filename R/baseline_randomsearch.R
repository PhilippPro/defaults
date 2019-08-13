#' Random search on surrogates
baseline_random_search = function(sc, n_defaults) {
  assert_class(sc, "SurrogateCollection")
  assert_count(n_defaults)
  ds = DefaultSearch$new(sc, n_defaults, holdout_task_id = NULL)

  pts = c(1, 2, 4, 8, 16, 32, 64)

  res_rs = foreach(n_points = pts, .combine = "rbind") %do% {
        res = replicate(10, { # Replications to reduce stochasticity
          ds$ctrl$points = n_points
          ds$do_random_search()$opt.prds
        })
        apply(res, 2, mean)
  }
  df = data.frame(res_rs, n_points = pts)

  # Separate out into 1, 2 and 4 times random search
  out = do.call("rbind", apply(outer(c(1, 2, 4), c(1, 2, 4, 8, 16)), 1, 
    function(x) {
      df = df[df$n_points %in% x, ]
      df$factor = paste0("x", x[1])
      return(df)
  }))
  return(out) 
}
