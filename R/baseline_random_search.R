#' Random search on surrogates
baseline_random_search = function(sc, repls = 1, seed = 141516) {
  requireNamespace("dplyr")
  requireNamespace("tidyr")
  set.seed(seed)
  assert_class(sc, "SurrogateCollection")
  assert_count(repls)
  ds = DefaultSearch$new(sc, 1L, holdout_task_id = NULL)
  pts = c(1, 2, 4, 8, 16, 32, 64)

  res_rs = foreach(n_points = pts, .combine = "rbind") %dopar% {
        res = replicate(repls, {
          pts = ds$generate_random_points(n_points, unique(ds$sc$base_learners))
          prds = ds$sc$predict(pts)
          prds = lapply(prds, function(x) {colnames(x) = NULL; return(x)})
          prds = do.call("rbind", prds)
          apply(prds, 2, max)
        })
        apply(res, 1, mean)
  }
  colnames(res_rs) = unique(as.vector(sc$oml_task_ids))
  df = data.frame(res_rs, iter = pts)

  # Separate out into 1, 2 and 4 times random search
  out = do.call("rbind", apply(outer(c(1, 2, 4), c(1, 2, 4, 8, 16)), 1,
    function(x) {
      df = df[df$iter %in% x, ]
      df$iter = c(1, 2, 4, 8, 16)
      df$method = paste0("rs_x", x[1])
      return(df)
  }))

  rownames(out) = NULL
  out = data.frame(out) %>% gather("dataset", "auc", -iter, -method)
  out$dataset = gsub("X", "", out$dataset)
  return(out)
}
