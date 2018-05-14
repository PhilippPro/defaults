predictGridFromSurrogates = function(surrogates, learner.name) {
  param.set = surrogates$param.set
  grd.points = generateGridDesign(param.set, resolution = 32L, trafo = TRUE)
  grd.points = deleteNA(grd.points)
  
  parallelMap::parallelStartMulticore(parallel::detectCores() / 8)
  res.list = parallelMap::parallelMap(surrogates$surrogates,
                                      fun = function(x) {predict(x, newdata = grd.points)$data$response},
                                      simplify = TRUE)
  colnames(res.list) = paste0("neg.auc_ds_", seq_len(ncol(res.list)))
  df = cbind(grd.points, res.list)
  feather::write_feather(df, stri_paste("gridpredictions/", learner.name, ".feather"))
}