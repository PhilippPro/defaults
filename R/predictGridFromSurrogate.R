predictGridFromSurrogates = function(surrogates, learner.name) {
  param.set = surrogates$param.set
  grd.points = generateGridDesign(param.set, resolution = 32L, trafo = TRUE)
  grd.points = deleteNA(grd.points)
  
  res = parallelMap::parallelMap(surrogates$surrogates,
                                 fun = function(x) {predict(x, newdata = grd.points)$data$response},
                                 simplify = TRUE)

  # Split into train/test
  colnames(res) = names(surrogates$surrogates)
  df_train = cbind(grd.points, res[, colnames(res) %in% train_split()])
  df_test  = cbind(grd.points, res[, colnames(res) %in% test_split()])
  # And write to feather
  feather::write_feather(df_train, stri_paste("gridpredictions/train_", gsub("mlr.classif.", "", learner.name), ".feather"))
  feather::write_feather(df_test, stri_paste("gridpredictions/test_", gsub("mlr.classif.", "", learner.name), ".feather"))
  messagef("Writing train/test feather's of size %d x %d!", nrow(df_train), ncol(df_train))
}