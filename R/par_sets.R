#' Get the param set for a given learner.
#'
#' Paramsets define the space in which we want to search for defaults.
#' @param lrn.cl [`character`] learner.id.
#' @export
get_param_set = function(lrn.cl) {

  if (stringi::stri_sub(lrn.cl, 1, 8) != "classif.")
    lrn.cl = stringi::stri_paste("classif.", lrn.cl)

  if (!checkmate::check_choice(lrn.cl, c("classif.glmnet", "classif.rpart",
    "classif.kknn", "classif.ranger", "classif.xgboost", "classif.svm")))
    stop(sprintf("No parameter set for %s available, please supply a param set!", lrn))

  switch(lrn.cl,
  "classif.glmnet" = makeParamSet(
      makeNumericParam("alpha", lower = 0, upper = 1, default = 1),
      makeNumericVectorParam("lambda", len = 1L, lower = -10, upper = 10, default = 0, trafo = function(x) 2^x)),

  "classif.rpart" = makeParamSet(
      makeNumericParam("cp", lower = 0, upper = 1, default = 0.01),
      makeIntegerParam("maxdepth", lower = 1, upper = 30, default = 30),
      makeIntegerParam("minbucket", lower = 1, upper = 60, default = 1),
      makeIntegerParam("minsplit", lower = 1, upper = 60, default = 20)),

  "classif.kknn" = makeParamSet(
      makeIntegerParam("k", lower = 1, upper = 30)),

  "classif.svm" = makeParamSet(
      makeDiscreteParam("kernel", values = c("linear", "polynomial", "radial")),
      makeNumericParam("cost", lower = -10, upper = 10, trafo = function(x) 2^x),
      makeNumericParam("gamma", lower = -10, upper = 10, trafo = function(x) 2^x, requires = quote(kernel == "radial")),
      makeIntegerParam("degree", lower = 2, upper = 5, requires = quote(kernel == "polynomial"))),

  "classif.ranger" = makeParamSet(
      makeIntegerParam("num.trees", lower = 1, upper = 2000),
      makeLogicalParam("replace"),
      makeNumericParam("sample.fraction", lower = 0.1, upper = 1),
      makeNumericParam("mtry", lower = 0, upper = 1),
      makeLogicalParam(id = "respect.unordered.factors"),
      makeNumericParam("min.node.size", lower = 0, upper = 1)),

  "classif.xgboost" = makeParamSet(
      makeIntegerParam("nrounds", lower = 1, upper = 5000),
      makeDiscreteParam("booster", values = c("gbtree", "gblinear")),
      makeNumericParam("eta", lower = -10, upper = 0, trafo = function(x) 2^x),
      makeNumericParam("subsample",lower = 0.1, upper = 1, requires = quote(booster == "gbtree")),
      makeIntegerParam("max_depth", lower = 1, upper = 15, requires = quote(booster == "gbtree")),
      makeNumericParam("min_child_weight", lower = 0, upper = 7, requires = quote(booster == "gbtree"),
        trafo = function(x) 2^x),
      makeNumericParam("colsample_bytree", lower = 0, upper = 1, requires = quote(booster == "gbtree")),
      makeNumericParam("colsample_bylevel", lower = 0, upper = 1, requires = quote(booster == "gbtree")),
      makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x,
        requires = quote(booster == "gblinear")),
      makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x,
        requires = quote(booster == "gblinear")))
  )
}

assert_param_set = function(ps) {
  if (ParamHelpers::checkParamSet(ps))
    return(ps)
}
