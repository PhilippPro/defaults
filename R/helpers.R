getSimpleLearners = function(){
  # Simple learner param set
  simple.lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.glmnet", predict.type = "prob"),
    param.set = makeParamSet(
      makeNumericParam("alpha", lower = 0, upper = 1, default = 1),
      makeNumericVectorParam("lambda", len = 1L, lower = -10, upper = 10, default = 0 ,trafo = function(x) 2^x)))
  
  simple.lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.rpart", predict.type = "prob"), 
    param.set = makeParamSet(
      makeNumericParam("cp", lower = 0, upper = 1, default = 0.01),
      makeIntegerParam("maxdepth", lower = 1, upper = 30, default = 30),
      makeIntegerParam("minbucket", lower = 1, upper = 60, default = 1),
      makeIntegerParam("minsplit", lower = 1, upper = 60, default = 20)), 
    lrn.ps.sets = simple.lrn.par.set)
  
  return(simple.lrn.par.set)
}

getLearnerParSets = function(){
  simple.lrn.par.set = getSimpleLearners()
  
  # increase to a general param set
  lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.kknn", predict.type = "prob"), 
    param.set = makeParamSet(
      makeIntegerParam("k", lower = 1, upper = 30)),
    lrn.ps.sets = simple.lrn.par.set)
  
  lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.svm", predict.type = "prob"), 
    param.set = makeParamSet(
      makeDiscreteParam("kernel", values = c("linear", "polynomial", "radial")),
      makeNumericParam("cost", lower = -10, upper = 10, trafo = function(x) 2^x),
      makeNumericParam("gamma", lower = -10, upper = 10, trafo = function(x) 2^x, requires = quote(kernel == "radial")),
      makeIntegerParam("degree", lower = 2, upper = 5, requires = quote(kernel == "polynomial"))),
    lrn.ps.sets = lrn.par.set)
  
  lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.ranger", predict.type = "prob"), 
    param.set = makeParamSet(
      makeIntegerParam("num.trees", lower = 1, upper = 2000),
      makeLogicalParam("replace"),
      makeNumericParam("sample.fraction", lower = 0.1, upper = 1),
      makeNumericParam("mtry", lower = 0, upper = 1),
      makeLogicalParam(id = "respect.unordered.factors"),
      makeNumericParam("min.node.size", lower = 0, upper = 1)),
    lrn.ps.sets = lrn.par.set)
  
  lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.xgboost", predict.type = "prob"), 
    param.set = makeParamSet(
      makeIntegerParam("nrounds", lower = 1, upper = 5000), 
      makeNumericParam("eta", lower = -10, upper = 0, trafo = function(x) 2^x),
      makeNumericParam("subsample",lower = 0.1, upper = 1),
      makeDiscreteParam("booster", values = c("gbtree", "gblinear")),
      makeIntegerParam("max_depth", lower = 1, upper = 15, requires = quote(booster == "gbtree")),
      makeNumericParam("min_child_weight", lower = 0, upper = 7, requires = quote(booster == "gbtree"), trafo = function(x) 2^x),
      makeNumericParam("colsample_bytree", lower = 0, upper = 1, requires = quote(booster == "gbtree")),
      makeNumericParam("colsample_bylevel", lower = 0, upper = 1, requires = quote(booster == "gbtree")),
      makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x),
      makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x)),
    lrn.ps.sets = lrn.par.set)
  
  return(lrn.par.set)
}

makeLrnPsSets = function(learner, param.set, lrn.ps.sets = NULL, 
  id = paste0(learner$id, ".set"), overwrite = FALSE) {
  
  assertClass(learner, "Learner")
  assertClass(param.set, "ParamSet")
  par.match = names(param.set$pars) %in% names(learner$par.set$pars)
  if(all(par.match)){
    ls = list(learner = learner, param.set = param.set)
  } else {
    stop(paste("The following parameters in param.set are not included in learner:", 
      paste(names(param.set$pars[par.match == FALSE]), collapse = ", ")))
  }
  
  if(is.null(lrn.ps.sets)){
    lrn.ps.sets = list()
    lrn.ps.sets[[id]] = ls
    attr(lrn.ps.sets, "class") = "LrnPsSet"
  } else {
    assertClass(lrn.ps.sets, "LrnPsSet")
    
    if(id %in% names(lrn.ps.sets) & overwrite == FALSE){
      stop("tune.pair already contains id: \"", id, "\". Please specify a new id or set overwrite = TRUE.")
    } else {
      lrn.ps.sets[[id]] = ls
    }
  }
  
  return(lrn.ps.sets)
}

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

makePertastkParamSet = function() {
  list(
    ps.adaboost = makeParamSet(
      makeDiscreteParam(id = "algorithm", values = c("SAMME", "SAMME.R")),
      makeNumericParam(id = "learning_rate", lower = 0.01, upper = 2.0),
      makeIntegerParam(id = "max_depth", lower = 1, upper = 20),
      makeIntegerParam(id = "n_estimators", lower = 50, upper = 500),
      makeDiscreteParam(id = "strategy", values = c("median", "most_frequent", "mean"))
    ),
    ps.svm_svc = makeParamSet(
      makeDiscreteParam(id = "kernel", values = c("poly", "sigmoid", "rbf")),
      makeNumericParam(id = "coef0", lower = -1, upper = 1),
      makeNumericParam(id = "gamma", lower = 0.00003, upper = 7.999292),
      makeNumericParam(id = "tol", lower = 10^-5, upper = 10^-1),
      makeIntegerParam(id = "degree", lower = 1, upper = 5),
      makeLogicalParam("shrinking"),
      makeDiscreteParam(id = "strategy", values = c("median", "most_frequent", "mean"))
    ),
    ps.random_forest = makeParamSet(
      makeDiscreteParam(id = "criterion", values = c("entropy", "gini")),
      makeNumericParam(id = "max_features", lower = 0.1, upper = 0.9),
      makeIntegerParam(id = "min_samples_leaf", lower = 1, upper = 20),
      makeIntegerParam(id = "min_samples_split", lower = 1, upper = 20),
      makeDiscreteParam(id = "strategy", values = c("median", "most_frequent", "mean"))
    )
  )
}

convertParamType = function(x, param_type) {
  if(param_type %in% c("integer", "numeric", "numericvector")) 
    x = as.numeric(x)
  if(param_type %in% c("character", "logical", "factor", "discrete"))
    x = as.factor(x)
  return(x)
}

# Creates a seed from a task and a learner object and the current random state
# Not sure if this will be used.
getHashSeed = function(task, learner, init.seed = 111) {
  
  # Hash the task and the learner object
  hash = digest::digest(c(task, learner, init.seed), algo = "murmur32")
  seed.state = paste0(sum(abs(.Random.seed)), collapse = "")
  x = paste0(hash, seed.state)
  
  # And transform this hash to a seed
  tmp = setNames(c(0:9,0:25), c(0:9, letters))
  xsplit = tmp[strsplit(gsub("[^0-9a-z]", "", as.character(x)), '')[[1]]]
  seed = sum(rev(7^(seq(along=xsplit) - 1)) * xsplit)
  as.integer(seed %% (2^31-1))
}

makeProgressBarOpts = function() {
  pb <- txtProgressBar(min=1, max=8000, style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  list(progress=progress)
}



# # Create Train/Test Splits: 
# set.seed(199)
# train = sample(names(surrogates$surrogates), 19)
# test = setdiff(names(surrogates$surrogates), train)

# Create Train/Test Splits:
# @param switch Either return data.id or task.id
train_split = function(which = "data.id") {
  if (which == "data.id") {
    c("31", "1467", "1471", "335", "3", "334", "1462", "1176", "1494","1036", "1487", "1046", "1068", 
      "44", "1570", "1050", "1486", "37", "1480")
  } else {
    c(31, 145972, 146803, 146804, 146813, 9980, 145839, 9983, 14951, 
      3494, 146066, 3, 145953, 3493, 146065, 10093, 145834, 167125, 9957, 
      145862, NA, 9978, 145855, 3899, 3918, 43, 145979, 9889, 9914, 
      3903, 9977, 145854, 37, 145976, 9971, 145848)
  }
}

test_split = function(which = "data.id") {
  if (which == "data.id") {
    c("50", "151", "312", "333", "1038", "1043", "1049", "1063", "1067", "1120", "1461", "1464",
      "1479", "1485", "1489", "1504", "1510", "4134", "4534")
  } else {
    c(49L, 219L, 3485L, 3492L, 3891L, 3902L, 3913L, 3917L, 3954L, 
      9910L, 9946L, 9952L, 9967L, 9970L, 9976L, 10101L, 14952L, 14965L, 
      14966L, 34537L, 145677L, 145804L, 145833L, 145836L, 145847L, 
      145853L, 145857L, 145872L, 145878L, 146012L, 146064L, 168295L)
  }
}

train_split_pertask = function() {
  c(96L, 31L, 9L, 86L, 76L, 90L, 84L, 62L, 64L, 98L, 78L, 68L, 
    88L, 61L, 10L, 97L, 74L, 93L, 52L, 56L, 2L, 92L, 32L, 71L, 17L, 
    24L, 65L, 39L, 41L, 49L, 83L, 7L, 20L, 43L, 87L, 21L, 42L, 70L, 
    3L, 26L, 57L, 69L, 14L, 33L, 89L, 59L, 82L, 4L, 75L, 34L)
}

test_split_pertask = function() {
  c(1L, 5L, 6L, 8L, 11L, 12L, 13L, 15L, 16L, 18L, 19L, 22L, 23L, 
    25L, 27L, 28L, 29L, 30L, 35L, 36L, 37L, 38L, 40L, 44L, 45L, 46L, 
    47L, 48L, 50L, 51L, 53L, 54L, 55L, 58L, 60L, 63L, 66L, 67L, 72L, 
    73L, 77L, 79L, 80L, 81L, 85L, 91L, 94L, 95L, 99L, 100L)
}