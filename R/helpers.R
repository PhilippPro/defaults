# ----------------------------------------------------------------------------------------
# Misc Helpers
# Critical Differences
create_cdplot = function(df, learner, aggr.meas = "auc.test.mean") {

  dft = df %>%
    mutate(search_n = paste0(search.type, "_", n)) %>%
    filter(learner.short == learner) %>%
    filter(n %in% c(2, 4, 8, 16, 32)) %>%
    mutate(search_n = paste0(search.type, "_", n)) %>%
    filter(search_n %in% c("random_8", "random_16", "random_32", "mbo_32", "defaults_8", "defaults_4")) %>%
    rename(aggrMeasure = aggr.meas)

  mask = dft %>%
   group_by(task.id) %>% summarize(n = n()) %>%
   arrange(desc(n)) %>%
   filter(n == max(n))

  dft = dft %>% filter(task.id %in% mask$task.id)

  messagef("Creating cd-plot for %s on %i datasets", learner, length(unique(dft$task.id)))

  frm = as.formula(stri_paste("aggrMeasure ~  search_n| task.id", sep = ""))
  friedman.test(frm, data = dft)
  ntst = PMCMRplus::frdAllPairsNemenyiTest(dft[["aggrMeasure"]], dft$search_n, dft$task.id)

  n.learners = length((unique(dft$search_n)))
  n.tasks = length(unique(dft$task.id))
  q.nemenyi = qtukey(1 - 0.05, n.learners, 1e+06) / sqrt(2L)
  cd.nemenyi = q.nemenyi * sqrt(n.learners * (n.learners + 1L) / (6L * n.tasks))
  q.bd = qtukey(1L - (0.05 / (n.learners - 1L)), 2L, 1e+06) / sqrt(2L)
  cd.bd = q.bd * sqrt(n.learners * (n.learners + 1L) / (6L * n.tasks))


  dd = dft %>%
   group_by(task.id) %>%
   mutate(rnk = dense_rank(desc(aggrMeasure))) %>%
   select(rnk, task.id, search_n, aggrMeasure) %>%
   arrange(task.id) %>%
   group_by(search_n) %>%
   summarize(mean_rank = mean(rnk)) %>%
   mutate(right = mean_rank >= median(mean_rank)) %>%
   mutate(yend = min_rank(mean_rank)) %>%
   mutate(yend = ifelse(yend <= median(yend), yend, max(yend) - yend + 1)) %>%
   mutate(yend = yend * 0.5) %>%
   arrange(mean_rank) %>%
   mutate(yend = ifelse(yend == lag(yend, default = 0), yend - 0.2, yend)) %>%
   mutate(xend = ifelse(!right, 0L, max(mean_rank) + 1L)) %>%
   mutate(right = as.numeric(right))

  sub = sort(dd$mean_rank)
    # Compute a matrix of all possible bars
    mat = apply(t(outer(sub, sub, `-`)), c(1, 2),
      FUN = function(x) ifelse(x > 0 && x < cd.nemenyi, x, 0))
    # Get start and end point of all possible bars
    xstart = round(apply(mat + sub, 1, min), 3)
    xend   = round(apply(mat + sub, 1, max), 3)
    nem.df = data.table(xstart, xend, "diff" = xend - xstart)
    nem.df = nem.df[, .SD[which.max(.SD$diff)], by = "xend"]
    nem.df = nem.df[nem.df$xend - nem.df$xstart > 0, ]
    nem.df$y = seq(from = 0.1, to = 0.35, length.out = dim(nem.df)[1])

  p = ggplot(dd)
  p = p + geom_point(aes_string("mean_rank", 0, colour = "search_n"), size = 3)
  p = p + geom_segment(aes_string("mean_rank", 0, xend = "mean_rank", yend = "yend",
                                color = "search_n"), size = 1)
  p = p + geom_segment(aes_string("mean_rank", "yend", xend = "xend",
                                yend = "yend", color = "search_n"), size = 1)
  p = p + geom_text(aes_string("xend", "yend", label = "search_n",
                               hjust = "right"), vjust = -0.5)
  p = p + xlab("Average Rank")
  p = p + geom_segment(aes_string("xstart", "y", xend = "xend", yend = "y"),
                     data = nem.df, size = 2, color = "dimgrey", alpha = 0.9)
  p = p + annotate("text",
                 label = stri_paste("Critical Difference =", round(cd.nemenyi, 2), sep = " "),
                 y =  max(dd$yend) - .1, x = mean(dd$mean_rank))
  p = p + annotate("segment",
                 x =  mean(dd$mean_rank)- 0.5 * cd.nemenyi,
                 xend = mean(dd$mean_rank) + 0.5 * cd.nemenyi,
                 y = max(dd$yend) - .3,
                 yend = max(dd$yend) - .3,
                 size = 2L)
  p = p + theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank(),
                legend.position = "none",
                panel.background = element_blank(),
                panel.border = element_blank(),
                axis.line = element_line(size = 1),
                axis.line.y = element_blank(),
                panel.grid.major = element_blank(),
                plot.background = element_blank())
  p = p + ggtitle(learner)
  p = p + annotate("point", x = mean(dd$mean_rank), y = max(dd$yend) + 0.2, alpha = 0)

  p
}

stringBuilder = function(folder, what, learner.name, cfg = NULL) {
  files = list.files("surrogates")[grep(x = list.files("surrogates"), "regr.*_classif")]
  out = stri_paste(folder, "/", what, gsub("regr.", "", files[grep(stri_sub(learner.name, from = 5), x = files)]))
  if (!is.null(cfg))
    out = paste0(stri_sub(out, to = -6), "_", cfg, ".RDS")
  return(out)
}

load_surrogates = function(learner.name) {
  files = list.files("surrogates")[grep(x = list.files("surrogates"), "regr.cubist_classif")]
  catf("Loading surrogate for learner: %s", learner.name)
  # Read surrogates from Hard Drive
  surrogates = readRDS(stri_paste("surrogates/", files[grep(stri_sub(learner.name, from = 5), x = files)]))
  return(surrogates)
}

# ----------------------------------------------------------------------------------------
# Parameter Sets and Helpers for Param Sets

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
        requires = quote(booster == "gblinear"))),
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

makePerTaskParamSet = function() {
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
