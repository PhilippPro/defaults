#' Train and save surrogate models on all datasets
#' @param surrogate.mlr.lrn Surrogate learner
#' @param measure Name of the measure to optimize.
#'   Can be one of: measures = list(auc, acc, brier)
#' @param scaling Scaling to use. 
#'   Can be one of: scaling = c("none", "logit", "zscale", "scale01")
train_save_surrogates = function(surrogate.mlr.lrn, lrn.par.set, learner.names, measure = auc, scaling = "zscale") {
  data.ids = sort(unique(tbl.results$data_id))
  
  foreach(j = seq_along(learner.names)) %:% {
    sprintf("Learner %i: %s", j, learner.names[j])
    set.seed(199 + j)
    # Surrogate model calculation
    surrogates = makeSurrogateModels(measure = measure, learner.name = learner.names[j],
                                     data.ids = data.ids, tbl.results, tbl.metaFeatures, tbl.hypPars,
                                     lrn.par.set, surrogate.mlr.lrn,
                                     scale_before = TRUE, scaling = scaling)
    # Save surrogates
    saveRDS(surrogates, file = paste("surrogates/", surrogate.mlr.lrn$id,
                                     stri_sub(learner.names[j], from = 5),
                                     measure$id, scaling, ".RDS", sep = "_", collapse = "_"))
  }
  messagef("Successfully computed all surrogates")
  return(surrogates)
}



#' Create surrogate models for different tasks
#' @param measure Name of the measure to optimize
#' @param learner.name Name of learner
#' @param data.ids [\code{numeric}] ids of the dataset
#' @param lrn.par.set learner-parameter set which should include relevant bounds for flow
#' @param tbl.results df with getMlrRandomBotResults()
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @param tbl.metaFeatures df with getMlrRandomBotHyperpars()
#' @param min.experiments minimum number of experiments that should be available for a dataset, otherwise the dataset is excluded
#' @return surrogate model
makeSurrogateModels = function(measure, learner.name, data.ids, tbl.results, 
                               tbl.metaFeatures, tbl.hypPars, lrn.par.set, surrogate.mlr.lrn, scale_before = TRUE, scaling = "none") {
  
  
  param.set = lrn.par.set[[which(names(lrn.par.set) == paste0(substr(learner.name, 5, 100), ".set"))]]$param.set
  
  #train mlr model on full table for measure
  task.data = makeBotTable(measure, learner.name, tbl.results, tbl.metaFeatures, tbl.hypPars, param.set, data.ids, scale_before, scaling)
  task.data = data.frame(task.data)
  task.data = deleteNA(task.data)
  
  # get specific task ids
  if(!is.null(data.ids)) {
    uni = unique(task.data$data_id)
    data.ids = sort(uni[uni %in% data.ids])
  } else {
    data.ids = sort(unique(task.data$data_id))
  }
  
  mlr.mod.measure = foreach(i = seq_along(data.ids)) %dopar% {
    gc()
    print(paste("surrogate train: task", i, "of", length(data.ids)))
    data.idi = data.ids[i]
    
    mlr.task.measure = makeRegrTask(id = as.character(data.idi),
                                    subset(task.data, data_id == data.idi,
                                           select =  c("measure.value", names(param.set$pars))), target = "measure.value")
    train(surrogate.mlr.lrn, mlr.task.measure)
  }
  names(mlr.mod.measure) = data.ids
  return(list(surrogates = mlr.mod.measure, param.set = param.set))
}


#' Merge results, hyperpars and features tables and prepare for mlr.task input
#' @param measure Which measure to analyse
#' @param learner.name What learner to analyse
#' @param tbl.results df with getMlrRandomBotResults()
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @param tbl.metaFeatures df with getMlrRandomBotHyperpars()
#' @return [\code{data.frame}] Complete table used for creating the surrogate model 
makeBotTable = function(measure, learner.name, tbl.results, tbl.metaFeatures, tbl.hypPars, param.set, data.ids, scale_before = TRUE, scaling = "none") {
  
  tbl.hypPars.learner = tbl.hypPars %>% filter(fullName == learner.name)
  tbl.hypPars.learner = spread(tbl.hypPars.learner, name, value)
  tbl.hypPars.learner = data.frame(tbl.hypPars.learner)
  # Convert the columns to the specific classes
  params = getParamIds(param.set)
  param_types = getParamTypes(param.set)
  for(i in seq_along(params))
    tbl.hypPars.learner[, params[i]] = convertParamType(tbl.hypPars.learner[, params[i]], param_types[i])
  
  measure.name = measure$id
  bot.table = tbl.results %>%
    rename(acc = accuracy) %>%
    select(one_of("setup", measure.name, "data_id", "task_id")) %>%
    inner_join(tbl.hypPars.learner, by = "setup") %>%
    select(., -setup)
  
  # Scale mtry and min.node.size in random forest
  if(learner.name == "mlr.classif.ranger"){
    n_feats = filter(tbl.metaFeatures, quality == "NumberOfFeatures") %>%
      select(., -quality)
    n_feats$value = as.numeric(n_feats$value)
    bot.table = inner_join(bot.table, n_feats, by = "data_id")
    bot.table$mtry = bot.table$mtry/bot.table$value
    bot.table = bot.table %>% select(., -value)
    
    n_inst = filter(tbl.metaFeatures, quality == "NumberOfInstances") %>%
      select(., -quality)
    n_inst$value = as.numeric(n_inst$value)
    bot.table = inner_join(bot.table, n_inst, by = "data_id")
    bot.table$min.node.size = log(bot.table$min.node.size, 2) / log(bot.table$value, 2)
    bot.table = bot.table %>% select(., -value)
  }
  
  bot.table = bot.table %>% select(., -task_id)
  colnames(bot.table)[colnames(bot.table) == measure$id] = "measure.value"
  bot.table$measure.value = as.numeric(bot.table$measure.value)
  if (scale_before)
    bot.table = scalePerformances(bot.table, measure, scaling)
  
  # select only runs on the specific data.ids
  bot.table =  subset(bot.table, data_id %in% data.ids)
  
  return(bot.table)
}



#' #' Resample surrogate models on all datasets
#' #' @param surrogate.mlr.lrn Surrogate learner
#' #' @param measure Name of the measure to optimize.
#' #'   Can be one of: measures = list(auc, acc, brier)
#' #' @param scaling Scaling to use. 
#' #'   Can be one of: scaling = c("none", "logit", "zscale", "scale01")
#' resample_surrogates = function(surrogate.mlr.lrn, lrn.par.set, learner.names, measure = auc, scaling = "zscale") {
#'   
#'   task.data = readRDS(file = "surrogates/task.data.RDS")
#'   task.data$measure.value = as.numeric(task.data$measure.value)
#' 
#'   
#'   dlist = task.data %>%
#'     select(-fullName) %>%
#'     filter(data_id %in% train_split()) %>%
#'     split(.$data_id)
#'   
#'   registerDoParallel(30)
#'   coms = c(1, 2, 3, 5, 10, 20, 50)
#'   surrogate.mlr.lrn = makeLearner("regr.cubist", committees = coms[6], extrapolation = 20)
#'   res = foreach(i = seq_along(dlist)) %dopar% {
#'     t = makeRegrTask(data = dlist[[i]], target = "measure.value")
#'     t = subsetTask(t, features = which(getTaskFeatureNames(t) != "data_id"))
#'     resample(surrogate.mlr.lrn, t, resampling = cv3)$aggr
#'   }
#'   mean(sapply(res, mean))
#'   
#'   # 1  = 0.001991945
#'   # 2  = 0.001737474
#'   # 3  = 0.001633081
#'   # 5  = 0.001601866
#'   # 10 = 0.001558349
#'   # 20 = 0.001487181 <- we take 20
#'   # 50 = 0.001486619
#'   
#' }