learner_to_default_set_name = function(learner) {
  sapply(learner, function(x) {
  switch(x,
    "glmnet" = "mlr_glmnet",
    "rpart" = "mlr_rpart",
    "xgboost" = "mlr_xgboost",
    "adaboost" = "sklearn_adaboost",
    "random_forest" = "sklearn_random_forest",
    "libsvm_svc" = "sklearn_libsvm_svc")
  })
}

print_defaults = function(learner) {
  set_name = learner_to_default_set_name(learner[1])
  df = readRDS(paste0("default_sets/median_defaults_", set_name, ".RDS"))$defaults
  df = data.frame(default_no = paste0(seq_len(nrow(df)), "."), df)
  df %>% data.table()
}

preproc_data = function(input, learner) {
  readRDS("full_results.Rds") %>%
    filter(search.type %in% input$search.type) %>%
    filter(n %in% input$ndef | search.type != "defaults") %>%
    filter(n %in% input$nrs | search.type != "random") %>%
    filter(learner.short %in% learner) %>%
    mutate(n = as.factor(n)) %>%
    mutate(search.typeXn = paste(search.type, n, sep = "_")) %>%
    mutate(task.idXn = paste(task.id, n, sep = "_"))
}

aggregate_data = function(data, input, measure_str) {
  
  # Define symbols for dplyr quoting
  variable = rlang::sym(input$color)
  measure = rlang::sym(measure_str)
  
  # compute rank
  data = data %>%
    group_by(task.id) %>%
    mutate(
      rnk = dense_rank(desc(!! measure)),
      measure.normalized = (!! measure - min(!! measure)) / (max(!! measure) - min(!! measure))
      ) %>%
    ungroup() %>%
    mutate(measure.normalized = ifelse(is.nan(measure.normalized), 1, measure.normalized))
  
  # Group depending on facet variable
  if (input$color == "learner.id") {
    data = data %>%
      group_by(learner.short, search.type, n) 
  } else if (input$color == "task.id") {
    data = data %>%
      group_by(task.id)
  } else if (input$color == "search.type and n") {
    variable = rlang::sym("search.typeXn")
    data = data %>%
      group_by(search.typeXn)
  } else if (input$color == "task.id and n") {
    variable = rlang::sym("task.idXn")
    data = data %>%
      group_by(task.idXn)
  } else {
    data = data %>%
      group_by(search.type, n) 
  }
  
  
  # Aggregate
  if (input$framework == "mlr") {
    data = data %>%
      summarise(
        mean_rank_auc = mean(rnk),
        mean_auc = mean(!! measure),
        mn_auc_norm = mean(measure.normalized),
        median_auc = median(!! measure),
        cnt = n(),
        cnt_na = sum(is.na(!! measure))) %>%
      group_by(!! variable) %>%
      summarize(mean_rank_auc = mean(mean_rank_auc), mean_auc = mean(mean_auc),
        mean_auc_norm. =  mean(mn_auc_norm))
  } else {
    data = data %>%
      summarise(
        mean_rank_acc = mean(rnk),
        mean_acc = mean(!! measure),
        mn_acc_norm = mean(measure.normalized),
        median_acc = median(!! measure),
        cnt = n(),
        cnt_na = sum(is.na(!! measure))) %>%
      group_by(!! variable) %>%
      summarize(mean_rank_acc = mean(mean_rank_acc), mean_auc = mean(mean_acc),
        mean_acc_norm. =  mean(mn_acc_norm))
  }
  return(data)
}


# get_long_learner_name = function(learner) {
#   sapply(learner, function(x) {
#     learner = switch(x, 
#       "glmnet" = "classif.glmnet.tuned",
#       "rpart" = "classif.rpart.tuned",
#       "svm" = "classif.svm.tuned",
#       "xgboost" = "classif.xgboost.dummied.tuned"
#     )
#   })
# }
# --------------------------------------------------------------------------------------------------
## Preprocess results and save as RDS
# lst = readRDS("defaultLOOCV/full_results.Rds")$oob.perf
# lst = lst %>%
#   filter(!(search.type %in% c("defaults_cycle", "defaults_mean", "hodges-lehmann", "randomBotData"))) %>%
#   mutate(search.type = ifelse(search.type == "design", "defaults", search.type)) %>%
#   select(-(timetrain.test.sum:timepredict.test.sum)) %>%
#   rename(acc.test.mean = acc.test.join) %>%
#   filter(learner.id != "classif.svm.tuned") %>%
#   mutate(learner.short = ifelse(learner.id == "classif.rpart.tuned", "rpart",
#     ifelse(learner.id == "classif.glmnet.tuned", "glmnet",
#       ifelse(learner.id == "classif.xgboost.dummied.tuned", "xgboost", NA)))) %>%
#   mutate(n = as.factor(n), learner.id = as.factor(learner.id), task.id = as.factor(task.id))
# 
# dfsklearn = lapply(list.files("results_sklearn", full.names = TRUE), read.csv, stringsAsFactors = FALSE) %>%
#   setNames(., stri_sub(list.files("results_sklearn"), to = -5)) %>%
#   bind_rows(., .id = "learner.id") %>%
#   rename(task.id = task_id) %>%
#   rename(acc.test.mean = evaluation) %>%
#   separate(strategy_name, c("search.type", "n"), "__") %>%
#   mutate(n = as.factor(n)) %>%
#   mutate(learner.id = as.factor(learner.id)) %>%
#   mutate(search_n = as.factor(paste0(search.type, "_", n))) %>%
#   select(-X) %>%
#   mutate(search.type = ifelse(search.type == "greedy", "defaults", search.type)) %>%
#   mutate(search.type = ifelse(search.type == "random_search", "random", search.type)) %>%
#   mutate(learner.short = learner.id) %>%
#   select(-configuration_specification) %>%
#   mutate(auc.test.mean = 0, f1.test.mean = 0) %>%
#   select(-flow_id) %>%
#   mutate(task.id = as.factor(task.id)) %>%
#   select(-search_n)
# 
# mlr_sklearn_df = bind_rows(lst, dfsklearn)
#   
# saveRDS(mlr_sklearn_df, "inverse_ecdf/full_results.Rds")

