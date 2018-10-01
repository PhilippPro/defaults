get_long_learner_name = function(learner) {
  sapply(learner, function(x) {
    learner = switch(x, 
      "glmnet" = "classif.glmnet.tuned",
      "rpart" = "classif.rpart.tuned",
      "svm" = "classif.svm.tuned",
      "xgboost" = "classif.xgboost.dummied.tuned"
    )
  })
}

# --------------------------------------------------------------------------------------------------
learner_to_default_set_name = function(learner) {
  sapply(learner, function(x) {
  switch(x,
    "glmnet" = "mlr_glmnet",
    "rpart" = "mlr_rpart",
    "xgboost" = "mlr_xgboost",
    "adaboost" = "sklearn_adaboost",
    "random forest" = "sklearn_random_forest",
    "svm" = "sklearn_svm")
  })
}

print_defaults = function(input) {
  set_name = learner_to_default_set_name(input$learner)
  df = readRDS(paste0("default_sets/median_defaults_", set_name, ".RDS"))$defaults
  df = data.frame(default_no = paste0(seq_len(nrow(df)), "."), df)
  df %>% data.table()
}

# lst = readRDS("defaultLOOCV/full_results.Rds")$oob.perf
# lst = lst %>%
#   filter(!(search.type %in% c("defaults_cycle", "defaults_mean", "hodges-lehmann", "randomBotData")))
# saveRDS(lst, "inverse_ecdf/full_results.Rds")

