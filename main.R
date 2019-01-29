library(devtools)         # load_all()
library(stringi)          # string manipulation
library(focussearch)      # Search the surrogates
library(foreach)
library(R6)
library(ParamHelpers)
load_all()
load_all("../surrogates") # Surrogates


# Preprocessing Steps (only run once)
# Step 1: Obtain OML Bot Data -> figshare_to_data()
# Step 2: Train Surrogates    -> train_surrogates_omlbot()

# Step 3: Search for the surrogates
# Load Collection of Surrogates
library(doParallel)
registerDoParallel(19)

# svm, xgboost
sc = make_surrogates_omlbot(baselearners = c("svm", "xgboost"))
res1 = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 10 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$evaluate_defaults_holdout()
}

# xgboost
sc = make_surrogates_omlbot(baselearners = "xgboost")
res_xgb = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 10 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$evaluate_defaults_holdout()
}

# svm
sc = make_surrogates_omlbot(baselearners = "svm")
res_svm = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 10 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$evaluate_defaults_holdout()
}

# ranger
sc = make_surrogates_omlbot(baselearners = "ranger")
res_ranger = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 10 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$evaluate_defaults_holdout()
}

# glmnet
sc = make_surrogates_omlbot(baselearners = "glmnet")
res_glmnet = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 10 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$evaluate_defaults_holdout()
}

# svm, xgboost
sc = make_surrogates_omlbot(baselearners = c("svm", "xgboost", "ranger", "glmnet", "rpart"))
res_all = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 10 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, holdout_task_id = oml_task_id, "median")
  ds$search_defaults()
  ds$save_to_disk()
  ds$evaluate_defaults_holdout()
}

library(tidyr)

to_long = function(res, lrn) {
  res = data.frame(res)
  res$id = seq_len(32)
  long = gather(res, "task", "auc", -id)
  long$learner = lrn
  long
}

df = rbind(
  to_long(res1, "xgb_svm"),
  to_long(res_xgb, "xgboost"),
  to_long(res_svm, "svm"),
  to_long(res_all, "all"),
  to_long(res_ranger, "ranger"),
  to_long(res_glmnet, "glmnet")
)

library(ggplot2)
library(patchwork)
pdf = df %>%
  filter(id %in% c(1, 2, 4, 8, 16, 32)) %>%
  mutate(id = as.factor(id)) %>%
  group_by(learner, task) %>%
  mutate(auc_max = cummax(auc))

ggplot(df, aes(x = as.numeric(id), y = auc, group = task)) + geom_line() + facet_wrap(~learner)


p1 = ggplot(pdf, aes(x = id, y = auc, color = learner)) +
  geom_boxplot()
p2 = ggplot(pdf, aes(x = id, y = auc_max, color = learner)) +
  geom_boxplot()
ggsave("defaults_comparison.pdf", (p1 + p2), width = 20, height = 10)


for (i in 1:32) print(sc$surrogates[[1]]$predict(ds$defaults.params[[i]]))

ds$holdout_task_id
ds$sc$predict(ds$defaults.params, 34537)

ds$sc$aggfun
