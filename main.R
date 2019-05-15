library(devtools)         # load_all()
library(foreach)
library(ParamHelpers)
load_all()


# Preprocessing Steps (only run once)
# Step 1: Obtain OML Bot Data -> Run figshare_to_data() to download the data once.
# Step 2: Train Surrogates    -> train_surrogates_omlbot()
# Step 3: Search for the defaults -> calculate_defaults.R


df = rbind(
  # to_long(res1, "xgb_svm"),
  to_long(res_xgb, "xgboost"),
  to_long(res_svm, "svm"),
  to_long(res_all, "all"),
  to_long(res_ranger, "ranger"),
  to_long(res_glmnet, "glmnet")
)

library(ggplot2)
library(patchwork)
pdf = df %>%
  arrange(learner, task, id) %>%
  group_by(learner, task) %>%
  mutate(auc_max = cummax(auc)) %>%
  filter(id %in% c(1, 2, 4, 8, 16, 32))

pdf %>% group_by(learner) %>% summarize(mean(auc_max))

df2 = df %>%
  arrange(learner, task, id) %>%
  group_by(task) %>%
  mutate(auc = (auc - min(auc)) / (max(auc) - min(auc))) %>%
  group_by(task, learner) %>%
  mutate(auc_norm_max = cummax(auc))

p = ggplot(df2, aes(x = id, y = auc, color = learner)) + geom_line() + facet_wrap(~task)
ggsave("defaults_cummax.pdf", p, width = 20, height = 10)

p2 = ggplot(pdf, aes(x = as.factor(id), y = auc_max, color = learner)) +
  geom_boxplot()
ggsave("defaults_comparison.pdf", p2, width = 20, height = 10)
}
