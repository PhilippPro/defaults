library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

# -----------  SKLEARN   -----------------------------------------------------------------

plot_results = function(sc, res, title) {
  res = gather_with_rs(sc, res)
  plot_res(res, title)
}

plot_results(sc_ada, res_ada, "adaboost") +
plot_results(sc_libsvm_svc, res_libsvm_svc, "libsvm_svc") +
plot_results(sc_random_forest, res_random_forest, "random_forest")

# -----------  MLR  -----------------------------------------------------------------






res %>%
  group_by(method) %>%
  filter(iter %in% c(1, 2, 4, 8, 16)) %>%
  spread("method", "auc") %>% group_by(iter) %>% summarize_if(is.numeric, mean)


library(tidyr)
library(tibble)
library(dplyr)
library(ggplot2)

# xgboost
res_xgb_rs = baseline_random_search(sc_xgb, 16L)
res_defs = rbind_res(list("xgboost" = res_xgb, "xgb_time" = res_xgbt))

res_xgb = res_xgb_rs %>%
  bind_rows(res_defs) %>%
  spread(method, auc)

res_xgb %>%
  group_by(iter) %>%
  summarize_if(is.numeric, median)

plot_res(res_xgb)



xgb_timesense = list("xgboost" = res_svm, "xgb_time" = res_svmt)
rbind_res(xgb_timesense) %>% spread(method, auc) %>% group_by(iter) %>% summarize(mean(xgb_time - xgboost))
plot_res(xgb_timesense)

compare_baslearners = list("glmnet" = res_glmnet, "ranger" = res_ranger, "svm" = res_svm,
  "xgboost" = res_xgb,  "all_learners" = res_all)
plot_res(rbind_res(compare_baslearners))


gather_res(res_glmnet, method = "glmnet")

xgb_timesense = list("xgb_mean" = res_xgb, "xgb_median" = res_xgb_median, "xgb_mix" = res_xgb_mix)
rbind_res(xgb_timesense) %>% spread(method, auc) %>% group_by(iter) %>% summarize(d_med = mean(xgb_median - xgb_mean), d_mix = mean(xgb_mix - xgb_mean), vs_med = mean(xgb_mean > xgb_median), vs_mix = mean(xgb_mean > xgb_mix), med_vs_mean = mean(xgb_median > xgb_mix))
plot_res(xgb_timesense)



ggsave(p, file = "figures/auc_eval_xgb_timesense_normal_rs.pdf")


p = gather_res(res_xgb_def_t, "default") %>%
bind_rows(gather_res(res_xgb_tc_t, "timesense")) %>%
bind_rows(gather_res(res_xgb_rs_t, "random_search")) %>%
group_by(dataset, method) %>%
mutate(runtime_pct = cumsum(auc), iter = as.factor(iter)) %>%
filter(iter %in% c(1, 2, 4, 8, 12, 16)) %>%
ungroup()
p %>%
ggplot(., aes(iter, runtime_pct, fill = method)) + geom_boxplot()

ggsave(p, file = "figures/runtime_eval_xgb_timesense_normal_rs.pdf")

p %>% select(-auc) %>% spread(method, runtime_pct) %>% group_by(iter) %>% summarize_if(is.numeric, mean)
