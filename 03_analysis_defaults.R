library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)


# Comparison to Random Search
#-----------------------------------------------------------------------------------------
# sklearn
plot_results_vs_rs(sc_ada, res_ada, "adaboost") +
plot_results_vs_rs(sc_libsvm_svc, res_libsvm_svc, "libsvm_svc") +
plot_results_vs_rs(sc_random_forest, res_random_forest, "random_forest")

# mlr
plot_results_vs_rs(sc_xgb, res_xgb, "xgboost") +
plot_results_vs_rs(sc_svm, res_svm, "svm") +
plot_results_vs_rs(sc_ranger, res_ranger, "ranger") +
plot_results_vs_rs(sc_glmnet, res_glmnet, "glmnet")

plot_results_vs_rs(sc_all, res_all, "All learners")


# Single learners vs aggregates
#-----------------------------------------------------------------------------------------
gather_res(res_xgb, method = "xgboost") %>%
  bind_rows(gather_res(res_ranger, method = "ranger")) %>%
  bind_rows(gather_res(res_all, method = "all")) %>%
  filter(iter %in% c(1, 2, 4, 8, 16)) %>%
  plot_res("Comparison across learners")





ls = list.files("data/intermediate/surrogates/xgboost/ranger_surrogate/auc/range_timecrit_b_1p_0.5")

get_oml_task_ids()[!(get_oml_task_ids() %in% as.numeric(stringi::stri_extract(ls, regex = "[0-9][0-9]*")))]



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
