gather_res = function(res, method) {
  res %>%
    as_tibble() %>%
    mutate(iter = row_number()) %>%
    gather("dataset", "auc", -iter) %>%
    mutate(method = method)
}

rbind_res = function(lst) {
  assert_list(lst, names = "named")
  Reduce(bind_rows, Map(gather_res, res = lst, method = names(lst))) %>%
  group_by(dataset, method) %>%
  mutate(auc = cummax(auc), iter = iter) %>%
  filter(iter %in% c(1, 2, 4, 8, 16))
}

plot_res = function(lst) {
    rbind_res(lst) %>%
    ggplot(., aes(as.factor(iter), auc, fill = method)) +
    geom_boxplot() +
    theme_bw() +
    xlab("Iteration")
}

xgb_timesense = list("xgboost" = res_xgb, "xgb_time" = res_xgbt)
rbind_res(xgb_timesense) %>% spread(method, auc) %>% group_by(iter) %>% summarize(mean(xgb_time - xgboost))
plot_res(xgb_timesense)

xgb_timesense = list("xgboost" = res_svm, "xgb_time" = res_svmt)
rbind_res(xgb_timesense) %>% spread(method, auc) %>% group_by(iter) %>% summarize(mean(xgb_time - xgboost))
plot_res(xgb_timesense)

compare_baslearners = list("glmnet" = res_glmnet, "ranger" = res_ranger, "svm" = res_svm,
  "xgboost" = res_xgb,  "all_learners" = res_all)
plot_res(compare_baslearners)


xgb_timesense = list("xgboost" = res_xgb, "xgb_time" = res_xgb_mean)
rbind_res(xgb_timesense) %>% spread(method, auc) %>% group_by(iter) %>% summarize(mean(xgb_time - xgboost))
plot_res(xgb_timesense)


ggsave(p, file = "figures/auc_eval_xgb_timesense_normal_rs.pdf")


p = gather_res(res_xgb_def_t, "multi_default") %>%
bind_rows(gather_res(res_xgb_tc_t, "timesense")) %>%
bind_rows(gather_res(res_xgb_rs_t, "random_search")) %>%
group_by(dataset, method) %>%
mutate(runtime_pct = cumsum(auc), iter = as.factor(iter)) %>%
filter(iter %in% c(1, 2, 4, 8, 12, 16)) %>%
ggplot(., aes(iter, runtime_pct, fill = method)) + geom_boxplot()

ggsave(p, file = "figures/runtime_eval_xgb_timesense_normal_rs.pdf")
