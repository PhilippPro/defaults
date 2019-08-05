gather_res = function(res, method) {
  res %>% as_tibble() %>% mutate(iter = row_number()) %>%
    gather("dataset", "auc", -iter) %>% mutate(method = method)
}

# Performance
p = gather_res(res_xgb, "multi_default") %>%
bind_rows(gather_res(res_xgb_timesense, "timesense")) %>%
bind_rows(gather_res(res_xgb_rs, "random_search")) %>%
bind_rows(gather_res(res_xgb_rs2, "random_searchx2")) %>%
bind_rows(gather_res(res_xgb_rs4, "random_searchx4")) %>%
group_by(dataset, method) %>%
mutate(auc = cummax(auc), iter = as.factor(iter)) %>%
ggplot(., aes(iter, auc, fill = method)) + geom_boxplot()

ggsave(p, file = "figures/auc_eval_xgb_timesense_normal_rs.pdf")


p = gather_res(res_xgb_def_t, "multi_default") %>%
bind_rows(gather_res(res_xgb_tc_t, "timesense")) %>%
bind_rows(gather_res(res_xgb_rs_t, "random_search")) %>%
group_by(dataset, method) %>%
mutate(runtime_pct = cumsum(auc), iter = as.factor(iter)) %>%
filter(iter %in% c(1, 2, 4, 8, 12, 16)) %>%
ggplot(., aes(iter, runtime_pct, fill = method)) + geom_boxplot()

ggsave(p, file = "figures/runtime_eval_xgb_timesense_normal_rs.pdf")
