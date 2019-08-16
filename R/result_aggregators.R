
rbind_res = function(lst) {
  assert_list(lst, names = "named")
  Reduce(bind_rows, Map(gather_res, res = lst, method = names(lst))) %>%
  group_by(dataset, method) %>%
  mutate(auc = cummax(auc), iter = iter) %>%
  filter(iter %in% c(1, 2, 4, 8, 16))
}

gather_res = function(res, method) {
  res %>%
    as_tibble() %>%
    mutate(iter = row_number()) %>%
    gather("dataset", "auc", -iter) %>%
    mutate(method = method)
}

plot_res = function(lst) {
    lst %>%
    mutate(auc = as.numeric(auc)) %>%
    ggplot(., aes(as.factor(iter), auc, fill = method)) +
    geom_boxplot() +
    theme_bw() +
    xlab("Iteration")
}
