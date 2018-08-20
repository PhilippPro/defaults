create_report = function(learner.name, data, j) {
  targetfile = paste0("report_", learner.name, ".html")
  rmarkdown::render(
    knitr_in("report_tmpl.Rmd"),
    output_file = file_out(targetfile),
    quiet = TRUE, params = list(j = j))
}

make_plots = function(results, data, j) {
  # Boxplot of the different methods
  p = results %>%
    filter(search.type != "randomBotData") %>%
    filter(search.type %in% c("design", "package-default", "random")) %>%
    ggboxplot(., x = "n", y = "auc.test.mean", color = "n", add = "jitter") +
    ggtitle("Performance across all datasets") +
    facet_wrap(~search.type, scales = "free_x")
  
  # Boxplot of randomBotData
  r = results %>%
    filter(search.type == "randomBotData") %>%
    ggboxplot(., x = "n", y = "auc.test.mean", color = "n", add = "jitter") +
    ggtitle("Performance across all datasets") +
    facet_wrap(~search.type, scales = "free_x")
  
  # Comparison to package defaults
  g = compare_to_pkg_defaults(results) 
  
  # Comparison to n-fold random search
  nfold = compare_to_nfold_plot(results)
  
  # Plot ranks and mean of means etc.
  rankplot = make_rankplot(data)

  tsneplot = make_tsneplot(j)
  
  return(list(p = p, r = r, g = g, nfold = nfold, rankplot = rankplot, tsneplot = tsneplot))
  
}

update_all_results = function() {
  files = list.files("defaultLOOCV/save", full.names = TRUE)
  lst = list(oob.perf = do.call("bind_rows", lapply(files, readRDS)))
  saveRDS(lst, "defaultLOOCV/full_results.Rds")
}


get_results_from_folder = function(j, data_in) {
  # Get the saved performances (either partial or full result)
  learner = stri_sub(str = learner.names[j], from = 13)
  files = list.files("defaultLOOCV/save", full.names = TRUE)
  files = files[stri_detect_fixed(files , learner)]
  do.call("bind_rows", lapply(files, readRDS)) %>%
    filter(stri_detect_fixed(learner.id , learner)) %>%
    filter(!(task.id %in% c("nomao", "Bioresponse"))) # nomao | Bioresponse take really long
  
}

preprocess_results = function(oob.perf) {
  oob.perf %>%
    filter(search.type != "randomBotData") %>%
    group_by(task.id) %>%
    mutate(
      rnk = dense_rank(desc(auc.test.mean)), 
      auc.test.normalized = (auc.test.mean - min(auc.test.mean)) / (max(auc.test.mean) - min(auc.test.mean)))  %>%
    ungroup() %>%
    group_by(search.type, n) %>%
    summarise(
      mean_rank_auc = mean(rnk),
      mean_auc = mean(auc.test.mean),
      mn_auc_norm. = mean(auc.test.normalized),
      median_auc = median(auc.test.mean),
      cnt = n(),
      cnt_na = sum(is.na(auc.test.mean)))
}

compare_to_pkg_defaults = function(oob.perf, meas = "auc.test.mean") {
  # Boxplot comparing to package default
  gdata = oob.perf %>% 
    filter(search.type %in% c("design", "package-defaults", "random")) %>%
    filter(search.type != "randomBotData") %>%
    left_join(oob.perf %>%
        filter(search.type == "package-default") %>%
        mutate(
          auc.def = auc.test.mean,
          acc.def = acc.test.join,
          f1.def = f1.test.mean
        ),
      by = c("task.id", "learner.id")) %>%
    mutate(
      delta_auc = (auc.test.mean.x - auc.def),
      delta_acc = (acc.test.join.x - acc.def),
      delta_f1 = (f1.test.mean.x - f1.def)
    ) %>%
    filter(search.type.x != "package-default") %>%
    mutate(n = as.factor(n.x))
  
  delta = switch(meas, "auc.test.mean" = "delta_auc", "acc.test.join" = "delta_acc", "f1.test.mean" = "delta_f1")
  
  # Boxplot Differences
  ggboxplot(gdata, x = "search.type.x", y = delta, color = "n") +
    geom_abline(intercept = 0, slope = 0) +
    ggtitle(stri_paste("Performance difference to defaults for ", "auc.test.mean")) +
    ylab("Improvement over package defaults") +
    xlab("Search.type")
}

compare_to_nfold_plot = function(oob.perf, meas = "auc.test.mean") {
  # Boxplot comparing to package default
  gdata = oob.perf %>%
    filter(search.type == "design") %>%
    left_join(oob.perf %>%
        filter(search.type == "random") %>%
        mutate(
          auc.def = auc.test.mean,
          acc.def = acc.test.join,
          f1.def = f1.test.mean),
      by = c("task.id", "learner.id")) %>%
    filter(n.x %in% c(2, 4, 8)) %>%
    mutate(
      delta_auc =  auc.def- auc.test.mean.x,
      delta_acc = acc.def -acc.test.join.x,
      delta_f1 = f1.def - f1.test.mean.x,
      multiple = n.y / n.x) %>%
    filter(multiple %in% c(1, 2, 4, 8)) %>%
    mutate(n = as.factor(n.x), multiple = as.factor(multiple))
  
  delta = switch(meas, "auc.test.mean" = "delta_auc", "acc.test.join" = "delta_acc", "f1.test.mean" = "delta_f1")
  
  # Boxplot Differences
  ggboxplot(gdata, x = "search.type.y", y = delta, color = "multiple") +
    geom_abline(intercept = 0, slope = 0) +
    facet_grid(~ n) + 
    ggtitle(stri_paste("Performance difference to defaults for ", "auc.test.mean")) +
    ylab("Improvement over randomSearch") +
    xlab("Search.type")
}

save_plots_to_file = function(p, g, rp, r, i) {
  # Create combined plot
  ggsave(
    plot = (p / g),
    filename = paste0("defaultLOOCV/", "auc.test.mean", learner.names[i], "Q2", ".png"),
    scale = 3)
  ggsave(
    plot = (rp / r),
    filename = paste0("defaultLOOCV/", "auc.test.mean_randomBot", learner.names[i], "Q2", ".png"),
    scale = 3)
}

make_rankplot = function(data) {
  data_rp = tidyr::gather(data %>% select(-cnt, -cnt_na), "measure", "value", -c("n", "search.type"))
  ggplot(data_rp, aes(x = as.factor(n), y = value, color = search.type)) +
    geom_point() +
    ylab("Average rank") + xlab("No. defaults") +
    facet_wrap(~measure, scales = "free")
}

make_tsneplot = function(j) {
  # Eval defaults using tsne
  defs.file = stringBuilder("defaultLOOCV", "Q2_defaults", learner.names[j])
  defs = readRDS(defs.file)
  
  library(Rtsne)
  set.seed(2999 + j)
  defdf = do.call("bind_rows", list(defs$defaults, .id = "iter"))
  
  nums = sapply(defdf, is.numeric)
  # Add some noise to avoid duplicates
  if (sum(nums) > 0) {
    for(i in sum(nums)) {
      defdf[, nums][, i] = defdf[, nums][, i] + rnorm(nrow(defdf), 0, 10^-8)
    }
  }
  
  defdf = defdf[!duplicated(defdf), ]
  rtsne = Rtsne(defdf[, -1], theta = 0.1)
  
  pdf = as.data.frame(rtsne$Y)
  pdf$n = as.factor(rep(1:10, 19))
  pdf$iter = as.factor(defdf$iter)
  # p1 = ggplot(pdf, aes(x = V1, y = V2)) + geom_density2d() + facet_wrap(~n)
  p2 = ggplot(pdf, aes(x = V1, y = V2, color = iter), alpha = 0.7) + geom_point() + facet_wrap(~n) + geom_jitter()
  return(p2)
}


compare_defaults_random_multiples = function(perfs) {
  # How long do I need to search to beat defaults
  perfs = lst %>% filter(search.type == "random") %>%
    left_join(lst$oob.perf %>% filter(search.type == "design"), suffix = c(".random", ".default"), by = c("task.id")) %>%
    group_by(task.id) %>%
    mutate(delta_auc = auc.test.mean.random - auc.test.mean.default) %>%
    filter(n.default <= n.random) %>% 
    select(task.id, delta_auc, auc.test.mean.random, auc.test.mean.default, n.random, n.default) %>%
    mutate(x.random = n.random / n.default) %>%
    group_by(n.default, x.random) %>%
    summarize(
      mean.difference.auc = mean(delta_auc),
      defs.better = sum(delta_auc < 0),
      tie = sum(delta_auc == 0),
      rnd.better = sum(delta_auc > 0))
  
  p1 = ggplot(perfs, aes(x = x.random, y = defs.better / (defs.better + rnd.better), color = as.factor(n.default))) +
    geom_point() + geom_line()
  p2 = ggplot(perfs, aes(x = x.random, y = mean.difference.auc, color = as.factor(n.default))) +
    geom_point() + geom_line()
  return(p1 + p2)
}