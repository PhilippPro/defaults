# Obtains data from figshare, saves a data.frame
figshare_to_data = function() {
  load(url("https://ndownloader.figshare.com/files/10462297"))
  lps = getLearnerParSets()

  tbl.metaFeatures = tbl.metaFeatures %>% filter(quality %in% c("NumberOfFeatures", "NumberOfInstances"))

  learner_feats_list = tbl.hypPars %>%
    group_by(fullName) %>%
    do({
      param.set = lps[[substr(paste0(unique(.$fullName), ".set"), 5, 10^3)]]$param.set
      hpvals = spread(., name, value)

      # Ensure correct data.types for params
      params = getParamIds(param.set)
      param_types = getParamTypes(param.set)
      for(i in seq_along(params))
        hpvals[, params[i]] = convertParamType(hpvals[[params[i]]], param_types[i])
      bot.table = tbl.results %>%
        dplyr::rename(acc = accuracy) %>%
        gather("measure", "performance", -setup, -run_id, -task_id, -data_id) %>%
        inner_join(hpvals, by = "setup") %>%
        select(., -setup)

      # Scale mtry and min.node.size in random forest
      if (unique(.$fullName) == "mlr.classif.ranger") {
        bot.table = bot.table %>%
          inner_join(
            filter(tbl.metaFeatures, quality == "NumberOfFeatures") %>% select(., -quality),
            by = "data_id") %>%
          mutate(mtry = mtry / as.numeric(value)) %>%
          select(., -value) %>%
          inner_join(
            filter(tbl.metaFeatures, quality == "NumberOfInstances") %>% select(., -quality),
            by = "data_id") %>%
          mutate(min.node.size = log(min.node.size, 2) / log(as.numeric(value), 2)) %>%
          select(., -value)
      }
      bot.table %>%
       rename(learner_id = fullName) %>%
       select(-run_id)
    })
  saveRDS(file = "oml_bot_data.RDS", learner_feats_list)
}
