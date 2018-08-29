evalDefaultsSurrogates = function(task.ids, lrn, defaults, ps, it, n, overwrite = FALSE) {
  defaults = defaults[[it]][seq_len(n), , drop = FALSE]

  # The names of the surrogates are "OpenML Data Id's". We need "OpenML Task Id's.
  data_task_match = read.csv("oml_data_task.txt", sep = " ")
  if (is.character(task.ids)) task.ids = as.numeric(task.ids)
  task.name = data_task_match[data_task_match$data.id == task.ids, "task.name"]

  out = predict(surrogates$surrogates[[as.character(task.ids)]], newdata = defaults)$data %>%
    rename(auc.scaled = response)
  out$task.id = task.name
  out$n = n
  out$search.type = "defaults"
  return(out[which.min(out$auc.scaled), ])
}

evalRandomSearchSurrogates = function(task.ids, lrn, defaults, ps, it, n, overwrite = FALSE) {
  defaults = defaults[[it]][seq_len(n), , drop = FALSE]

  # The names of the surrogates are "OpenML Data Id's". We need "OpenML Task Id's.
  data_task_match = read.csv("oml_data_task.txt", sep = " ")
  if (is.character(task.ids)) task.ids = as.numeric(task.ids)
  task.name = data_task_match[data_task_match$data.id == task.ids, "task.name"]

  defaults = generateRandomDesign(n, ps)
  defaults = fixDefaultsForWrapper(defaults, lrn, lrn.ps)
  out = predict(surrogates$surrogates[[as.character(task.ids)]], newdata = defaults)$data %>%
    rename(auc.scaled = response)
  out$task.id = task.name
  out$n = n
  out$search.type = "random"
  return(out[which.min(out$auc.scaled), ])
}


