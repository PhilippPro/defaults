# Scales the performances according to the 4 agreed versions.
# For measures that need to be maximized (auc, acc), 1-x is computed before
scalePerformances = function(table, measure, scaling = "none") {
  # Make sure we always have a measure we want to minimize
  if (!measure$minimize)
    table$measure.value = measure$best - table$measure.value
  
  # Drop "perfect" performances
  if (scaling == "logit")
    table = table %>% filter(measure.value != 0)
  # Do the scaling
  table = switch(scaling,
    none = table,
    logit = logit(table),
    zscale = zNormalize(table),
    scale01 = zeroOneScale(table),
    table
    )
  return(table)
}


# Logit Transform  before subtracting means / dividing by standard deviation.
logit = function(x) {
  x %>% group_by(data_id) %>% mutate(measure.name = scale(log(measure.value / (1 - measure.value))))
}


# Normalize values by subtracting mean and dividing by standard deviation, so a z-score.
zNormalize = function(x) {
  x %>% group_by(data_id) %>% mutate(measure.value = scale(measure.value))
}


# Map to [0,1] where 1 is %(majority class) and 0 is best achieved performance for this learner
# across the whole parameter space.
zeroOneScale = function(x) {
  # FIXME: 1 should be majorityclass, not the minimum
  x %>%
    group_by(data_id) %>%
    mutate(measure.name = 1 - (measure.value - min(measure.value) / (max(measure.value) - min(measure.value))))
}
