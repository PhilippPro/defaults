# OpenML Task Ids
get_oml_task_ids = function() {
  # Do not use: 14971L, 34539L. Perfs are constant everywhere
  c(3L, 31L, 37L, 43L, 49L, 219L, 3485L, 3492L, 3493L, 3494L, 3889L,
    3891L, 3896L, 3899L, 3902L, 3903L, 3913L, 3917L, 3918L, 3954L,
    34536L, 14965L, 10093L, 10101L, 9980L, 9983L, 9970L,
    9971L, 9976L, 9977L, 9978L, 9952L, 9957L, 9967L, 9946L, 9914L,
    14966L, 34537L
  )
}

# Available learners
get_baselearners = function() {
  c("glmnet", "rpart", "svm", "ranger", "xgboost")
}

# Available measures
get_measures = function() {
  c("auc", "acc", "brier")
}

#' Substitute NA's with out of bounds data.
deleteNA = function(task.data) {
  for(i in 1:ncol(task.data)) {
    if(is.numeric(task.data[, i]))
      task.data[is.na(task.data[, i]), i] = -10 - 1
    if(is.factor(task.data[, i])) {
      task.data[, i] = addNA(task.data[, i])
      task.data[, i] = droplevels(task.data[, i])
    }
    if(is.logical(task.data[, i]))
      task.data[, i] = as.factor(task.data[, i])
  }
  task.data
}

