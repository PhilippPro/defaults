library(devtools)         # load_all()
library(stringi)          # string manipulation
library(focussearch)      # Search the surrogates
load_all()                # /R
load_all("../surrogates") # Surrogates

source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
lrn = makeLearner("regr.fixcubist", committees = 20, extrapolation = 20)

s = SurrogateFromRDS$new(
  oml_task_id = 31,
  baselearner_name = "glmnet",
  data_source = "data/oml_bot_data.RDS",
  measure_name = "auc",
  param_names = "lambda",
  surrogate_learner = lrn)
s$train()

