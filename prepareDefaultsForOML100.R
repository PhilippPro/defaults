library(farff)
library(dplyr)
library(mlr)
library(focussearch)  # Search the surrogates
library(doMC)         # Parallelization

registerDoMC(3)

# Define a surrogate model
source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
surrogate.lrn = makeLearner("regr.cubist", committees = 20, extrapolation = 20)

# Train Surrogates
surrogates = foreach(sklearner = c("adaboost", "randomforest", "libsvm_svc")) %dopar% {
  dflst = readARFF(paste0("sklearn_oml100/", sklearner, ".arff")) %>% split(f = df$task_id)
  foreach( i = seq_len(100)) %do% {
    tsk = dflst[[1]] %>% makeRegrTask(id = paste0(sklearner, names(dflst)[1]), data = ., target = "y")
    mod = train(surrogate.lrn, tsk)
  }
}
saveRDS(surrogates, "sklearn_oml100/surrogates.RDS")

surrogates = readRDS("sklearn_oml100/surrogates.RDS")


param.set = getSkLearnParamsets()

# Defaults
defs.file = stringBuilder("sklearn_oml100/defaults", "defaults", sklearner)
# Compute defaults if not yet available
if (!file.exists(defs.file)) {
  # Iterate over ResampleInstance and its indices
  defs = foreach(it = seq_len(rin$desc$iters)) %dorng% {
    set.seed(199 + i)
    # Search for defaults
    defs = searchDefaultsOML100(
      surrogates[-it], # training surrogates (L-1-Out-CV)
      param.set[[sklearner]], # parameter space to search through
      n.defaults = 32)
    return(defs)
  }
  # Save found defaults as RDS
  saveRDS(list("defaults" = defs), defs.file)
}