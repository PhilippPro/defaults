library(devtools)     # load_all()
library(stringi)      # string manipulation
library(focussearch)  # Search the surrogates
library(doParallel)   # Parallelization
library(doMC)         # Parallelization
library(doRNG)        # Parallel RNG
library(foreach)      # Parallelization
load_all()

lrn.par.sets = getLearnerParSets()
learner.names = stri_sub(stri_paste("mlr.", names(lrn.par.sets)), 1, -5)

source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
surrogate.mlr.lrn = makeLearner("regr.cubist", committees = 20, extrapolation = 20)

files = list.files("surrogates")[grep(x = list.files("surrogates"), "regr.cubist_classif")]

# Defaults
defs.file = stringBuilder("defaultLOOCV", "Q2_defaults", learner.names[i])
# Compute defaults if not yet available
if (!file.exists(defs.file)) {
  # Iterate over ResampleInstance and its indices
  defs = foreach(it = seq_len(rin$desc$iters)) %dorng% {
    set.seed(199 + i)
    # Search for defaults
    defs = searchDefaults(
      surrogates$surrogates[rin$train.inds[[it]]], # training surrogates (L-1-Out-CV)
      surrogates$param.set, # parameter space to search through
      n.defaults = 10, # Number of defaults we want to find
      probs = 0.5) # Quantile we want to optimize
    return(defs)
  }
  # Save found defaults as RDS
  saveRDS(list("defaults" = defs), defs.file)
}

catf("Learner: %s", learner.names[i])
set.seed(199 + i)

# Read surrogates from Hard Drive
surrogates = readRDS(stri_paste("surrogates/", files[grep(stri_sub(learner.names[i], from = 5), x = files)]))
# Create resampling train/test splits
rin = makeResampleInstance(makeResampleDesc("CV", iters = 38), size = length(surrogates$surrogates))