library(devtools)     # load_all()
library(stringi)      # string manipulation
library(focussearch)  # Search the surrogates
library(doParallel)   # Parallelization
library(doMC)         # Parallelization
library(doRNG)        # Parallel RNG
library(foreach)      # Parallelization
library(patchwork)    # Vizualisation
library(ggpubr)       # Vizualisation
library(drake)        # Reports
load_all()
# packrat::restore()

# Get randomBot Data from the figshare repository     ----------------------------------------------
# load(url("https://ndownloader.figshare.com/files/10462297"))

# Train/Save the surrogates ------------------------------------------------------------------------
lrn.par.sets = getLearnerParSets()
learner.names = stri_sub(stri_paste("mlr.", names(lrn.par.sets)), 1, -5)

# Use cubist as a learner
# 20 Comittees in order to get a lower mse (25% better then 1 comittee) 
# and set extrapolation to 20 in order to not extrapolate to no-data areas to much.
# Obtained a fixed version of cubist learner from github branch.
source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
surrogate.mlr.lrn = makeLearner("regr.cubist", committees = 20, extrapolation = 20)

registerDoMC(20)
trainSaveSurrogates(surrogate.mlr.lrn, lrn.par.sets, learner.names)
stopImplicitCluster()

# Extract a grid from the surrogates
# foreach(i = 4) %do% { # seq_along(learner.names)
#   parallelMap::parallelStartMulticore(parallel::detectCores())
#   predictGridFromSurrogates(readRDS("surrogates/regr.cubistclassif.svmauczscale.RDS"), learner.names[i]) 
#   parallelMap::parallelStop()
# }


# Forward selection --------------------------------------------------------------------------------
i = 2 # 1: glmnet, 2: rpart, 4: svm, 6: xgboost

# Read surrogates from Hard Drive
surrogates = load_surrogates(learner.names[i])

# Create resampling train/test splits
set.seed(199 + i)
rin = makeResampleInstance(makeResampleDesc("CV", iters = 38), size = length(surrogates$surrogates))


registerDoMC(20)
defs.file = stringBuilder("defaultLOOCV", "hodges-lehmann", learner.names[i])
# ------------------------------------------------------------------------------------------------
# Defaults
# Compute defaults if not yet done
# probs : 0.5; mean, cycle, hodges-lehmann
if (!file.exists(defs.file)) {
  # Iterate over ResampleInstance and its indices
  defs = foreach(it = seq_len(rin$desc$iters)) %dorng% {
    set.seed(199 + i)
    # Search for defaults
    defs = searchDefaults(
      surrogates$surrogates[rin$train.inds[[it]]], # training surrogates (L-1-Out-CV)
      surrogates$param.set, # parameter space to search through
      n.defaults = 10, # Number of defaults we want to find
      probs = "cycle") # Quantile we want to optimize
    return(defs)
  }
  # Save found defaults as RDS
  saveRDS(list("defaults" = defs), defs.file)
}

#-------------------------------------------------------------------------------------------------
# Evaluate found defaults on OOB-Tasks on OpenML
defs = readRDS(defs.file)
n.defs = c(1, 2, 4, 6, 8, 10)
def.res = foreach(it = seq_len(rin$desc$iters)) %:%
  foreach(n = n.defs) %dopar% {
    evalDefaultsOpenML(
      task.ids = names(surrogates$surrogates[rin$test.inds[[it]]]),
      lrn = makeLearner(gsub(x = learner.names[i], "mlr.", "", fixed = TRUE)),
      defaults = defs$defaults,
      ps = surrogates$param.set,
      it = it,
      n = n,
      aggr.fun = defs$aggr.fun)
  }

# Evaluate random search on OOB-Tasks on OpenML
n.rs   = c(4, 8, 16, 32, 64)
rs.res = foreach(it = seq_len(rin$desc$iters)) %:%
  foreach(n = n.rs) %dopar% {
    evalRandomSearchOpenML(
      task.ids = names(surrogates$surrogates[rin$test.inds[[it]]]),
      lrn = makeLearner(gsub(x = learner.names[i], "mlr.", "", fixed = TRUE)),
      defaults = defs$defaults,
      ps = surrogates$param.set,
      it = it,
      n = n)
  }

# Evaluate Package Defaults on OOB-Tasks on OpenML
pd.res = foreach(it = seq_len(rin$desc$iters)) %dopar%
  evalPackageDefaultOpenML(
    task.ids = names(surrogates$surrogates[rin$test.inds[[it]]]),
    lrn = makeLearner(gsub(x = learner.names[i], "mlr.", "", fixed = TRUE)),
    defaults = defs$defaults,
    ps = surrogates$param.set,
    it = it,
    n = 1, overwrite = TRUE)

# Evaluate MBO on OOB-Tasks on OpenML
mbo.res = foreach(it = seq_len(rin$desc$iters)) %dopar%
  evalMBOOpenML(
    task.ids = names(surrogates$surrogates[rin$test.inds[[it]]]),
    lrn = makeLearner(gsub(x = learner.names[i], "mlr.", "", fixed = TRUE)),
    defaults = defs$defaults,
    ps = surrogates$param.set,
    it = it,
    n = 10, overwrite = TRUE)

# Evaluate against random search on Surrogates (mean over 100 reps)
set.seed(1999 + i)
# This requires loaded RandomBot Data
rb.res = evalRandomBotData(measure = mlr::auc, i, n.rs = c(4, 8, 16, 32, 64), reps = 100) 

stopImplicitCluster()
saveRDS(list("oob.perf" = oml.res), stringBuilder("defaultLOOCV", "Q2_perf", learner.names[i]))
gc();


#--------------------------------------------------------------------------------------------------
# Evaluate found defaults on complete holdout datasets,
# for which surrogates are not even available

omlds = listOMLTasks(
  task.type = "Supervised Classification",
  estimation.procedure = "10-fold Crossvalidation",
  number.of.instances = c(1, 10^5 - 1),
  number.of.features = c(1, 1000),
  number.of.missing.values = 0,
  number.of.classes = 2) %>%
  filter(!(data.id %in% read.csv("oml_data_task.txt", sep = " ")$data.id)) %>%
  filter(status == "active") %>%
  group_by(data.id) %>%
  filter(row_number(data.id) == 1) %>%
  arrange(number.of.features * sqrt(number.of.instances))

# Tasks "1220" and "4135" are listed in the paper but not in the surrogates
n.defs = c(2, 4, 6, 8, 10)[1:3]
hout.res = foreach(it = seq_len(rin$desc$iters)) %:%
  foreach(n = n.defs) %dopar% {
    evalDefaultsOpenML(
      task.ids = c("1220", "4135"),
      lrn = makeLearner(gsub(x = learner.names[i], "mlr.", "", fixed = TRUE)),
      defaults = defs$defaults,
      ps = surrogates$param.set,
      it = it,
      n = n)
  }
oml.res = do.call("bind_rows", hout.res)
saveRDS(oml.res, "defaultLOOCV/HOUT_Q2_cubist_classif.rpart_auc_zscale_.RDS")