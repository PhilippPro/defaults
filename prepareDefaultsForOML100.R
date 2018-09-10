library(devtools)
library(farff)
library(dplyr)
library(mlr)
library(foreach)
library(focussearch)  # Search the surrogates
library(doMC)         # Parallelization
load_all()

# Surrogate model: Cubist
source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
surrogate.lrn = makeLearner("regr.cubist", committees = 20, extrapolation = 20)

# Parallelization: 27 Cores
registerDoMC(27)


#----------------------------------------------------------------------------------
# Train Surrogates
surrogates = foreach(sklearner = c("adaboost", "random_forest", "libsvm_svc")) %do% {

    # Possibly a better solution for the scaling:
    # mutate(y = 1 - y) %>%
    # mutate(y = (y - mean(y)) / sd(y))

  # Invert as we minimize our measure, scale to mean = 0, sd = 1
  dflst = readARFF(paste0("sklearn_oml100/", sklearner, ".arff")) %>%
    group_by(task_id) %>%
    mutate(y = ((- y - mean(- y)) / max(sd(- y), 10^-12))) %>%
    ungroup() %>%
    data.frame() %>%
    split(x = . , f = .$task_id)

  sklst = foreach( i = seq_len(length(dflst))) %dopar% {
    tsk = dflst[[i]] %>%
      preprocess_omldata(sklearner) %>%
      makeRegrTask(id = paste0(sklearner, "_", names(dflst)[i]), data = ., target = "y") %>%
      removeConstantFeatures()
    mod = train(surrogate.lrn, tsk)
  }
  sklst
}
names(surrogates) = c("adaboost", "random_forest", "libsvm_svc")
saveRDS(surrogates, "sklearn_oml100/surrogates.RDS")



#----------------------------------------------------------------------------------
# Learn Defaults
surrogates = readRDS("sklearn_oml100/surrogates.RDS")
param.set = getSkLearnParamsets()


sklearner = "libsvm_svc"
sklearner = "adaboost"
sklearner = "random_forest"

# Defaults
defs.file = paste0("sklearn_oml100/defaults", "_defaults_", sklearner, ".RDS")
# Compute defaults if not yet available
if (!file.exists(defs.file)) {
  # Iterate over ResampleInstance and its indices
  defs = foreach(it = seq_len(length(surrogates[[sklearner]]))) %dopar% {
    set.seed(199 + it)
    # Search for defaults
    defs = searchDefaultsOML100(
      surrogates[[sklearner]][-it], # training surrogates (L-1-Out-CV)
      param.set[[sklearner]], # parameter space to search through
      n.defaults = 32)
    return(defs)
    catf("Finished iteration %i!", it)
  }
  # Save found defaults as RDS
  saveRDS(list("defaults" = defs), defs.file)
}
# Add some info and write to an .arff file
df = do.call("bind_rows", readRDS(defs.file)$defaults)
df$task_id = readARFF(paste0("sklearn_oml100/", sklearner, ".arff")) %>%
  pull(task_id) %>% unique() %>% rep(each = 32)
df$default_no = rep(seq_len(32), nrow(df) / 32)

# Cubist has issues with *sample* in feature names
if(sklearner == "random_forest")
  df = df %>% rename(min_samples_leaf = min_leaf, min_samples_split = min_split)
writeARFF(df,
  paste0("sklearn_oml100/defaults", "_defaults_", sklearner, ".arff"),
  overwrite = TRUE)




# ------------------------------------------------------------------------------------
# Compute surrogates on the full data
load_all()
surrogates = readRDS("sklearn_oml100/surrogates.RDS")
param.set = getSkLearnParamsets()
sklearners = c("libsvm_svc", "adaboost", "random_forest")

registerDoMC(3)
# Compute defaults if not yet available
    # Iterate over ResampleInstance and its indices
    defs = foreach(sklearner = sklearners) %dopar% {
      defs.file = paste0("full_defaults/median_defaults_sklearn_", sklearner, ".RDS")
      set.seed(3999)
      # Search for defaults
      defs = searchDefaultsOML100(
        surrogates[[sklearner]], # training surrogates (L-1-Out-CV)
        param.set[[sklearner]], # parameter space to search through
        n.defaults = 32)
      saveRDS(list("defaults" = defs), defs.file)
      return(defs)
    }


defs.files = paste0("full_defaults/median_defaults_sklearn_", sklearner, ".RDS")
sapply(defs.files, function(x) {farff::writeARFF(readRDS(x)$defaults,
  paste0(stringi::stri_sub(x, to = -5), ".arff"))})

