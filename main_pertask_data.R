library(devtools)
library(stringi)
library(focussearch)
library(doParallel)
library(foreach)
load_all()

# Compute surrogates ------------------------------------------------------------------------------
registerDoParallel(16)
trainSaveSurrogates_pertask(surrogate.mlr.lrn)
stopImplicitCluster()


# Compute defaults ------------------------------------------------------------------------------
files = list.files("surrogates", full.names = TRUE)[grep(x = list.files("surrogates"), "pertask")]
for(i in seq_len(length(files))) {
  set.seed(199 + i)
  # Read surrogates from Hard Drive
  surrogates = readRDS(files[i])
  ps = makePertastkParamSet()[[i]]
  # Create a resample instance from mlr
  rin = makeResampleInstance(makeResampleDesc("CV", iters = 24), size = 100)
  
  # Iterate over it and its indices
  res = foreach(it = seq_len(rin$desc$iters)) %dopar% {
    # Search for defaults
    defs = searchDefaults(surrogates[rin$train.inds[[it]]], ps, n.defaults = 10, probs = 0.5)
    # Get performance on train and test data
    prds = getDefaultPerfs(surrogates, defs, train.inds = rin$train.inds[[it]])
    list("preds" = prds, "params" = defs)
  }

  saveRDS(
    res,
    stri_paste("defaultLOOCV/Q2", gsub("regr.", "", stri_sub(files[i], from = 13, to = -6)))
    )
  gc()
}

# Values via random search -------------------------------------------------------------------------
files = list.files("surrogates", full.names = TRUE)[grep(x = list.files("surrogates"), "pertask")]
for(i in seq_len(length(defaults))) {
  set.seed(199 + i)
  # Read surrogates from Hard Drive
  surrogates = readRDS(files[i])
  ps = makePertastkParamSet()[[i]]
  # Create a resample instance from mlr
  rin = makeResampleInstance(makeResampleDesc("CV", iters = 24), size = 100)
  
  # Iterate over it and its indices
  res = foreach(it = seq_len(rin$desc$iters)) %dopar% {
    # Search for defaults
    randomSearch(surrogates, ps, 1, 1)
    # Get performance on train and test data
    prds = getDefaultPerfs(surrogates, defs, train.inds = rin$train.inds[[it]])
    list("preds" = prds, "params" = defs)
  }
  
  saveRDS(res, stri_paste("defaultLOOCV/Q2", gsub("regr.", "", stri_sub(files, from = 13, to = -6)))[i])
}
defaults = list.files("defaultLOOCV", full.names = TRUE)
defaults = defaults[grep(x = defaults , "pertask")]
defs = readRDS(defaults[1])

extractSubList(defs, "params", simplify = FALSE)
extractSubList(defs, "preds", simplify = FALSE)
