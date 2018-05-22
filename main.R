library(devtools)
library(stringi)
library(focussearch)
load_all()

# Get file from the figshare repository
load(url("https://ndownloader.figshare.com/files/10462297"))
data.ids = sort(unique(tbl.results$data_id))

# Learner parsets and names
lrn.par.set = getMultipleLearners()
learner.names = stri_sub(stri_paste("mlr.", names(lrn.par.set)), 1, -5)
# Possible measures
measures = list(auc, acc, brier)
# Possible scalings
scaling = c("none", "logit", "zscale", "scale01")

# Learn the surrogate models
# surrogate.mlr.lrn = makeLearner("regr.ranger",
#   # We use the defautls from Philips paper.
#   par.vals = list(num.trees = 2000, respect.unordered.factors = "order", num.threads = 32,
#                   replace = FALSE, sample.fraction = 0.751))
# k = 1 # auc
# scaling = "none"
# for(i in seq_along(learner.names)) {
#   sprintf("Learner %i: %s", i, learner.names[i])
#   set.seed(199 + i)
#   # Surrogate model calculation
#   surrogates = makeSurrogateModels(measure = measures[[k]], learner.name = learner.names[i], 
#     data.ids = data.ids, tbl.results, tbl.metaFeatures, tbl.hypPars, lrn.par.set, surrogate.mlr.lrn,
#     scale_before = TRUE, scaling = scaling)
#   saveRDS(surrogates, file = paste0("surrogates/", stri_sub(learner.names[i], from = 5), measures[k]$id, "_scale_", scaling, ".RDS"))
#   gc()
# }


# Forward selection
defaults = setNames(as.list(numeric(length(learner.names))), stri_sub(learner.names, 13, 100))
files = list.files("surrogates")
for(i in seq_along(learner.names)) {
  catf("Learner: %s", learner.names[i])
  set.seed(199 + i)
  # Read surrogates from Hard Drive
  surrogates = readRDS(stri_paste("surrogates/", files[grep(stri_sub(learner.names[i], from = 5), x = files)]))
  # Default calculation
  defaults[[i]] = defaultCV(surrogates, n.defaults = 20, probs = 0.5)
  saveRDS(defaults, stri_paste("defaultLOOCV/", files[grep(stri_sub(learner.names[i], from = 5), x = files)]))
  gc()
}


for(i in seq_along(learner.names)) {
  for(j in 1:ncol(defaults[[i]]$default)) {
    if(is.factor(defaults[[i]]$default[,j]))
      defaults[[i]]$default[,j] = as.character(defaults[[i]]$default[,j])
    defaults[[i]]$default[,j][defaults[[i]]$default[,j] == -11] = NA 
  }
}

defaults$ranger$default$replace = as.logical(defaults$ranger$default$replace)
levels(defaults$ranger$default$respect.unordered.factors) = c("ignore", "order")
defaults$ranger$default$respect.unordered.factors = as.character(defaults$ranger$default$respect.unordered.factors)

save(defaults, file = "defaults.RData")

load("defaults.RData")



# Performance of first default
for(i in 1:6)
  print(c(stri_sub(learner.names[i], 13, 30), round(mean(defaults[[i]]$result[1,]), 4)))

# Performance of first 10 defaults
for(i in 1:6)
  print(c(stri_sub(learner.names[i], 13, 30), round(mean(apply(defaults[[i]]$result, 2, max)), 4)))


