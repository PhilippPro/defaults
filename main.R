library(devtools)
library(stringi)
load_all()

# Get file from the figshare repository
load(url("https://ndownloader.figshare.com/files/10462297"))

lrn.par.set = getMultipleLearners()
data.ids = sort(unique(tbl.results$data_id))
learner.names = stri_sub(stri_paste("mlr.", names(lrn.par.set)), 1, -5)
measures = list(auc, acc, brier)

surrogate.mlr.lrn = makeLearner("regr.ranger",
  par.vals = list(num.trees = 2000, respect.unordered.factors = "order", num.threads = 4))

k = 1 # auc
scaling = "none"
for(i in seq_along(learner.names)) {
  sprintf("Learner %i: %s", i, learner.names[i])
  set.seed(199 + i)
  # Surrogate model calculation
  surrogates = makeSurrogateModels(measure = measures[[k]], learner.name = learner.names[i], 
    data.ids = data.ids, tbl.results, tbl.metaFeatures, tbl.hypPars, lrn.par.set, surrogate.mlr.lrn,
    scale_before = TRUE, scaling = scaling)
  saveRDS(surrogates, file = paste0("surrogates/", stri_sub(learner.names[i], from = 5), measures[k]$id, "_scale_", scaling, ".RDS"))
}


# Forward selection
defaults = list()

for(i in seq_along(learner.names)) {
  print(i)
  set.seed(199 + i)
  load(paste0("surrogates_", measures[k], "_", i, ".RData"))
  # Default calculation
  defaults[[length(defaults) + 1]] = calculateDefaultForward(surrogates, n.points = 100000, n.default = 10)
}
names(defaults) = stri_sub(learner.names, 13, 100)

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


