library(devtools)
library(stringi)
library(focussearch)
library(doParallel)
library(foreach)
load_all()

# ----------------------------------------------------------------------------------
# Get randomBot Data from the figshare repository
# load(url("https://ndownloader.figshare.com/files/10462297"))


# ----------------------------------------------------------------------------------
# Train/Save the surrogates
lrn.par.sets = getLearnerParSets()
learner.names = stri_sub(stri_paste("mlr.", names(lrn.par.sets)), 1, -5)

# Use cubist as a learner
surrogate.mlr.lrn = makeLearner("regr.cubist")
# registerDoParallel(8)
# train_save_surrogates(surrogate.mlr.lrn, lrn.par.sets[4], learner.names[4])
# stopImplicitCluster()

# Extract a grid from the surrogates
# parallelMap::parallelStartMulticore(parallel::detectCores())
# predictGridFromSurrogates(readRDS("surrogates/regr.cubistclassif.svmauczscale.RDS"), learner.names[4]) 
# parallelMap::parallelStop()

# ----------------------------------------------------------------------------------
# Forward selection
defaults = setNames(as.list(numeric(length(learner.names))), stri_sub(learner.names, 13, 100))
files = list.files("surrogates")[grep(x = list.files("surrogates"), surrogate.mlr.lrn$id)]

for(i in seq_along(learner.names)) {
  catf("Learner: %s", learner.names[i])
  set.seed(199 + i)
  
  # Read surrogates from Hard Drive
  surrogates = readRDS(stri_paste("surrogates/", files[grep(stri_sub(learner.names[i], from = 5), x = files)]))
  
  # Search for defaults
  train = searchDefaults(surrogates$surrogates, surrogates$param.set, n.defaults = 10, probs = 0.5)
  
  # Get performance on train and test data
  prds = getDefaultPerfs(surrogates$surrogates, lst$params)

  saveRDS(list("preds" = prds, "params" = train),
          stri_paste("defaultLOOCV/p1", gsub("regr.", "", files[grep(stri_sub(learner.names[i], from = 5), x = files)])))
  gc()
}

# ----------------------------------------------------------------------------------
# Create Plots comparing to random search
# Read in found defaults and surrogates
lst = readRDS("defaultLOOCV/p1cubistclassif.svmauczscale.RDS")
surrogates = readRDS(stri_paste("surrogates/", files[grep(stri_sub(learner.names[i], from = 5), x = files)]))



# Do the randomsearch for different multipliers
set.seed(199 + i)
ys = foreach(points = seq(from = 2, to = 10, by = 2), .combine = "rbind") %:%
  foreach(multiplier = c(1, 2, 4, 8), .combine = "rbind") %do% {
    rs = randomSearch(surrogates$surrogates, surrogates$param.set, multiplier, points)
    extractSubList(rs, "y")
  }
df = data.frame(ys, row.names = NULL) %>%
  mutate(points = rep(seq(from = 2, to = 10, by = 2), each = 4),
         multiplier = rep(paste0("x", c(1, 2, 4, 8)), times = 5))
colnames(df) = gsub("X", "", colnames(df))

# Plot function
create_plot = function(n.defaults) {
  def = gather(prds, "dataset", "y") %>%
    group_by(dataset) %>%
    filter(row_number() <= n.defaults) %>%
    summarise(y = min(y))
  rnd = df %>% filter(points == n.defaults) %>%
    gather("dataset", "y", -one_of(c("points", "multiplier"))) %>%
    select(-points) %>% spread("multiplier", "y")
  p = inner_join(def, rnd, by = "dataset") %>%
    mutate("x1" = y - x1, "x2" = y - x2, "x4" =  y - x4, "x8" = y - x8) %>%
    mutate(split = ifelse(dataset %in% train_split(), "train", "test")) %>%
    mutate(split = factor(split, levels = c("train", "test"))) %>%
    select(-y) %>%
    gather("randomsearch", "delta_y", -one_of("dataset", "split")) %>%
    mutate(randomsearch = factor(randomsearch, levels = c("x1", "x2", "x4", "x8"))) %>%
    ggplot(aes(x = randomsearch, y = delta_y)) +
    geom_boxplot() + facet_wrap(~split) + 
    ggtitle(paste0("Using ", n.defaults, " defaults"))
  ggsave(p, filename = paste0("defaultLOOCV/d", n.defaults, "svm_feather.png"))
return(p)
}

# Plot and save the plots
sapply(seq(from = 2, to = 10, by = 2), create_plot)







# for(i in seq_along(learner.names)) {
#   for(j in 1:ncol(defaults[[i]]$default)) {
#     if(is.factor(defaults[[i]]$default[,j]))
#       defaults[[i]]$default[,j] = as.character(defaults[[i]]$default[,j])
#     defaults[[i]]$default[,j][defaults[[i]]$default[,j] == -11] = NA 
#   }
# }
# defaults$ranger$default$replace = as.logical(defaults$ranger$default$replace)
# levels(defaults$ranger$default$respect.unordered.factors) = c("ignore", "order")
# defaults$ranger$default$respect.unordered.factors = as.character(defaults$ranger$default$respect.unordered.factors)
# 
# save(defaults, file = "defaults.RData")
# 
# load("defaults.RData")
# # Performance of first default
# for(i in 1:6)
#   print(c(stri_sub(learner.names[i], 13, 30), round(mean(defaults[[i]]$result[1,]), 4)))
# # Performance of first 10 defaults
# for(i in 1:6)
#   print(c(stri_sub(learner.names[i], 13, 30), round(mean(apply(defaults[[i]]$result, 2, max)), 4)))


