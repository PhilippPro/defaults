library(devtools)
library(stringi)
library(focussearch)
library(doParallel)
library(foreach)
load_all()

# Get randomBot Data from the figshare repository-------------------------
# load(url("https://ndownloader.figshare.com/files/10462297"))


# Train/Save the surrogates ------------------------------------------------------
lrn.par.sets = getLearnerParSets()
learner.names = stri_sub(stri_paste("mlr.", names(lrn.par.sets)), 1, -5)

# Use cubist as a learner
# 20 Comittees in order to get a lower mse (25% better then 1 comittee) 
# and set extrapolation to 20 in order to not extrapolate to no-data areas to much.
# Obtained a fixed version of cubist learner from github branch.
source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
surrogate.mlr.lrn = makeLearner("regr.cubist", committees = 20, extrapolation = 20)

registerDoParallel(19)
trainSaveSurrogates(surrogate.mlr.lrn, lrn.par.sets, learner.names)
stopImplicitCluster()

# Extract a grid from the surrogates
# foreach(i = 4) %do% { # seq_along(learner.names)
#   parallelMap::parallelStartMulticore(parallel::detectCores())
#   predictGridFromSurrogates(readRDS("surrogates/regr.cubistclassif.svmauczscale.RDS"), learner.names[i]) 
#   parallelMap::parallelStop()
# }


# Forward selection ----------------------------------------------------------------------------------
files = list.files("surrogates")[grep(x = list.files("surrogates"), "regr.cubist_classif")]
for(i in c(2)) { # seq_along(learner.names)
  catf("Learner: %s", learner.names[i])
  set.seed(199 + i)
  
  # Read surrogates from Hard Drive
  surrogates = readRDS(stri_paste("surrogates/", files[grep(stri_sub(learner.names[i], from = 5), x = files)]))
  
  # Search for defaults
  rin = makeResampleInstance(makeResampleDesc("CV", iters = 19), size = 38)
  
  registerDoParallel(30)
  
  # Iterate over ResampleInstance and its indices
  defs = foreach(it = seq_len(rin$desc$iters)) %dopar% {
    # Search for defaults
    defs = searchDefaults(surrogates$surrogates[rin$train.inds[[it]]], surrogates$param.set,
      n.defaults = 10, probs = 0.5)
    return(defs)
  }
  
  # Evaluate found defaults on OpenML
  n.defs = 2 # c(2, 4, 6, 8, 10)
  res = foreach(it = seq_len(rin$desc$iters)) %:%
    foreach(n = n.defs) %dopar% {
      evalDefaultsOpenML(
        task.ids = names(surrogates$surrogates[rin$test.inds[[it]]]),
        lrn = makeLearner(gsub(x = learner.names[i], "mlr.", "", fixed = TRUE)),
        defaults = defs[[it]],
        ps = surrogates$param.set,
        n = n)
    }
  oml.res = do.call("bind_rows", res)
  
  stopImplicitCluster()
  
  saveRDS(list("oob.perf" = oml.res, "defaults" = defs),
          stri_paste("defaultLOOCV/Q2",
            gsub("regr.", "", files[grep(stri_sub(learner.names[i], from = 5), x = files)])))
  gc()
}



# Create Plots comparing to random search ------------------------------------------------------------
# Read in found defaults and surrogates
lst = readRDS("defaultLOOCV/MEAN_cubist_classif.svm_auc_zscale_.RDS")
i = 4
surrogates = readRDS(stri_paste("surrogates/", files[grep(stri_sub(learner.names[i], from = 5), x = files)]))

# Get data from randomsearch:
ys = foreach(points = seq(from = 2, to = 10, by = 2), .combine = "rbind") %:%
  foreach(multiplier = c(1, 2, 4, 8), .combine = "rbind") %do% {
    randomSearch(surrogates$surrogates, surrogates$param.set, multiplier, points)
  }
df = data.frame(ys, row.names = NULL) %>%
  mutate(points = rep(seq(from = 2, to = 10, by = 2), each = 4),
         multiplier = rep(paste0("x", c(1, 2, 4, 8)), times = 5))
colnames(df) = gsub("X", "", colnames(df))

# Plot function
create_plot = function(lst, df, n.defaults, algorithm) {
  def = gather(lst$preds, "dataset", "y") %>%
    separate(dataset, into = c("dataset", "split")) %>%
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
    stat_boxplot(geom ='errorbar', width = 0.5) +
    geom_boxplot(notch = FALSE) +
    facet_wrap(~split) + 
    ggtitle(paste0("Using ", n.defaults, " defaults"))
  ggsave(p, filename = paste0("defaultLOOCV/d", n.defaults, algorithm, "Mean", ".png"))
  return(NULL)
}
# Plot and save the plots
sapply(seq(from = 2, to = 10, by = 2), create_plot, algorithm = "svm", lst = lst, df = df)

# Plot the cummulative error ------------------------------------------------------------
#pdf("defaultLOOCV/cumml_error_Q2")
# tst = prds %>% select(ends_with("test")) %>% apply(2, cummin) %>% apply(1, mean)
# trn = prds %>% select(ends_with("train")) %>% apply(2, cummin) %>% apply(1, mean)
# tst %>% plot(xlab = "nDefaults", ylab = "Avg. standardized Error", ylim = c(-1, 0.5),
#              main = "Test (black) / Train (red) datasets")
# tst %>% lines
# trn %>% points(col = "red")
# trn %>% lines(col = "red")
# dev.off()

# Eval on true test set ------------------------------------------------------------
registerDoParallel(cores = 32)
evalDefaultsOpenML(test_split("task.ids"), makeLearner("classif.svm"), lst$params)
stopImplicitCluster()

# Old Unused Code -----------------------------------------------------------------
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


