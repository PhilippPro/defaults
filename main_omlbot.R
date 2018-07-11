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

  # Read surrogates from Hard Drive
  surrogates = readRDS(stri_paste("surrogates/", files[grep(stri_sub(learner.names[i], from = 5), x = files)]))
  
  # Search for defaults
  set.seed(199 + i)
  rin = makeResampleInstance(makeResampleDesc("CV", iters = 19), size = length(surrogates$surrogates))
  
  registerDoParallel(30)
  
  # Iterate over ResampleInstance and its indices
  defs = foreach(it = seq_len(rin$desc$iters)) %dopar% {
    # Search for defaults
    defs = searchDefaults(surrogates$surrogates[rin$train.inds[[it]]], surrogates$param.set,
      n.defaults = 10, probs = 0.5)
    return(defs)
  }
  
  # Evaluate found defaults on OpenML
  n.defs = c(2, 4, 6, 8, 10)
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
lst = readRDS("defaultLOOCV/Q2_cubist_classif.rpart_auc_zscale_.RDS")

library(ggpubr)
library(patchwork)

# Boxplot of the different methods
p <- ggboxplot(
  lst$oob.perf, x = "search.type", y = "auc.test.mean", color = "search.type",
  palette =c("#00AFBB", "#E7B800", "#FC4E07", "#FC88BB"),
  add = "jitter") +
  ggtitle("Performance across all datasets")

# Boxplot comparing to default search
gdata = lst$oob.perf %>% 
  left_join(lst$oob.perf %>%
    filter(search.type == "defaults") %>%
    mutate(
      auc.def = auc.test.mean,
      acc.def = acc.test.join,
      f1.def = f1.test.mean
      ),
    by = c("task.id", "learner.id", "n.defaults")) %>%
  mutate(
    delta_auc = auc.test.mean.x - auc.def,
    delta_acc = acc.test.join.x - acc.def,
    delta_f1 = f1.test.mean.x - f1.def
    ) %>%
  filter(search.type.x != "defaults")

# Boxplot Differences
g = ggboxplot(gdata, x = "search.type.x", y = "delta_auc", color = "search.type.x",
    palette =c("#E7B800", "#FC4E07", "#FC88BB"),
    add = "jitter") +
  geom_abline(intercept = 0, slope = 0) +
  coord_cartesian(ylim = c(-0.4, 0.05)) +
  ggtitle("Performance difference to defaults")

# Create combined plot
pg = (p + facet_grid(cols = vars(n.defaults))) / (g + facet_grid(cols = vars(n.defaults)))

ggsave(pg, filename = paste0("defaultLOOCV/d", 2, unique(lst$oob.perf$learner.id), "Q2", ".png"), scale = 2)





#--------------------------------------------------------------------------------------------------
# Evaluate found defaults on complete holdout datasets,
# for which surrogates are not even available
n.defs = c(2, 4, 6, 8, 10)
hout.res = foreach(it = seq_len(rin$desc$iters)) %:%
  foreach(n = n.defs) %dopar% {
    evalDefaultsOpenML(
      task.ids = c("1220", "4135"),
      lrn = makeLearner(gsub(x = learner.names[i], "mlr.", "", fixed = TRUE)),
      defaults = lst$defaults[[it]],
      ps = surrogates$param.set,
      n = n)
  }
oml.res = do.call("bind_rows", hout.res)
saveRDS(oml.res, "defaultLOOCV/HOUT_Q2_cubist_classif.rpart_auc_zscale_.RDS")


oml.res %>%
  group_by(search.type, task.id, learner.id, n.defaults) %>%
  summarize(auc = mean(auc.test.mean),
    sd_auc = sd(auc.test.mean),
    f1 = mean(f1.test.mean)) %>%
  arrange(n.defaults)

p2 = ggplot(oml.res, aes(x = search.type, y = auc.test.mean, color = task.id)) + 
  geom_boxplot() + geom_jitter() + facet_grid(cols = vars(n.defaults)) +
  ggtitle("Results for 19 different defaults / random search strategies")


ggsave(p2, filename = paste0("defaultLOOCV/HOUT_", unique(lst$oob.perf$learner.id), "Q2", ".png"), scale = 2)

