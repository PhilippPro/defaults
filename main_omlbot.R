library(devtools)     # load_all()
library(stringi)      # string manipulation
library(focussearch)  # Search the surrogates
library(doParallel)   # Parallelization
library(doMC)         # Parallelization
library(doRNG)        # Parallel RNG
library(foreach)      # Parallelization
library(patchwork)    # Vizualisation
library(ggpubr)       # Vizualisation
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

registerDoMC(19)
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
  # Create resampling train/test splits
  rin = makeResampleInstance(makeResampleDesc("CV", iters = 38), size = length(surrogates$surrogates))
  
  registerDoMC(24)
  # ------------------------------------------------------------------------------------------------
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
  
  #-------------------------------------------------------------------------------------------------
  # Evaluate found defaults on OOB-Tasks on OpenML
  defs = readRDS(defs.file)
  n.defs = c(2, 4, 6, 8, 10)
  def.res = foreach(it = seq_len(rin$desc$iters)) %:%
    foreach(n = n.defs) %dopar% {
      evalDefaultsOpenML(
        task.ids = names(surrogates$surrogates[rin$test.inds[[it]]]),
        lrn = makeLearner(gsub(x = learner.names[i], "mlr.", "", fixed = TRUE)),
        defaults = defs$defaults,
        ps = surrogates$param.set,
        it = it,
        n = n)
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
      n = 1)
  
  # Evaluate against random search on Surrogates (mean over 100 reps)
  set.seed(1999 + i)
  # This requires loaded RandomBot Data
  rb.res = evalRandomBotData(measure = auc, i, n.rs = c(4, 8, 16, 32, 64), reps = 100) 

  stopImplicitCluster()
  saveRDS(list("oob.perf" = oml.res), stringBuilder("defaultLOOCV", "Q2_perf", learner.names[i]))
  
  gc();
}

# Create Plots comparing to random search ------------------------------------------------------------
# Get the saved performances (either partial or full result)
results.file = stringBuilder("defaultLOOCV", "Q2_perf", learner.names[i])
if (file.exists(results.file)) {
  lst = readRDS(results.file)
} else {
  partial = lapply(list.files("defaultLOOCV/save", full.names = TRUE), readRDS)
  lst = list(oob.perf = do.call("bind_rows", partial) %>%
    filter(learner.id == "classif.xgboost.dummied.tuned") %>%
    filter(task.id != "nomao")) # Nomao takes really long
}


  # Boxplot of the different methods
  p = ggboxplot(lst$oob.perf,
    x = "n", y = "auc.test.mean", color = "n",
    # palette = c("#00AFBB", "#E7B800", "#FC4E07", "#FC88BB"),
    add = "jitter") +
    ggtitle("Performance across all datasets") +
    facet_wrap(~search.type, scales = "free_x")
  
  # Table with means and medians
  lst$oob.perf %>%
    group_by(task.id) %>%
    filter(search.type != "randomBotData") %>%
    mutate(
      rnk = dense_rank(desc(auc.test.mean)),
      mn = mean(auc.test.mean),
      med = median(auc.test.mean)) %>%
    ungroup() %>%
    group_by(search.type, n) %>%
    summarise(mean.ranks = mean(rnk), mean.means = mean(mn), mean.medians = mean(med))
  
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
  
  delta = switch(measure, "auc.test.mean" = "delta_auc", "acc.test.join" = "delta_acc", "f1.test.mean" = "delta_f1")
  # Boxplot Differences
  g = ggboxplot(gdata, x = "search.type.x", y = delta, color = "search.type.x",
    palette = c("#E7B800", "#FC4E07", "#FC88BB"),
    add = "jitter") +
    geom_abline(intercept = 0, slope = 0) +
    coord_cartesian(ylim = c(-0.4, max(gdata[delta]))) +
    ggtitle(stri_paste("Performance difference to defaults for ", measure))
  
  # Create combined plot
  pg = (p + facet_grid(cols = vars(n.defaults))) / (g + facet_grid(cols = vars(n.defaults)))
  ggsave(pg, filename = paste0("defaultLOOCV/", measure, unique(lst$oob.perf$learner.id), "Q2", ".png"), scale = 3)
}




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




p2 = ggplot(oml.res, aes(x = search.type, y = auc.test.mean, color = task.id)) + 
  geom_boxplot() + geom_jitter() + facet_grid(cols = vars(n.defaults)) +
  ggtitle("Results for 19 different defaults / 19 iters of random search strategies")
ggsave(p2, filename = paste0("defaultLOOCV/HOUT_", unique(lst$oob.perf$learner.id), "Q2", ".png"), scale = 2)

p2f1 = ggplot(oml.res, aes(x = search.type, y = f1.test.mean, color = task.id)) + 
  geom_boxplot() + geom_jitter() + facet_grid(cols = vars(n.defaults), rows = vars(task.id), scales = "free_y") +
  ggtitle("Results for 19 different defaults / 19 iters of random search strategies")
ggsave(p2f1, filename = paste0("defaultLOOCV/HOUT_f1", unique(lst$oob.perf$learner.id), "Q2", ".png"), scale = 2)

p2acc = ggplot(oml.res, aes(x = search.type, y = acc.test.join, color = task.id)) + 
  geom_boxplot() + geom_jitter() + facet_grid(cols = vars(n.defaults), rows = vars(task.id), scales = "free_y") +
  ggtitle("Results for 19 different defaults / 19 iters of random search strategies")
ggsave(p2acc, filename = paste0("defaultLOOCV/HOUT_acc", unique(lst$oob.perf$learner.id), "Q2", ".png"), scale = 2)


#---------------------------------------------------------------------------------------------------
# Eval defaults using tsne
names(lst$defaults) = seq_len(length(lst$defaults))
defdf = do.call("bind_rows", list(lst$defaults, .id = "iter"))
library(Rtsne)
rtsne = Rtsne(defdf[, -1], theta = 0.1)
pdf = as.data.frame(rtsne$Y)
pdf$n = as.factor(rep(1:10, 19))
pdf$iter = as.factor(defdf$iter)
ggplot(pdf, aes(x = V1, y = V2)) + geom_density2d() + facet_wrap(~n)
ggplot(pdf, aes(x = V1, y = V2, color = iter)) + geom_point() + facet_wrap(~n)
ggplot(pdf, aes(x = V1, y = V2, color = n)) + geom_point() + facet_wrap(~iter)

library(ggbiplot)
defdfpca <- prcomp(defdf[, -1] %>% filter(pdf$n %in% 1:4), scale. = TRUE)

ggbiplot(defdfpca, choices = 1:2, obs.scale = 1, var.scale = 1,
  groups = pdf$n[pdf$n %in% 1:4], ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

defdf$n = as.factor(rep(1:10, 19))
ggplot(defdf %>% filter(n %in% 1:4, iter %in% 1:5), aes(x = cp, y = minsplit, color = n, shape = iter)) + geom_point(size = 4)
