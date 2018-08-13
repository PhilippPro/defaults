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
i = 2
catf("Learner: %s", learner.names[i])
set.seed(199 + i)

# Read surrogates from Hard Drive
surrogates = readRDS(stri_paste("surrogates/", files[grep(stri_sub(learner.names[i], from = 5), x = files)]))
# Create resampling train/test splits
rin = makeResampleInstance(makeResampleDesc("CV", iters = 38), size = length(surrogates$surrogates))


registerDoMC(20)
defs.file = stringBuilder("defaultLOOCV", "cycle_defaults", learner.names[i])
# ------------------------------------------------------------------------------------------------
# Defaults
# Compute defaults if not yet available
# probs : 0.5; mean, cycle
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
n.defs = c(2, 4, 6, 8, 10)
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

# Evaluate against random search on Surrogates (mean over 100 reps)
set.seed(1999 + i)
# This requires loaded RandomBot Data
rb.res = evalRandomBotData(measure = mlr::auc, i, n.rs = c(4, 8, 16, 32, 64), reps = 100) 

stopImplicitCluster()
saveRDS(list("oob.perf" = oml.res), stringBuilder("defaultLOOCV", "Q2_perf", learner.names[i]))

gc();




# Create Plots comparing to random search ------------------------------------------------------------

get_results_from_folder = function(i) {
  # Get the saved performances (either partial or full result)
  learner = stri_sub(str = learner.names[i], from = 13)
  files = list.files("defaultLOOCV/save", full.names = TRUE)
  files = files[stri_detect_fixed(files , learner)]
  list(oob.perf = do.call("bind_rows", lapply(files, readRDS)) %>%
      filter(stri_detect_fixed(learner.id , learner)) %>%
      filter(!(task.id %in% c("nomao", "Bioresponse")))) # nomao | Bioresponse take really long
}

preprocess_results = function(lst) {
  lst$oob.perf %>%
    filter(search.type != "randomBotData") %>%
    group_by(task.id) %>%
    mutate(
      rnk = min_rank(desc(auc.test.mean)), 
      auc.test.normalized = (auc.test.mean - min(auc.test.mean)) / (max(auc.test.mean) - min(auc.test.mean)))  %>%
    ungroup() %>%
    group_by(search.type, n) %>%
    summarise(
      mean_rank_auc = mean(rnk),
      mean_auc = mean(auc.test.mean),
      mn_auc_norm. = mean(auc.test.normalized),
      median_auc = median(auc.test.mean),
      cnt = n(),
      cnt_na = sum(is.na(auc.test.mean)))
}

compare_to_pkg_defaults = function(lst, meas = "auc.test.mean") {
  # Boxplot comparing to package default
  gdata = lst$oob.perf %>% 
    filter(search.type %in% c("design", "package-defaults", "random")) %>%
    filter(search.type != "randomBotData") %>%
    left_join(lst$oob.perf %>%
        filter(search.type == "package-default") %>%
        mutate(
          auc.def = auc.test.mean,
          acc.def = acc.test.join,
          f1.def = f1.test.mean
        ),
      by = c("task.id", "learner.id")) %>%
    mutate(
      delta_auc = auc.test.mean.x - auc.def,
      delta_acc = acc.test.join.x - acc.def,
      delta_f1 = f1.test.mean.x - f1.def
    ) %>%
    filter(search.type.x != "package-default") %>%
    mutate(n = as.factor(n.x))
  
  delta = switch(meas, "auc.test.mean" = "delta_auc", "acc.test.join" = "delta_acc", "f1.test.mean" = "delta_f1")
  
  # Boxplot Differences
  ggboxplot(gdata, x = "search.type.x", y = delta, color = "n") +
    geom_abline(intercept = 0, slope = 0) +
    ggtitle(stri_paste("Performance difference to defaults for ", "auc.test.mean")) +
    ylab("Improvement over package defaults") +
    xlab("Search.type")
}

compare_to_nfold_plot = function(lst, meas = "auc.test.mean") {
  # Boxplot comparing to package default
  gdata = lst$oob.perf %>%
    filter(search.type == "design") %>%
    left_join(lst$oob.perf %>%
        filter(search.type == "random") %>%
        mutate(
          auc.def = auc.test.mean,
          acc.def = acc.test.join,
          f1.def = f1.test.mean
        ),
      by = c("task.id", "learner.id")) %>%
    filter(n.x %in% c(2, 4, 8)) %>%
    mutate(
      delta_auc = auc.test.mean.x - auc.def,
      delta_acc = acc.test.join.x - acc.def,
      delta_f1 = f1.test.mean.x - f1.def,
      multiple = n.y / n.x) %>%
    filter(multiple %in% c(1, 2, 4, 8)) %>%
    mutate(n = as.factor(n.x))
  
  delta = switch(meas, "auc.test.mean" = "delta_auc", "acc.test.join" = "delta_acc", "f1.test.mean" = "delta_f1")
  
  # Boxplot Differences
  ggboxplot(gdata, x = "search.type.x", y = delta, color = "multiple") +
    geom_abline(intercept = 0, slope = 0) +
    facet_grid(~ n) + 
    ggtitle(stri_paste("Performance difference to defaults for ", "auc.test.mean")) +
    ylab("Improvement over randomSearch") +
    xlab("Search.type")
}

save_plots_to_file = function(p, g, rp, r, i) {
  # Create combined plot
  ggsave(
    plot = (p / g),
    filename = paste0("defaultLOOCV/", "auc.test.mean", learner.names[i], "Q2", ".png"),
    scale = 3)
  ggsave(
    plot = (rp / r),
    filename = paste0("defaultLOOCV/", "auc.test.mean_randomBot", learner.names[i], "Q2", ".png"),
    scale = 3)
}

make_rankplot = function(data) {
  data_rp = tidyr::gather(data %>% select(-cnt, -cnt_na), "measure", "value", -c("n", "search.type"))
  ggplot(data_rp, aes(x = as.factor(n), y = value, color = search.type)) +
    geom_point() +
    ylab("Average rank") + xlab("No. defaults") +
    facet_wrap(~measure, scales = "free")
}

create_report = function(i) {
  rmarkdown::render(
    knitr_in("report_tmpl.Rmd"),
    output_file = file_out(readd(target.file)),
    quiet = TRUE)
}

make_tsneplot = function(i) {
  # Eval defaults using tsne
  defs.file = stringBuilder("defaultLOOCV", "Q2_defaults", learner.names[i])
  defs = readRDS(defs.file)
  
  library(Rtsne)
  set.seed(2999 + i)
  defdf = do.call("bind_rows", list(defs$defaults, .id = "iter"))

  nums = sapply(defdf, is.numeric)
  # Add some noise to avoid duplicates
  if (sum(nums) > 0) {
    for(i in sum(nums)) {
      defdf[, nums][, i] = defdf[, nums][, i] + rnorm(nrow(defdf), 0, 10^-8)
    }
  }
  
  defdf = defdf[!duplicated(defdf), ]
  rtsne = Rtsne(defdf[, -1], theta = 0.1)
  
  pdf = as.data.frame(rtsne$Y)
  pdf$n = as.factor(rep(1:10, 19))
  pdf$iter = as.factor(defdf$iter)
  # p1 = ggplot(pdf, aes(x = V1, y = V2)) + geom_density2d() + facet_wrap(~n)
  p2 = ggplot(pdf, aes(x = V1, y = V2, color = iter), alpha = 0.7) + geom_point() + facet_wrap(~n) + geom_jitter()
  return(p2)
}


plans = vector("list", 4)

for (i__ in c(1, 2, 4, 6)) {
  
  plans[[i__]] = drake_plan(
    results = get_results_from_folder(i__),
    
    data = preprocess_results(results) %>%
      filter(search.type %in% c("design", "package-defaults", "random")),
    
    data_aggfun = preprocess_results(results) %>%
      filter(search.type %in% c("design", "defaults_mean", "defaults_cycle")),
    
    # Boxplot of the different methods
    p = results[["oob.perf"]] %>%
      filter(search.type != "randomBotData") %>%
      filter(search.type %in% c("design", "package-defaults", "random")) %>%
      ggboxplot(., x = "n", y = "auc.test.mean", color = "n", add = "jitter") +
      ggtitle("Performance across all datasets") +
      facet_wrap(~search.type, scales = "free_x"),
    
    # Boxplot of randomBotData
    r = results[["oob.perf"]] %>%
      filter(search.type == "randomBotData") %>%
      ggboxplot(., x = "n", y = "auc.test.mean", color = "n", add = "jitter") +
      ggtitle("Performance across all datasets") +
      facet_wrap(~search.type, scales = "free_x"),
    
    # Comparison to package defaults
    g = compare_to_pkg_defaults(results), 
    
    # Comparison to n-fold random search
    nfold = compare_to_nfold_plot(results),
    
    # Plot ranks and mean of means etc.
    rankplot = make_rankplot(data),
    
    tsneplot = make_tsneplot(i__),
    
    learner.name = paste0(learner.names[i__]), 
    
    target.file = paste0("report_", learner.names[i__], ".html"),
    
    report = create_report(i__),
    
    strings_in_dots = "literals"
  )
}

for (i__ in c(1, 2, 4, 6)) {
  make(plans[[i__]])
}





# How long do I need to search to beat defaults
perfs = lst$oob.perf %>% filter(search.type == "random") %>%
  left_join(lst$oob.perf %>% filter(search.type == "design"), suffix = c(".random", ".default"), by = c("task.id")) %>%
  group_by(task.id) %>%
  mutate(delta_auc = auc.test.mean.random - auc.test.mean.default) %>%
  filter(n.default <= n.random) %>% 
  select(task.id, delta_auc, auc.test.mean.random, auc.test.mean.default, n.random, n.default) %>%
  mutate(x.random = n.random / n.default) %>%
  group_by(n.default, x.random) %>%
  summarize(
    mean.difference.auc = mean(delta_auc),
    defs.better = sum(delta_auc < 0),
    tie = sum(delta_auc == 0),
    rnd.better = sum(delta_auc > 0))

perfs %>% print(n = 21)

ggplot(perfs, aes(x = x.random, y = defs.better / (defs.better + rnd.better), color = as.factor(n.default))) +
  geom_point() + geom_line()
ggplot(perfs, aes(x = x.random, y = mean.difference.auc, color = as.factor(n.default))) +
  geom_point() + geom_line()
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



lst$oob.perf %>%
  filter(search.type != "randomBotData") %>%
  group_by(task.id) %>%
  mutate(rnk = min_rank(desc(auc.test.mean)),
    auc.test.normalized = (auc.test.mean - min(auc.test.mean)) / (max(auc.test.mean) - min(auc.test.mean))) %>%
  select(task.id, rnk, auc.test.mean, auc.test.normalized, search.type, n) %>%
  arrange(task.id, rnk) %>% print(n = 10^3)
