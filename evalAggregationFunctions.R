library(devtools)     # load_all()
library(stringi)      # string manipulation
library(focussearch)  # Search the surrogates
library(doParallel)   # Parallelization
library(doMC)         # Parallelization
library(doRNG)        # Parallel RNG
library(foreach)      # Parallelization
library(mlr)
load_all()

# Load neccessary files
lrn.par.sets = getLearnerParSets()
learner.names = stri_sub(stri_paste("mlr.", names(lrn.par.sets)), 1, -5)
source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
surrogate.mlr.lrn = makeLearner("regr.cubist", committees = 20, extrapolation = 20)
files = list.files("surrogates")[grep(x = list.files("surrogates"), "regr.cubist_classif")]


fs.configs = data.frame(
  iters = c(10^4, 10^3, 1.11 * 10^3),
  depth = c(1, 2, 3),
  reps = c(1, 5, 3)
)

registerDoParallel(30)
# registerDoSEQ()

foreach(i = seq_len(6)[-5][-3]) %:%
  foreach(aggrFun = c("mean", "hodges-lehmann", "design")) %:% # "avg.quantiles357", "avg.quantiles05595"
    foreach(fs.config = seq_len(2)) %do% {


    fs.cfg.string = paste0(fs.configs[fs.config, ], collapse = "_")
    res.file = stringBuilder("evalAggrFuns/results", aggrFun, learner.names[i], fs.cfg.string)

    if (!file.exists(res.file))  {

      catf("Learner: %s", learner.names[i])
      set.seed(199 + i)

      # Read surrogates from Hard Drive
      surrogates = readRDS(stri_paste("surrogates/", files[grep(stri_sub(learner.names[i], from = 5), x = files)]))

      # Defaults
      defs.file = stringBuilder("evalAggrFuns/defaults", aggrFun, learner.names[i], fs.cfg.string)[1]
      n_datasets = length(surrogates$surrogates)
      # Compute defaults if not yet available
      if (!file.exists(defs.file)) {
        # Iterate over ResampleInstance and its indices
        defs = foreach(it = seq_len(n_datasets), .export = "surrogates") %do% {
          if(aggrFun == "design") aggrFun = 0.5 # Design means optimize the median
          set.seed(199 + i)
          # Search for defaults
          defs = searchDefaults(
            surrogates$surrogates[-it], # training surrogates (L-1-Out-CV)
            surrogates$param.set, # parameter space to search through
            n.defaults = 10, # Number of defaults we want to find
            probs = aggrFun, # AggrFun we want to optimize
            fs.config = fs.configs[fs.config, ]
          )
          return(defs)
        }
        # Save found defaults as RDS
        saveRDS(list("defaults" = defs), defs.file)
        defs = list("defaults" = defs)
      } else {
        defs = readRDS(defs.file)
      }


      defs = readRDS(defs.file)

      n.defs = c(1, 2, 4, 6, 8, 10)
      def.res.sur = foreach(it = seq_len(n_datasets), .combine = "bind_rows", .export = "surrogates") %:%
        foreach(n = n.defs, .combine = "bind_rows", .export = "surrogates") %dopar% {
          evalDefaultsSurrogates(
            task.ids = names(surrogates$surrogates[it]),
            lrn = makeLearner(gsub(x = learner.names[i], "mlr.", "", fixed = TRUE)),
            defaults = defs$defaults,
            ps = surrogates$param.set,
            it = it,
            n = n)
        }
      def.res.sur$aggrFun = aggrFun
      def.res.sur$cfg = fs.cfg.string

      if (aggrFun == "mean" & fs.config == 1) {
      # Evaluate random search on OOB-Tasks on OpenML
      n.rs   = c(1, 2, 4, 8, 16, 32, 64)
      rs.res.sur = foreach (z = seq_len(30), .combine = "bind_rows", .export = "surrogates") %:%
        foreach(it = seq_len(n_datasets), .combine = "bind_rows", .export = "surrogates") %:%
        foreach(n = n.rs, .combine = "bind_rows", .export = "surrogates") %dopar% {
          evalRandomSearchSurrogates(
            task.ids = names(surrogates$surrogates[it]),
            lrn = makeLearner(gsub(x = learner.names[i], "mlr.", "", fixed = TRUE)),
            defaults = defs$defaults,
            ps = surrogates$param.set,
            it = it,
            n = n)
        }
        rs.res.sur$aggrFun = "random"
        rs.res.sur$cfg = "n_1_1"
        def.res.sur = bind_rows(def.res.sur, rs.res.sur)
      }
      def.res.sur$learner.id = learner.names[i]


      saveRDS(def.res.sur, file = res.file)
    } else {
      catf("Skipped learner: %s", learner.names[i])
      readRDS(res.file)
    }
  }


lst = sapply(list.files("evalAggrFuns/results/", full.names = TRUE), readRDS, simplify = FALSE)
df = do.call("bind_rows", lst)
df$learner.id = factor(df$learner.id, labels = c("glmnet", "rpart", "svm", "xgboost"))
saveRDS(df,  "evalAggrFuns/aggrFunsResult.RDS")


library(ggplot2)
p = df %>%
 group_by(task.id, search.type, aggrFun, n, cfg, learner.id) %>%
 summarize(auc.scaled = mean(auc.scaled)) %>%
 filter(search.type == "defaults") %>%
 group_by(learner.id, task.id) %>%
 filter(n %in% c(1, 2, 4, 8)) %>%
 mutate(n = as.factor(n)) %>%
 ggplot(aes(x = n, y = auc.scaled, color = aggrFun)) +
 geom_boxplot() +
 facet_wrap(~learner.id)
ggsave(filename = "evalAggrFuns/boxplot_comparison_by_learner.png", plot = p)

p2 = df %>%
 group_by(task.id, search.type, aggrFun, n, cfg, learner.id) %>%
 summarize(auc.scaled = mean(auc.scaled)) %>%
 filter(search.type == "defaults") %>%
 filter(cfg == "1000_2_5") %>%
 group_by(task.id) %>%
 mutate(n = as.factor(n)) %>%
 filter(n == 10) %>%
 ggplot(aes(x = auc.scaled, color = aggrFun)) +
 stat_ecdf() +
 coord_flip() +
 facet_wrap(~learner.id, scales = "free_y")
ggsave(filename = "evalAggrFuns/ecdf_comparison_by_learner.png", plot = p2)

library(hrbrthemes)
p2.2 = df %>%
 group_by(task.id, search.type, aggrFun, n, cfg, learner.id) %>%
 summarize(auc.scaled = mean(auc.scaled)) %>%
 filter(learner.id %in% c("rpart", "xgboost")) %>%
 filter(search.type == "defaults") %>%
 filter(cfg == "1000_2_5") %>%
 group_by(task.id) %>%
 mutate(n = as.factor(n)) %>%
 filter(n  %in% c(2, 4, 8)) %>%
 ggplot(aes(x = auc.scaled, color = aggrFun)) +
 stat_ecdf() +
 coord_flip() +
 facet_wrap(~learner.id, nrow = 1, scales = "free_y") +
 theme_bw() +
 theme(legend.position="bottom", legend.title = element_text("Aggregation function")) +
 xlab("Normalized Area under the Curve") +
 ylab("Quantile")
ggsave(filename = "evalAggrFuns/ecdf_comparison_2_learner.png", plot = p2.2, width = 4, height = 3)
ggsave(filename = "../paper_2018_multiple_defaults/figures/ecdf_comparison_2_learner.pdf",
 plot = p2.2, width = 10.5, height = 6.5, scale = 1)

p3 = df %>%
 group_by(task.id, search.type, aggrFun, n, cfg, learner.id) %>%
 summarize(auc.scaled = mean(auc.scaled)) %>%
 filter(n %in% c(1, 2, 4, 8, 16, 32)) %>%
 filter(aggrFun %in% c("design", "random")) %>%
 group_by(learner.id, task.id) %>%
 mutate(n = as.factor(n)) %>%
 ggplot(aes(x = n, y = auc.scaled, color = search.type)) +
 geom_boxplot() +
 facet_wrap(~learner.id, scales = "free_y") +
 theme(legend.position="bottom") +
 ylab("Normalized Area under the Curve") +
 xlab("No. evaluations")
ggsave(filename = "evalAggrFuns/boxplot_compare_search_by_learner.png", plot = p)

p = df %>%
 group_by(task.id, search.type, aggrFun, n, cfg, learner.id) %>%
 summarize(auc.scaled = mean(auc.scaled)) %>%
 filter(n %in% c(1, 2, 4, 8, 16, 32)) %>%
 filter(aggrFun %in% c("design", "random")) %>%
 group_by(learner.id, task.id) %>%
 mutate(n = as.factor(n)) %>%
 ggplot(aes(x = n, y = auc.scaled, color = search.type)) +
 geom_boxplot() +
 facet_wrap(~learner.id, scales = "free_y") +
 theme(legend.position="bottom") +
 ylab("Normalized Area under the Curve") +
 xlab("No. evaluations")
ggsave(filename = "evalAggrFuns/boxplot_compare_search_by_learner.png", plot = p)


df %>%
 group_by(task.id, search.type, aggrFun, n, cfg, learner.id) %>%
 filter(cfg == "10000_1_1") %>%
 filter(n %in% c(1, 2, 4, 8, 16)) %>%
 filter(search.type == "random")
