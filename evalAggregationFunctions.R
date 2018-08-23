library(devtools)     # load_all()
library(stringi)      # string manipulation
library(focussearch)  # Search the surrogates
library(doParallel)   # Parallelization
library(doMC)         # Parallelization
library(doRNG)        # Parallel RNG
library(foreach)      # Parallelization
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

registerDoMC(5)

foreach(i = seq_len(6)[-3]) %:%
  foreach(aggrFun = c("mean", "hodges-lehmann", "design", "avg.quantiles357", "avg.quantiles05595")) %:%
  foreach(fs.config = seq_len(3)) %dopar% {


    fs.cfg.string = paste0(fs.configs[fs.config, ], collapse = "_")
    res.file = stringBuilder("evalAggrFuns/results", aggrFun, learner.names[i], fs.cfg.string)
    if (!file.exists(res.file))  {

      catf("Learner: %s", learner.names[i])
      set.seed(199 + i)

      # Read surrogates from Hard Drive
      surrogates = readRDS(stri_paste("surrogates/", files[grep(stri_sub(learner.names[i], from = 5), x = files)]))
      # Create resampling train/test splits
      rin = makeResampleInstance(makeResampleDesc("CV", iters = 38), size = length(surrogates$surrogates))


      # Defaults
      defs.file = stringBuilder("evalAggrFuns/defaults", aggrFun, learner.names[i], fs.cfg.string)[1]
      # Compute defaults if not yet available
      if (!file.exists(defs.file)) {
        # Iterate over ResampleInstance and its indices
        defs = foreach(it = seq_len(rin$desc$iters)) %dorng% {
          if(aggrFun == "design") aggrFun = 0.5 # Design means optimize the median
          set.seed(199 + i)
          # Search for defaults
          defs = searchDefaults(
            surrogates$surrogates[rin$train.inds[[it]]], # training surrogates (L-1-Out-CV)
            surrogates$param.set, # parameter space to search through
            n.defaults = 10, # Number of defaults we want to find
            probs = aggrFun, # AggrFun we want to optimize
            fs.config = fs.configs[fs.config, ]
            )
          return(defs)
        }
        # Save found defaults as RDS
        saveRDS(list("defaults" = defs), defs.file)
        list("defaults" = defs)
      } else {
        defs = readRDS(defs.file)
      }


      n.defs = c(1, 2, 4, 6, 8, 10)
      def.res.sur = foreach(it = seq_len(rin$desc$iters), .combine = "bind_rows") %:%
        foreach(n = n.defs, .combine = "bind_rows") %do% {
          evalDefaultsSurrogates(
            task.ids = names(surrogates$surrogates[rin$test.inds[[it]]]),
            lrn = makeLearner(gsub(x = learner.names[i], "mlr.", "", fixed = TRUE)),
            defaults = defs$defaults,
            ps = surrogates$param.set,
            it = it,
            n = n)
        }

      # Evaluate random search on OOB-Tasks on OpenML
      n.rs   = c(1, 2, 4, 8, 16, 32, 64)
      rs.res.sur = foreach (z = seq_len(30), .combine = "bind_rows") %:%
        foreach(it = seq_len(rin$desc$iters), .combine = "bind_rows") %:%
        foreach(n = n.rs, .combine = "bind_rows") %do% {
          evalRandomSearchSurrogates(
            task.ids = names(surrogates$surrogates[rin$test.inds[[it]]]),
            lrn = makeLearner(gsub(x = learner.names[i], "mlr.", "", fixed = TRUE)),
            defaults = defs$defaults,
            ps = surrogates$param.set,
            it = it,
            n = n)
        }
      df = bind_rows(rs.res.sur, def.res.sur)
      df$learner.id = learner.names[i]
      df$aggrFun = aggrFun
      df$cfg = fs.cfg.string

      saveRDS(df, file = res.file)
      df
    } else {
      catf("Skipped learner: %s", learner.names[i])
      readRDS(res.file)
    }
}



