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

source("R/drakes_plan.R")

# Create Reports -----------------------------------------------------------------------------------
lrn.par.sets = getLearnerParSets()
learner.names = stri_sub(stri_paste("mlr.", names(lrn.par.sets)), 1, -5)
update_all_results()

# --------------------------------------------------------------------------------------------------
plan = drake_plan(
  data_in = file_in("defaultLOOCV/full_results.Rds"),
  search.type = NULL,
  # glmnet
  learner.name_1 = paste0(learner.names[1]),
  results_1 = get_results_from_folder(1, loadd(data_in)),
  data_1 = preprocess_results(results_1 %>% filter(search.type %in% c("design", "package-default", "random"))),
  plots_1 = make_plots(results_1, data_1, 1),
  report_1 = create_report(learner.name_1, data_1, j =  1),
  # rpart
  learner.name_2 = paste0(learner.names[2]),
  results_2 = get_results_from_folder(2, data_in),
  data_2 = preprocess_results(results_2 %>% filter(search.type %in% c("design", "package-default", "random"))),
  plots_2 = make_plots(results_2, data_2, 2),
  report_2 = create_report(learner.name_2, data_2, 2),
  # svm
  learner.name_4 = paste0(learner.names[4]),
  results_4 = get_results_from_folder(4, data_in),
  data_4 = preprocess_results(results_4 %>% filter(search.type %in% c("design", "package-default", "random"))),
  plots_4 = make_plots(results_4, data_4, 4),
  report_4 = create_report(learner.name_4, data_4, 4),
  # xgboost
  learner.name_6 = paste0(learner.names[6]),
  results_6 = get_results_from_folder(6, data_in),
  data_6 = preprocess_results(results_6 %>% filter(search.type %in% c("design", "package-default", "random"))),
  plots_6 = make_plots(results_6, data_6, 6),
  report_6 = create_report(learner.name_6, data_6, 6),
  
  strings_in_dots = "literals"
)
make(plan)

config <- drake_config(plan)
vis_drake_graph(config)
