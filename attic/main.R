library(devtools)         # load_all()
library(stringi)          # string manipulation
library(focussearch)      # Search the surrogates
library(foreach)
library(R6)
load_all()                # /R
load_all("../surrogates") # Surrogates


# Preprocessing Steps (only run once)
# Step 1: Obtain OML Bot Data -> figshare_to_data()
# Step 2: Train Surrogates    -> train_surrogates_omlbot()

# Step 3: Search for the surrogates
# Load Collection of Surrogates
library(doParallel)
registerDoParallel(19)

# svm, xgboost
sc = make_surrogates_omlbot(baselearners = c("svm", "xgboost"))
res1 = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 10 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id)
  ds$search_defaults()
  ds$save_to_disk()
  ds$evaluate_defaults_holdout()
}

# xgboost
sc = make_surrogates_omlbot(baselearners = c("xgboost"))
res_xgb = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 10 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id)
  ds$search_defaults()
  ds$save_to_disk()
  ds$evaluate_defaults_holdout()
}

# svm
sc = make_surrogates_omlbot(baselearners = c("svm"))
res_svm = foreach(oml_task_id = get_oml_task_ids(), .combine = "cbind") %dopar% {
  # Search 10 Defaults, hold out task x
  ds = DefaultSearch$new(sc, 32L, oml_task_id)
  ds$search_defaults()
  ds$save_to_disk()
  ds$evaluate_defaults_holdout()
}
