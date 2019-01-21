
library(devtools)         # load_all()
library(stringi)          # string manipulation
library(focussearch)      # Search the surrogates
load_all()                # /R
load_all("../surrogates") # Surrogates

# Run the functions in R/preprocess.R to obtain trained surrogates

# Load surrogates, set a prediction aggregator.
bls = c("svm", "xgboost")
sc = make_surrogates_omlbot(base_learners = bls)
dsearch = DefaultSearch$new(sc, holdout_task_id)
dsearch$search_defaults()
