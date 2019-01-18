library(devtools)         # load_all()
library(stringi)          # string manipulation
library(focussearch)      # Search the surrogates
library(foreach)
load_all()                # /R
load_all("../surrogates") # Surrogates


# Step 1: Obtain OML Bot Data -> figshare_to_data()
# Step 2: Train Surrogates -> train_surrogates()

sc = SurrogateCollection$new(make_surrogates_omlbot())

sc$predict(newdata = data.frame("lambda" = 0:10, "alpha" = 0:10), baselearners = "glmnet")
