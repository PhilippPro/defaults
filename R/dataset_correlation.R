

library(ParamHelpers)
library(mlr)
library(BBmisc)
library(farff)
source("R/evalParsOpenML.R")

surr = readRDS("surrogates/_regr.cubist_classif.rpart_auc_zscale_.RDS")
lrn = makeLearner("classif.rpart")
rnd = generateRandomDesign(10^4, surr$param.set)
rnd = fixDefaultsForWrapper(rnd, lrn, lrn.ps)
out = lapply(surr$surrogates,  predict, newdata = rnd)
out = extractSubList(out, "data")
out = do.call("cbind", out)
cors = cor(out)

quantile(cors[upper.tri(cors, diag = FALSE)], 0.95)

nm = cors > .92 & cors < 1
for (i in seq_len(nrow(cors))) {
  for (j in seq_len(nrow(cors))) {
    if (nm[i,j] & i > j)
      print(paste(rownames(cors)[i], colnames(cors)[j], sep = "_"))
  }
}



library(OpenML)
surr = readRDS("surrogates/_regr.cubist_pertask_adaboost.arff_zscale_.RDS")
pertask = readARFF("sklearn_oml100/adaboost.arff")
names(surr) = unique(pertask$task_id)
ps = makeParamSet(
      makeDiscreteParam(id = "algorithm", values = c("SAMME", "SAMME.R")),
      makeNumericParam(id = "learning_rate", lower = 0.01, upper = 2.0),
      makeIntegerParam(id = "max_depth", lower = 1, upper = 20),
      makeIntegerParam(id = "n_estimators", lower = 50, upper = 500),
      makeDiscreteParam(id = "strategy", values = c("median", "most_frequent", "mean"))
    )
rnd = generateRandomDesign(10^4, ps)
out = lapply(surr,  predict, newdata = rnd)
names(out) = names(surr)
out = extractSubList(out, "data")
out = do.call("cbind", out)
out = out[, - which(colnames(out) == "34538")] # Remove constant dataset
cors = cor(out)
upper = cos[upper.tri(cors, diag = FALSE)]
nm = cors > .95 & cors < 1
for (i in seq_len(nrow(cors))) {
  for (j in seq_len(nrow(cors))) {
    if (nm[i,j] & i > j)
      print(paste(
        getOMLTask(as.numeric(rownames(cors)[i]))$input$data.set$desc$name,
        getOMLTask(as.numeric(colnames(cors)[j]))$input$data.set$desc$name,
          cors[i,j], sep = "_"))
  }
}


"connect-4_electricity_0.956356911779439"
"har_vehicle_0.974157423453116"
"artificial-characters_first-order-theorem-proving_0.964144335940679"
"gas-drift_semeion_0.96398986545376"
"gas-drift_steel-plates-fault_0.951955682420621"
"gas-drift_JapaneseVowels_0.962622074518973"
"gas-drift_pendigits_0.95085241288111"
"climate-model-simulation-crashes_breast-w_0.96076734397759"
"steel-plates-fault_pendigits_0.953536888615862"
"one-hundred-plants-texture_one-hundred-plants-margin_0.956682493266664"
"semeion_pendigits_0.955130148747626"
"micro-mass_isolet_0.953239246780531"
"pendigits_mfeat-factors_0.952907513984836"

dt = listOMLTasks(data.tag = c("OpenML100"))
rownames(dt) = dt$task.id
rn = rownames(cors)
noname = rn[!(rn %in% dt$task.id)]
noname_name = sapply(noname, function(x) getOMLTask(as.numeric(x))$input$data.set$desc$name)
colnames(cors) = dt[rn, "name"]
colnames(cors)[is.na(colnames(cors))] = noname_name
rownames(cors) = colnames(cors)

hcl = hclust(as.dist(1-cors))
plot(hcl)

library(corrplot)
pdf("~/corrplot.pdf", width = 20, height = 20)
corrplot(cors, order = "hclust")
dev.off()


library(data.table)
defs = as.data.frame(fread("~/Documents/defaults/seq_defaults.txt"))
surr = readRDS("surrogates/_regr.cubist_pertask_libsvm_svc.arff_zscale_.RDS")
defs$C = defs$c
defs$c = NULL
defs$shrinking = factor("True", levels = c("True", "False"))
defs$tol = 10^-3
defs$coef0 = 0
defs$strategy = factor("mean", levels = c("mean", "median", "most_frequent"))
defs$kernel = tolower(defs$kernel)
defs[defs$kernel == "linear", c("kernel", "degree")] = c("polynomial", 1)
defs$kernel[defs$kernel == "polynomial"] = "poly"
defs$kernel = factor(defs$kernel, levels = c("poly", "sigmoid", "rbf"))
defs$rnk = NULL

# PS:
ps = makeParamSet(
      makeNumericParam("C", lower = -5, upper = 15, trafo = function(x) 2^x),
      makeNumericParam("coef0", lower = -1, upper = 1),
      makeIntegerParam("degree", lower = 1, upper = 5),
      makeNumericParam("gamma", lower = -15, upper = 3, trafo = function(x) 2^x),
      makeDiscreteParam("kernel", values = c("poly", "sigmoid", "rbf")),
      makeDiscreteParam("strategy", values = c("mean", "median", "most_frequent")),
      makeDiscreteParam("shrinking", values = c("True", "False")),
      makeNumericParam("tol", lower = -5, upper = -1, trafo = function(x) 10^x)
    )

defs$gamma = as.numeric(defs$gamma)
defs$degree = as.numeric(defs$degree)
defs = impute(defs, classes = list("numeric" = -11))$data
# reorder
out = lapply(surr, predict, newdata = defs)
out = extractSubList(out, "data")


defs2 = readRDS("~/Documents/defaults/full_defaults/median_defaults_sklearn_libsvm_svc.RDS")
out2 = lapply(surr, predict, newdata = defs2$defaults[1:8,])
out2 = extractSubList(out2, "data")

o1 = unlist(lapply(out, max))
o2 = unlist(lapply(out2, max))

table(o1 > o2)
