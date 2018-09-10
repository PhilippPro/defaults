
getMM = function(lrn.inds = seq_len(6)) {

lrn.par.sets = getLearnerParSets()
lrn.ps = extractSubList(lrn.par.sets, "param.set", simplify = FALSE)
names(lrn.ps) = stri_sub(names(lrn.par.sets), to = -5)


learner.names = stri_sub(stri_sub(stri_paste("mlr.", names(lrn.par.sets)), 1, -5), from = 5)
lrns = sapply(learner.names, makeLearner, predict.type = "prob")
lrnMM = makeModelMultiplexer(lrns)
psMM  = makeModelMultiplexerParamSet(lrnMM,
 "classif.glmnet" = lrn.ps[[1]], "classif.rpart" = lrn.ps[[2]], "classif.svm" = lrn.ps[[4]],
 "classif.ranger" = lrn.ps[[5]], "classif.xgboost" = lrn.ps[[6]]))

list(lrn = lrnMM, ps = psMM)
}

# Convert data to a form where we can fit the surrogates.
# Rename columns to include prefixes

surrogateDataToMM = function(df, mm) {

}


querySurrogates = function(surrogates, x) {
	lrn = x$selected.learner
	models = surrogates[[lrn]]

}