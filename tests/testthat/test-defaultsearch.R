test_that("multiplication works", {
  sc_xgb = make_surrogates_omlbot(baselearners = "xgboost", measures = "auc")
  ds = list(
    ds1 = DefaultSearch$new(sc_xgb, 3, 3, learner_prefix = "tst"),
    ds2 = DefaultSearch$new(sc_xgb, 3, 37, learner_prefix = "tst"),
    ds3 = DefaultSearch$new(sc_xgb, 3, 43, learner_prefix = "tst")
  )
  sapply(ds, function(x) x$ctrl$points = 10)
  sc_xgb$surrogates[c(1, 4:38)] = NULL
  expect_true(length(sc_xgb$surrogates) == 2)
  ds[[1]]$search_defaults()
  expect_true(is.null(ds[[1]]$get_holdout_performance()))
  ds[[2]]$search_defaults()
  expect_true(all(dim(ds[[2]]$get_holdout_performance()) == c(3, 1)))
})
