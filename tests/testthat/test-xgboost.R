
us_deaths$off <- log(us_deaths$population)
x <- model.matrix(~ age_group + gender, us_deaths)[, -1]


xgtrain <- xgboost::xgb.DMatrix(x, label = us_deaths$deaths)
xgboost::setinfo(xgtrain, "base_margin", us_deaths$off)

set.seed(42)
mod <- xgboost::xgb.train(
  params = list(
    objective  = "count:poisson",
    eval_metric = "rmse",
    eta = 1,
    subsample = 1,
    colsample_bynode = 1,
    min_child_weight = 1,
    max_depth = 2
  ),
  data = xgtrain,
  nrounds = 25
)

x <- model.matrix(~ age_group + gender + off, us_deaths)[, -1]
mod2 <- xgb_train_offset(x,
                         us_deaths$deaths, "off",
                         eta = 1, subsample = 1, colsample_bynode = 1,
                         min_child_weight = 1, max_depth = 2, nrounds = 25,
                         counts = FALSE)

test_that("xgb_train_offset matches xgboost", {
  expect_equal(predict(mod, xgtrain), predict(mod2, xgtrain))
  expect_equal(predict(mod, xgtrain), xgb_predict_offset(mod2, xgtrain))
  expect_equal(predict(mod, xgtrain), xgb_predict_offset(mod2, x, "off"))
})
