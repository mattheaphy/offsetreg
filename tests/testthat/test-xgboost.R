
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

test_that("xgb_train_offset throws the correct errors and warnings", {
  expect_error(xgb_train_offset(x, us_deaths$deaths, "off", validation = -1),
               regexp = "`validation` should be")
  expect_error(xgb_train_offset(x, us_deaths$deaths, "off", early_stop = 1),
               regexp = "`early_stop` should be")
  expect_warning(xgb_train_offset(x, us_deaths$deaths, "off", early_stop = 99),
                 regexp = "`early_stop` was reduced to")
  expect_error(xgb_train_offset(x, us_deaths$deaths, "off", subsample = 1.01),
               regexp = "`subsample` should be")
  expect_warning(xgb_train_offset(x, us_deaths$deaths, "off",
                                  min_child_weight = 1E3),
                 regexp = "1000 samples were requested")
  expect_error(xgb_train_offset(x, us_deaths$deaths),
               regexp = "A column named `offset` must be present")
  expect_error(xgb_train_offset(x, us_deaths$deaths, "off",
                                colsample_bynode = 0.5),
               regexp = "Please use a value >= 1")
  expect_warning(xgb_train_offset(x, us_deaths$deaths, "off",
                                  objective = "reg:squarederror"),
                 regexp = "The following arguments are guarded")
  expect_warning(xgb_train_offset(x, us_deaths$deaths, "off",
                                  params = list(eta = 1)),
                 regexp = "Please supply elements of the `params` list")
  expect_error(xgb_predict_offset(mod2, xgboost::xgb.DMatrix(x)),
               regexp = "If `new_data` is an `xgb.DMatrix`,")
})
