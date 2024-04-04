library(parsnip)

us_deaths$off <- log(us_deaths$population)

us_deaths2 <- recipes::recipe(~ age_group + gender + year + off, us_deaths) |>
  recipes::step_dummy(age_group, gender, one_hot = TRUE) |>
  recipes::prep() |>
  recipes::juice()

x <- as.matrix(us_deaths2)

xgtrain <- xgboost::xgb.DMatrix(x[, colnames(x) != "off"],
                                label = us_deaths$deaths,
                                base_margin = us_deaths$off)

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

# standard formula for testing
f <- deaths ~ age_group + gender + year + off

test_that("boost_tree_offset() works", {

  xgb_off <- boost_tree_offset(learn_rate = 1,
                               sample_size = 1,
                               mtry = 11,
                               min_n = 1,
                               tree_depth = 2,
                               trees = 25) |>
    set_engine("xgboost_offset", offset_col = "off") |>
    fit(f, data = us_deaths)
  expect_identical(predict(mod, xgtrain),
                   predict(xgb_off, us_deaths)$.pred)
  expect_identical(predict(mod, xgtrain),
                   predict(xgb_off, us_deaths, type = "raw"))

})

rec <- recipes::recipe(deaths ~ age_group + gender + year + off, us_deaths) |>
  recipes::step_dummy(age_group, gender, one_hot = TRUE) |>
  recipes::step_rename(offset = off)

test_that("boost_tree_offset() works with recipes", {

  # rpart_exposure
  xgb_off <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(boost_tree_offset(learn_rate = 1,
                                           sample_size = 1,
                                           mtry = 11,
                                           min_n = 1,
                                           tree_depth = 2,
                                           trees = 25) |>
                           set_engine("xgboost_offset")) |>
    fit(data = us_deaths)
  expect_identical(predict(mod, xgtrain),
                   predict(xgb_off, us_deaths)$.pred)

})

test_that("finalize works", {

  mod_spec <- boost_tree_offset(mtry = tune(),
                                trees = tune(),
                                min_n = tune(),
                                tree_depth = tune(),
                                learn_rate = tune(),
                                loss_reduction = tune(),
                                sample_size = tune(),
                                stop_iter = tune()) |>
    set_engine("xgboost_offset")

  wf <- workflows::workflow() |>
    workflows::add_model(mod_spec) |>
    workflows::add_recipe(rec)

  param_grid <- data.frame(mtry = 4,
                           trees = 11,
                           min_n = 2,
                           tree_depth = 3,
                           learn_rate = 0.3,
                           loss_reduction = 0,
                           sample_size = 0.7,
                           stop_iter = 7)

  expect_no_error(tune::finalize_workflow(wf, param_grid) |> fit(us_deaths))

  expect_equal(tune::finalize_model(mod_spec, param_grid)$args |>
                 lapply(rlang::eval_tidy),
               as.list(param_grid))

})
