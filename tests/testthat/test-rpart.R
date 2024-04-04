# standard formula for testing
f <- deaths ~ gender + age_group + year

test_that("Error checks trigger", {
  expect_error(rpart_exposure(data = 1),
               regexp = "`data` must be a data frame")
  expect_error(rpart_exposure(deaths ~ age_group, data = us_deaths,
                              exposure_col = "x"),
               regexp = "A column named `x` must be present")
  expect_error(rpart_exposure(cbind(deaths, population) ~ age_group,
                              data = us_deaths,
                              exposure_col = "population"),
               regexp = "The left-hand side of `formula`")
})

rpart_base <- rpart::rpart(
  cbind(population, deaths) ~ gender + age_group + year,
  data = us_deaths, method = 'poisson', cp = 0.01)
rpart_expo <- rpart_exposure(f, exposure_col = "population",
                             data = us_deaths, cp = 0.01)

test_that("rpart_exposure() model works", {
  expect_identical(predict(rpart_base), predict(rpart_expo))
})

test_that("control and ... return identical results", {
  rpart_expo_ctrl <- rpart_exposure(
    f, exposure_col = "population",
    data = us_deaths,
    control = rpart::rpart.control(cp = 0.001, maxdepth = 3, minsplit = 4))
  rpart_expo_dots <- rpart_exposure(f, exposure_col = "population",
                                    data = us_deaths,
                                    cp = 0.001, maxdepth = 3, minsplit = 4)

  expect_identical(predict(rpart_expo_ctrl), predict(rpart_expo_dots))
})

test_that("weights and costs work", {
  rpart_wt <- rpart_exposure(f, exposure_col = "population",
                             data = us_deaths, cp = 0.01,
                             weights = us_deaths$population)
  expect_false(identical(predict(rpart_expo), predict(rpart_wt)))

  rpart_costs <- rpart_exposure(f, exposure_col = "population",
                                data = us_deaths, cp = 0.01,
                                cost = c(1, 100, 1))
  expect_false(identical(predict(rpart_expo), predict(rpart_costs)))

})

test_that("decision_tree_exposure() works", {

  # rpart_exposure
  rpart_expo <- decision_tree_exposure() |>
    set_engine("rpart_exposure", exposure_col = "population") |>
    fit(f, data = us_deaths)
  expect_identical(predict(rpart_base) |> unname(),
                   predict(rpart_expo, us_deaths)$.pred)
  expect_identical(predict(rpart_base),
                   predict(rpart_expo, us_deaths, type = "raw"))

})


rec <- recipes::recipe(deaths ~ gender + age_group + year + population,
                       data = us_deaths) |>
  recipes::step_rename(exposure = population)

test_that("decision_tree_exposure() works with recipes", {

  # rpart_exposure
  rpart_expo <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(decision_tree_exposure() |>
                           set_engine("rpart_exposure")) |>
    fit(data = us_deaths)
  expect_identical(predict(rpart_base) |> unname(),
                   predict(rpart_expo, us_deaths)$.pred)

})

test_that("finalize works", {

  mod_spec <- decision_tree_exposure(cost_complexity = tune(),
                                     tree_depth = tune(),
                                     min_n = tune()) |>
    set_engine("rpart_exposure")

  wf <- workflows::workflow() |>
    workflows::add_model(mod_spec) |>
    workflows::add_recipe(rec)

  param_grid <- data.frame(cost_complexity = 0.001, tree_depth = 25, min_n = 5)

  expect_no_error(tune::finalize_workflow(wf, param_grid) |> fit(us_deaths))

  expect_equal(tune::finalize_model(mod_spec, param_grid)$args |>
                 lapply(rlang::eval_tidy),
               as.list(param_grid))

})
