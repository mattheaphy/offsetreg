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
rpart_expo <- rpart_exposure(deaths ~ gender + age_group + year,
                             exposure_col = "population",
                             data = us_deaths, cp = 0.01)

test_that("rpart_exposure() model works", {
  expect_identical(predict(rpart_base), predict(rpart_expo))
})

test_that("control and ... return identical results", {
  rpart_expo_ctrl <- rpart_exposure(
    deaths ~ gender + age_group + year, exposure_col = "population",
    data = us_deaths,
    control = rpart::rpart.control(cp = 0.001, maxdepth = 3, minsplit = 4))
  rpart_expo_dots <- rpart_exposure(
    deaths ~ gender + age_group + year, exposure_col = "population",
    data = us_deaths,
    cp = 0.001, maxdepth = 3, minsplit = 4)

  expect_identical(predict(rpart_expo_ctrl), predict(rpart_expo_dots))
})

test_that("weights and costs work", {
  rpart_wt <- rpart_exposure(deaths ~ gender + age_group + year,
                             exposure_col = "population",
                             data = us_deaths, cp = 0.01,
                             weights = us_deaths$population)
  expect_false(identical(predict(rpart_expo), predict(rpart_wt)))

  rpart_costs <- rpart_exposure(deaths ~ gender + age_group + year,
                                exposure_col = "population",
                                data = us_deaths, cp = 0.01,
                                cost = c(1, 100, 1))
  expect_false(identical(predict(rpart_expo), predict(rpart_costs)))

})
