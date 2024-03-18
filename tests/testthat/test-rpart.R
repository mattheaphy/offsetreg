test_that("Error checks trigger", {
  expect_error(rpart_poisson_exposure(data = 1),
               regexp = "`data` must be a data frame")
  expect_error(rpart_poisson_exposure(deaths ~ age_group, data = us_deaths,
                                      exposure_col = "x"),
               regexp = "A column named `x` must be present")
  expect_error(rpart_poisson_exposure(cbind(deaths, population) ~ age_group,
                                      data = us_deaths,
                                      exposure_col = "population"),
               regexp = "The left-hand side of `formula`")
})
