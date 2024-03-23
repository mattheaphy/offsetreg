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

test_that("rpart_exposure() model works", {

  # glm_offset
  rpart_base <- rpart(cbind(population, deaths) ~ gender + age_group + year,
                      data = us_deaths, method = 'poisson', cp = 0.01)
  rpart_expo <- rpart_exposure(deaths ~ gender + age_group + year,
                                       exposure_col = "population",
                                       data = us_deaths, cp = 0.01)

  expect_identical(predict(rpart_base), predict(rpart_expo))
})
