us_deaths$off <- log(us_deaths$population)
f <- deaths ~ gender + age_group + year
f_off <- update(f, ~ . + off)
y <- us_deaths$deaths
x <- model.matrix(f, us_deaths)[, -1]
x_off <- model.matrix(update(f, ~. + off), us_deaths)[, -1]

# base models
glm_base <- glm(f, family = "poisson", us_deaths, offset = off)
glmnet_base <- glmnet::glmnet(x, y, family = "poisson", offset = us_deaths$off)

test_that("*_offset() models work", {

  # glm_offset
  glm_off <- glm_offset(f, family = "poisson",
                        us_deaths, offset_col = "off")

  expect_identical(predict(glm_base), predict(glm_off))

  # glmnet_offset
  glmnet_off <- glmnet_offset(x_off, y, family = "poisson", offset_col = "off")


  expect_identical(predict(glmnet_base, x, newoffset = us_deaths$off, s = 1E-5),
                   predict(glmnet_off, x, newoffset = us_deaths$off, s = 1E-5))

})

test_that("poisson_reg_offset() works", {

  # glm offset
  glm_off <- poisson_reg_offset() |>
    set_engine("glm_offset", offset_col = "off") |>
    fit(f, data = us_deaths)
  expect_identical(predict(glm_base, type = 'response') |> unname(),
                   predict(glm_off, us_deaths)$.pred)

  # glm offset
  glmnet_off <- poisson_reg_offset(penalty = 1E-5) |>
    set_engine("glmnet_offset", offset_col = "off") |>
    fit(f_off, data = us_deaths)
  expect_identical(predict(glmnet_base, x, newoffset = us_deaths$off, s = 1E-5,
                           type = 'response') |> as.numeric(),
                   predict(glmnet_off, us_deaths)$.pred)

})
