us_deaths$off <- log(us_deaths$population)
f <- deaths ~ gender + age_group + year
f_off <- update(f, ~ . + off)
y <- us_deaths$deaths
x <- model.matrix(f, us_deaths)[, -1]
x_off <- model.matrix(update(f, ~. + off), us_deaths)[, -1]

# base models
glm_base <- glm(f, family = "poisson", us_deaths, offset = off)
glmnet_base <- glmnet::glmnet(x, y, family = "poisson", offset = us_deaths$off,
                              alpha = 0.25)

rec <- recipes::recipe(deaths ~ gender + age_group + year + off,
                       data = us_deaths) |>
  recipes::step_rename(offset = off)

test_that("*_offset() models work", {

  # glm_offset
  glm_off <- glm_offset(f, family = "poisson",
                        us_deaths, offset_col = "off")

  expect_identical(predict(glm_base), predict(glm_off))

  # glmnet_offset
  glmnet_off <- glmnet_offset(x_off, y, family = "poisson", offset_col = "off",
                              alpha = 0.25)


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
  expect_identical(predict(glm_base),
                   predict(glm_off, us_deaths, type = "raw"))

  # glmnet offset
  glmnet_off <- poisson_reg_offset(penalty = 1E-5, mixture = 0.25) |>
    set_engine("glmnet_offset", offset_col = "off") |>
    fit(f_off, data = us_deaths)
  expect_identical(predict(glmnet_base, x, newoffset = us_deaths$off, s = 1E-5,
                           type = 'response') |> as.numeric(),
                   predict(glmnet_off, us_deaths)$.pred)
  expect_identical(predict(glmnet_base, x, newoffset = us_deaths$off, s = 1E-5),
                   predict(glmnet_off, us_deaths, type = "raw"))

})

test_that("poisson_reg_offset() works with recipes", {

  # glm offset
  glm_off <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(poisson_reg_offset() |> set_engine("glm_offset")) |>
    fit(data = us_deaths)
  expect_identical(predict(glm_base, type = 'response') |> unname(),
                   predict(glm_off, us_deaths)$.pred)

  # glmnet offset
  rec <- rec |> recipes::step_dummy(recipes::all_nominal_predictors())
  glmnet_off <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(poisson_reg_offset(penalty = 1E-5, mixture = 0.25) |>
                           set_engine("glmnet_offset")) |>
    fit(data = us_deaths)
  # re-do the baseline glmnet fix - columns are in a different order after the
  # recipe
  x <- rec |> recipes::prep() |> recipes::juice() |> as.matrix()
  x <- x[, !colnames(x) %in% c("offset", "deaths")]
  glmnet_base <- glmnet::glmnet(x, y, family = "poisson", offset = us_deaths$off,
                                alpha = 0.25)

  expect_identical(predict(glmnet_base, x, newoffset = us_deaths$off, s = 1E-5,
                           type = 'response') |> as.numeric(),
                   predict(glmnet_off, us_deaths)$.pred)

})

test_that("finalize works", {

  mod_spec <- poisson_reg_offset(penalty = tune(),
                                 mixture = tune()) |>
    set_engine("glmnet_offset")

  rec <- rec |> recipes::step_dummy(recipes::all_nominal_predictors())

  wf <- workflows::workflow() |>
    workflows::add_model(mod_spec) |>
    workflows::add_recipe(rec)

  param_grid <- data.frame(penalty = 0.005, mixture = 0.9)

  expect_no_error(tune::finalize_workflow(wf, param_grid) |> fit(us_deaths))

  expect_equal(tune::finalize_model(mod_spec, param_grid)$args |>
                 lapply(rlang::eval_tidy),
               list(penalty = 0.005, mixture =0.9))

})
