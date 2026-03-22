# When to use offsetreg

This vignette describes the motivation for offsetreg and when its usage
becomes necessary.

## When offsetreg is not necessary

For certain use cases, offsets are supported in tidymodels. Generally
speaking, for models that allow for offsets to be specified in a model
formula, tidymodels works fine out of the box and offsetreg is not
needed. The [`glm()`](https://rdrr.io/r/stats/glm.html) function from
the stats package is a good example of this.

### Offsets supported in model formulas: `glm()`

Below, a Poisson model is fit using the `us_deaths` data set with an
offset equal to the log of population.

``` r
library(parsnip)
library(offsetreg)
library(broom)
library(recipes)
library(workflows)
library(rsample)
library(tune)

us_deaths$log_pop <- log(us_deaths$population)

poisson_reg() |>
  set_engine("glm") |>
  fit(deaths ~ gender + age_group + year + offset(log_pop), 
      data = us_deaths)
#> parsnip model object
#> 
#> 
#> Call:  stats::glm(formula = deaths ~ gender + age_group + year + offset(log_pop), 
#>     family = stats::poisson, data = data)
#> 
#> Coefficients:
#>    (Intercept)      genderMale  age_group35-44  age_group45-54  age_group55-64  
#>     -18.337940        0.327632        0.442935        1.212463        1.990698  
#> age_group65-74  age_group75-84    age_group85+            year  
#>       2.713410        3.645763        4.770408        0.005683  
#> 
#> Degrees of Freedom: 139 Total (i.e. Null);  131 Residual
#> Null Deviance:       51700000 
#> Residual Deviance: 237800    AIC: 239800
```

The code above works for a few reasons:

- [`fit()`](https://generics.r-lib.org/reference/fit.html) captures the
  formula expression passed to it, and that formula is allowed to
  contain calls to other functions, like
  [`offset()`](https://rdrr.io/r/stats/offset.html).
- Since there is no additional pre-processing of the data required, that
  formula is passed to the [`glm()`](https://rdrr.io/r/stats/glm.html)
  function as-is, as shown in the call printed above.

Let’s assume we want to use a recipe to pre-process our data. In the
example below, a bare bones recipe is used to verify that we can
reproduce the same coefficients as the original example. Unfortunately,
this creates a problem because
[`recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
doesn’t allow in-line functions like
[`offset()`](https://rdrr.io/r/stats/offset.html).

``` r
mod <- poisson_reg() |> set_engine("glm")
rec <- recipe(deaths ~ gender + age_group + year + offset(log_pop), 
              data = us_deaths)
#> Error in `recipe()`:
#> ✖ Misspelled variable name or in-line functions detected.
#> ℹ The following function/misspelling was found: `offset`.
#> ℹ Use steps to do transformations instead.
#> ℹ If your modeling engine uses special terms in formulas, pass that formula to
#>   workflows as a model formula (`?parsnip::model_formula()`).
```

As the hint above explains, this error can be avoided by removing the
call to [`offset()`](https://rdrr.io/r/stats/offset.html) in the recipe
and passing a second formula to
[`add_model()`](https://workflows.tidymodels.org/reference/add_model.html)
as part of a workflow. Note that the variable passed to
[`offset()`](https://rdrr.io/r/stats/offset.html) must still be included
in the recipe.

``` r
rec <- recipe(deaths ~ gender + age_group + year + log_pop, 
              data = us_deaths)

workflow() |> 
  add_model(mod, 
            formula = deaths ~ gender + age_group + year + offset(log_pop)) |> 
  add_recipe(rec) |> 
  fit(us_deaths)
#> ══ Workflow [trained] ══════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: poisson_reg()
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 0 Recipe Steps
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> 
#> Call:  stats::glm(formula = deaths ~ gender + age_group + year + offset(log_pop), 
#>     family = stats::poisson, data = data)
#> 
#> Coefficients:
#>    (Intercept)      genderMale  age_group35-44  age_group45-54  age_group55-64  
#>     -18.337940        0.327632        0.442935        1.212463        1.990698  
#> age_group65-74  age_group75-84    age_group85+            year  
#>       2.713410        3.645763        4.770408        0.005683  
#> 
#> Degrees of Freedom: 139 Total (i.e. Null);  131 Residual
#> Null Deviance:       51700000 
#> Residual Deviance: 237800    AIC: 239800
```

These coefficients match the first example without a recipe, so we know
this model was set up correctly.

### Offsets not supported in model formulas: `glmnet()`

Not all modeling engines allow for offsets to be passed via the formula
interface. For example, the `glmnet()` function does not not accept
formulas; it requires model matrices. Instead, offsets are passed as a
numeric vector using an optional engine-specific `offset` argument.

``` r
poisson_reg(penalty = 1E-5) |>
  set_engine("glmnet", offset = us_deaths$log_pop) |>
  fit(deaths ~ year + gender + age_group, 
      data = us_deaths) |> 
  tidy()
#> Loaded glmnet 4.1-10
#> # A tibble: 9 × 3
#>   term            estimate penalty
#>   <chr>              <dbl>   <dbl>
#> 1 (Intercept)    -17.7     0.00001
#> 2 year             0.00540 0.00001
#> 3 genderMale       0.326   0.00001
#> 4 age_group35-44   0.338   0.00001
#> 5 age_group45-54   1.11    0.00001
#> 6 age_group55-64   1.89    0.00001
#> 7 age_group65-74   2.62    0.00001
#> 8 age_group75-84   3.55    0.00001
#> 9 age_group85+     4.68    0.00001
```

This code works because the argument `offset = us_deaths$log_pop` is
captured and passed directly into `glmnet()`.

If we try to use a recipe with an offset passed to the `formula`
argument of
[`add_model()`](https://workflows.tidymodels.org/reference/add_model.html),
a difficult-to-spot problem emerges. The model runs without errors, but
a completely different set of coefficients is returned.

``` r
mod_glmnet <- poisson_reg(penalty = 1E-5) |> set_engine("glmnet")
rec <- recipe(deaths ~ year + gender + age_group + log_pop, 
              data = us_deaths)

workflow() |> 
  add_model(mod_glmnet, 
            formula = deaths ~ year + gender + age_group + offset(log_pop)) |> 
  add_recipe(rec) |> 
  fit(us_deaths) |> 
  tidy()
#> # A tibble: 9 × 3
#>   term           estimate penalty
#>   <chr>             <dbl>   <dbl>
#> 1 (Intercept)    -42.9    0.00001
#> 2 year             0.0263 0.00001
#> 3 genderMale       0.0243 0.00001
#> 4 age_group35-44   0.303  0.00001
#> 5 age_group45-54   1.12   0.00001
#> 6 age_group55-64   1.85   0.00001
#> 7 age_group65-74   2.19   0.00001
#> 8 age_group75-84   2.45   0.00001
#> 9 age_group85+     2.71   0.00001
```

What happened here? Since `glmnet()` doesn’t natively support the
formula interface, it doesn’t know what to do with the
[`offset()`](https://rdrr.io/r/stats/offset.html) term passed to the
formula. Under the hood, the
[`offset()`](https://rdrr.io/r/stats/offset.html) term is quietly
dropped in a call to
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) that is
used to convert the formula to a matrix format acceptable to `glmnet()`.

``` r
model.matrix(deaths ~ year + gender + age_group + offset(log_pop),
             us_deaths) |> 
  head()
#>   (Intercept) year genderMale age_group35-44 age_group45-54 age_group55-64
#> 1           1 2011          0              0              0              0
#> 2           1 2012          0              0              0              0
#> 3           1 2013          0              0              0              0
#> 4           1 2014          0              0              0              0
#> 5           1 2015          0              0              0              0
#> 6           1 2016          0              0              0              0
#>   age_group65-74 age_group75-84 age_group85+
#> 1              0              0            0
#> 2              0              0            0
#> 3              0              0            0
#> 4              0              0            0
#> 5              0              0            0
#> 6              0              0            0
```

As a result, the model is exactly what we would see if there were no
offset terms to begin with. This is a situation when offsetreg is
required.

## When offsetreg is necessary

offsetreg becomes necessary when the underlying modeling engine does not
support offsets in formulas **and** either of these tasks are performed:

- A pre-processing recipe is applied to the data
- Resampling is performed, often in conjunction with hyperparameter
  tuning

### Using `recipe()` when offsets cannot be specified in a formula

Let’s continue with the last example. The problem can be addressed using
offsetreg as follows:

- Replace
  [`poisson_reg()`](https://parsnip.tidymodels.org/reference/poisson_reg.html)
  with
  [`poisson_reg_offset()`](https://mattheaphy.github.io/offsetreg/reference/poisson_reg_offset.md)
- Replace the “glmnet” engine with “glmnet_offset” and provide the name
  of the offset column
- Remove the `formula` argument in
  [`add_model()`](https://workflows.tidymodels.org/reference/add_model.html)
- Add a call to
  [`step_dummy()`](https://recipes.tidymodels.org/reference/step_dummy.html).
  This step was previously not necessary when `formula` was passed to
  [`add_model()`](https://workflows.tidymodels.org/reference/add_model.html).

``` r
mod_offset <- poisson_reg_offset(penalty = 1E-5) |> 
  set_engine("glmnet_offset", offset_col = "log_pop")
rec <- recipe(deaths ~ year + gender + age_group + log_pop, 
              data = us_deaths) |> 
  step_dummy(all_nominal_predictors())

workflow() |> 
  add_model(mod_offset) |> 
  add_recipe(rec) |> 
  fit(us_deaths) |> 
  tidy()
#> # A tibble: 9 × 3
#>   term              estimate penalty
#>   <chr>                <dbl>   <dbl>
#> 1 (Intercept)      -17.7     0.00001
#> 2 year               0.00540 0.00001
#> 3 gender_Male        0.326   0.00001
#> 4 age_group_X35.44   0.338   0.00001
#> 5 age_group_X45.54   1.11    0.00001
#> 6 age_group_X55.64   1.89    0.00001
#> 7 age_group_X65.74   2.62    0.00001
#> 8 age_group_X75.84   3.55    0.00001
#> 9 age_group_X85.     4.68    0.00001
```

### Resampling when offsets cannot be specified in a formula

For models like `glmnet()` where offsets can only be specified as a
numeric vector in engine-specific arguments, resampling presents a few
challenges:

- tidymodels is only aware of the numeric vector of offsets that has
  been passed and there is no defined relationship between individual
  observations and their associated offsets. As a result, when
  resampling occurs, offsets aren’t carried over to resampled data sets.
- Related, and pertinent to `glmnet()`, if the
  [`predict()`](https://rdrr.io/r/stats/predict.html) function requires
  offset terms, there is no mechanism to pass those along, which will
  result in an error.

Below is what happens if we attempt to fit 5 bootstrap resamples of the
`us_deaths` data set without offsetreg.

``` r
resamples <- bootstraps(us_deaths, times = 5)

mod_glmnet <- poisson_reg(penalty = 1E-5) |> 
  set_engine("glmnet", offset = us_deaths$log_pop)

workflow() |>
  add_recipe(rec) |>
  add_model(mod_glmnet) |>
  fit_resamples(resamples) |>
  collect_metrics()
#> → A | error:   No newoffset provided for prediction, yet offset used in fit of glmnet
#> There were issues with some computations   A: x1
#> Warning: All models failed. Run `show_notes(.Last.tune.result)` for more
#> information.
#> There were issues with some computations   A: x5
#> 
#> Error in `estimate_tune_results()`:
#> ! All models failed. Run `show_notes(.Last.tune.result)` for more
#>   information.
```

All models failed to fit, and we receive a specific error message about
no offsets being available for predictions.

``` r
show_notes(.Last.tune.result)
#> unique notes:
#> ──────────────────────────────────────────────────────────────────────
#> No newoffset provided for prediction, yet offset used in fit of glmnet
```

With offsetreg, this code performs as expected. offsetreg works because
behind the scenes it ensures that offset terms are attached to the data
at all times, which enables resampling and predictions to function
without error.

``` r
workflow() |>
    add_recipe(rec) |>
    add_model(mod_offset) |>
    fit_resamples(resamples) |>
    collect_metrics()
#> # A tibble: 2 × 6
#>   .metric .estimator      mean     n    std_err .config        
#>   <chr>   <chr>          <dbl> <int>      <dbl> <chr>          
#> 1 rmse    standard   26396.        5 3385.      pre0_mod0_post0
#> 2 rsq     standard       0.979     5    0.00313 pre0_mod0_post0
```
