
<!-- README.md is generated from README.Rmd. Please edit that file -->

# offsetreg

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/offsetreg)](https://CRAN.R-project.org/package=offsetreg)
[![R-CMD-check](https://github.com/mattheaphy/offsetreg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mattheaphy/offsetreg/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package extends the [tidymodels](https://www.tidymodels.org)
ecosystem to enable usage of predictive models with offset terms. Offset
terms are predictors in a linear model that with a known *a priori*
value. In other words, these terms do not have an associated coefficient
($\beta_i$) that needs to be determined. In a generalized linear model
(GLM), an offset specification looks like:

$$
\hat{Y} = g^{-1}(offset + \beta_0 + \beta_1X_1 + \beta_2X_2 + \ldots + \beta_pX_p)
$$

Models with offsets are most useful when working with count data or when
fitting an adjustment model on top of an existing model with a prior
expectation. The former situation is common in insurance where data is
often aggregated or weighted by exposures. The latter is common in life
insurance where industry mortality tables are often used as a starting
point for setting assumptions on a particular block of business.

In general, offsetreg functions are named after existing functions from
tidymodels or other modeling package suffixed by `_offset`. The modeling
engines in this package are wrappers around existing, well-known
modeling functions. These engines all include the argument `offset_col`
which is used to specify which column in the data passed to the engine
contains offsets.

Currently, the following functions are available:

Model specifications:

- `poisson_reg_offset()` - create a Poisson GLM spec

Model engines

- `glm_offset()` - a wrapper around `stats::glm()`
- `glmnet_offset()` - a wrapper around `glmnet::glmnet()`

## Installation

You can install the development version of offsetreg from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mattheaphy/offsetreg")
```

## Basic usage

The `us_deaths` data set contains United States deaths, population
estimates, and crude mortality rates for ages 25+ from the CDC Multiple
Causes of Death Files for the years 2011-2020.

``` r
library(offsetreg)
library(parsnip)
#> Warning: package 'parsnip' was built under R version 4.2.3
us_deaths
#> # A tibble: 140 × 6
#>    gender age_group  year deaths population       qx
#>    <fct>  <fct>     <int>  <dbl>      <dbl>    <dbl>
#>  1 Female 25-34      2011  13663   20746335 0.000659
#>  2 Female 25-34      2012  13808   20970529 0.000658
#>  3 Female 25-34      2013  14001   21203096 0.000660
#>  4 Female 25-34      2014  14480   21546290 0.000672
#>  5 Female 25-34      2015  15736   21838064 0.000721
#>  6 Female 25-34      2016  17359   22077505 0.000786
#>  7 Female 25-34      2017  18066   22351311 0.000808
#>  8 Female 25-34      2018  17980   22487065 0.000800
#>  9 Female 25-34      2019  17827   22581141 0.000789
#> 10 Female 25-34      2020  21654   22625267 0.000957
#> # ℹ 130 more rows
```

Assume we want to create a poisson model for the number of deaths.
First, an offset term is created by taking the natural log of
population, which is our exposure basis. A natural log is used because
the link function in poisson regression is the exponential function.

``` r
us_deaths$offset <- log(us_deaths$population)
```

Create a poisson regression model with an offset, and set the engine to
“glm_offset”. The engine-specific argument `offset_col` must refer to
the name of the column in `us_deaths` that contains offsets.

**Note**: The offset term should always be included in model formulas.

``` r
glm_off <- poisson_reg_offset() |>
  # set the modeling engine and specify the offset column
  set_engine("glm_offset", offset_col = "offset") |>
  # always include the offset term in the model formula
  fit(deaths ~ gender + age_group + year + offset, data = us_deaths)

glm_off
#> parsnip model object
#> 
#> 
#> Call:  stats::glm(formula = formula, family = family, data = data, offset = offset)
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

Verify that coefficients match `stats::glm()`.

``` r
glm_base <- glm(deaths ~ gender + age_group + year, offset = offset,
                data = us_deaths, family = 'poisson')

identical(extract_fit_engine(glm_off) |> coef(), coef(glm_base))
#> [1] TRUE
```
