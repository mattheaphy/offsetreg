# Poisson Recursive Partitioning and Regression Trees with Exposures

This function is a wrapper around
[`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html) for Poisson
regression trees using weighted exposures (observation times).

## Usage

``` r
rpart_exposure(
  formula,
  data,
  exposure_col = "exposure",
  weights = NULL,
  control,
  cost,
  shrink = 1,
  ...
)
```

## Arguments

- formula:

  A model formula that contains a single response variable on the
  left-hand side.

- data:

  Optional. A data frame containing variables used in the model.

- exposure_col:

  Character string. The name of a column in `data` containing exposures.

- weights:

  Optional weights to use in the fitting process.

- control:

  A list of hyperparameters. See
  [`rpart::rpart.control()`](https://rdrr.io/pkg/rpart/man/rpart.control.html).

- cost:

  A vector of non-negative costs for each variable in the model.

- shrink:

  Optional parameter for the splitting function. Coefficient of
  variation of the prior distribution.

- ...:

  Alternative input for arguments passed to
  [`rpart::rpart.control()`](https://rdrr.io/pkg/rpart/man/rpart.control.html).

## Value

An `rpart` model

## Details

Outside of the `tidymodels` ecosystem, `rpart_exposure()` has no
advantages over
[`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html) since that
function allows for exposures to be specified in the formula interface
by passing `cbind(exposure, y)` as a response variable.

Within `tidymodels`, `rpart_exposure()` provides an advantage because it
will ensure that exposures are included in the data whenever resamples
are created.

The `formula`, `data`, `weights`, `control`, and `cost` arguments have
the same meanings as
[`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html). `shrink`
is passed to
[`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html)'s `parms`
argument via a named list. See that function's documentation for full
details.

## See also

[`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html)

## Examples

``` r
if (interactive()) {
  rpart_exposure(
    deaths ~ age_group + gender,
    us_deaths,
    exposure_col = "population"
  )
}
```
