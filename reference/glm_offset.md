# Fit Generalized Linear Models with an Offset

This function is a wrapper around
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) that uses a column
from `data` as an offset.

## Usage

``` r
glm_offset(
  formula,
  family = "gaussian",
  data,
  offset_col = "offset",
  weights = NULL
)
```

## Arguments

- formula:

  A model formula

- family:

  A function or character string describing the link function and error
  distribution.

- data:

  Optional. A data frame containing variables used in the model.

- offset_col:

  Character string. The name of a column in `data` containing offsets.

- weights:

  Optional weights to use in the fitting process.

## Value

A `glm` object. See [`stats::glm()`](https://rdrr.io/r/stats/glm.html)
for full details.

## Details

Outside of the `tidymodels` ecosystem, `glm_offset()` has no advantages
over [`stats::glm()`](https://rdrr.io/r/stats/glm.html) since that
function allows for offsets to be specified in the formula interface or
its `offset` argument.

Within `tidymodels`, `glm_offset()` provides an advantage because it
will ensure that offsets are included in the data whenever resamples are
created.

The `formula`, `family`, `data`, and `weights` arguments have the same
meanings as [`stats::glm()`](https://rdrr.io/r/stats/glm.html). See that
function's documentation for full details.

## See also

[`stats::glm()`](https://rdrr.io/r/stats/glm.html)

## Examples

``` r
if (interactive()) {
  us_deaths$off <- log(us_deaths$population)
  glm_offset(deaths ~ age_group + gender, family = "poisson",
             us_deaths, offset_col = "off")
}
```
