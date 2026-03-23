# Fit Penalized Generalized Linear Models with an Offset

This function is a wrapper around
[`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
that uses a column from `x` as an offset.

## Usage

``` r
glmnet_offset(
  x,
  y,
  family,
  offset_col = "offset",
  weights = NULL,
  lambda = NULL,
  alpha = 1,
  ...
)
```

## Arguments

- x:

  Input matrix

- y:

  Response variable

- family:

  A function or character string describing the link function and error
  distribution.

- offset_col:

  Character string. The name of a column in `data` containing offsets.

- weights:

  Optional weights to use in the fitting process.

- lambda:

  A numeric vector of regularization penalty values

- alpha:

  A number between zero and one denoting the proportion of L1 (lasso)
  versus L2 (ridge) regularization.

  - `alpha = 1`: Pure lasso model

  - `alpha = 0`: Pure ridge model

- ...:

  Additional named arguments passed to
  [`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html).

## Value

A `glmnet` object. See
[`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
for full details.

## Details

Outside of the `tidymodels` ecosystem, `glmnet_offset()` has no
advantages over
[`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
since that function allows for offsets to be specified in its `offset`
argument.

Within `tidymodels`, `glmnet_offset()` provides an advantage because it
will ensure that offsets are included in the data whenever resamples are
created.

The `x`, `y`, `family`, `lambda`, `alpha` and `weights` arguments have
the same meanings as
[`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html).
See that function's documentation for full details.

## See also

[`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)

## Examples

``` r
if (interactive()) {
  us_deaths$off <- log(us_deaths$population)
  x <- model.matrix(~ age_group + gender + off, us_deaths)[, -1]
  glmnet_offset(x, us_deaths$deaths, family = "poisson", offset_col = "off")
}
```
