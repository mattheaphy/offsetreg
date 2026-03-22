# Poisson regression models with offsets

`poisson_reg_offset()` defines a generalized linear model of count data
with an offset that follows a Poisson distribution.

## Usage

``` r
poisson_reg_offset(
  mode = "regression",
  penalty = NULL,
  mixture = NULL,
  engine = "glm_offset"
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "regression".

- penalty:

  A non-negative number representing the total amount of regularization
  (`glmnet` only).

- mixture:

  A number between zero and one (inclusive) giving the proportion of L1
  regularization (i.e. lasso) in the model.

  - `mixture = 1` specifies a pure lasso model,

  - `mixture = 0` specifies a ridge regression model, and

  - `0 < mixture < 1` specifies an elastic net model, interpolating
    lasso and ridge.

  Available for `glmnet` and `spark` only.

- engine:

  A single character string specifying what computational engine to use
  for fitting.

## Value

A model specification object with the classes `poisson_reg_offset` and
`model_spec`.

## Details

This function is similar to
[`parsnip::poisson_reg()`](https://parsnip.tidymodels.org/reference/poisson_reg.html)
except that specification of an offset column is required.

## See also

[`parsnip::poisson_reg()`](https://parsnip.tidymodels.org/reference/poisson_reg.html)

## Examples

``` r
parsnip::show_model_info("poisson_reg_offset")
#> Information for `poisson_reg_offset`
#>  modes: unknown, regression 
#> 
#>  engines: 
#>    regression: glm_offset¹, glmnet_offset¹
#> 
#> ¹The model can use case weights.
#> 
#>  arguments: 
#>    glmnet_offset: 
#>       penalty --> lambda
#>       mixture --> alpha
#> 
#>  fit modules:
#>           engine       mode
#>       glm_offset regression
#>    glmnet_offset regression
#> 
#>  prediction modules:
#>          mode        engine      methods
#>    regression    glm_offset numeric, raw
#>    regression glmnet_offset numeric, raw
#> 

poisson_reg_offset()
#> Poisson Regression with Offsets Model Specification (regression)
#> 
#> Computational engine: glm_offset 
#> 
```
