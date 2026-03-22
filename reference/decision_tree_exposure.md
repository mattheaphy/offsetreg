# Poisson Decision Trees with Exposures

`decision_tree_exposure()` defines a Poisson decision tree model with
weighted exposures (observation times).

## Usage

``` r
decision_tree_exposure(
  mode = "regression",
  engine = "rpart_exposure",
  cost_complexity = NULL,
  tree_depth = NULL,
  min_n = NULL
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "regression"

- engine:

  A single character string specifying what computational engine to use
  for fitting.

- cost_complexity:

  A positive number for the the cost/complexity parameter (a.k.a. `Cp`)
  used by CART models (specific engines only).

- tree_depth:

  An integer for maximum depth of the tree.

- min_n:

  An integer for the minimum number of data points in a node that are
  required for the node to be split further.

## Value

A model specification object with the classes `decision_tree_exposure`
and `model_spec`.

## Details

This function is similar to
[`parsnip::decision_tree()`](https://parsnip.tidymodels.org/reference/decision_tree.html)
except that specification of an exposure column is required.

## See also

[`parsnip::decision_tree()`](https://parsnip.tidymodels.org/reference/decision_tree.html)

## Examples

``` r
parsnip::show_model_info("decision_tree_exposure")
#> Information for `decision_tree_exposure`
#>  modes: unknown, regression 
#> 
#>  engines: 
#>    regression: rpart_exposure¹
#> 
#> ¹The model can use case weights.
#> 
#>  arguments: 
#>    rpart_exposure: 
#>       cost_complexity --> cp
#>       min_n           --> minsplit
#>       tree_depth      --> maxdepth
#> 
#>  fit modules:
#>            engine       mode
#>    rpart_exposure regression
#> 
#>  prediction modules:
#>          mode         engine      methods
#>    regression rpart_exposure numeric, raw
#> 

decision_tree_exposure()
#> Poisson Decision Tree with Offsets Model Specification (regression)
#> 
#> Computational engine: rpart_exposure 
#> 
```
