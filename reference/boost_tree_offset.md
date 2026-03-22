# Boosted Poisson Trees with Offsets

`boost_tree_offset()` defines a model that creates a series of Poisson
decision trees with pre-defined offsets forming an ensemble. Each tree
depends on the results of previous trees. All trees in the ensemble are
combined to produce a final prediction. This function can be used for
count regression models only.

## Usage

``` r
boost_tree_offset(
  mode = "regression",
  engine = "xgboost_offset",
  mtry = NULL,
  trees = NULL,
  min_n = NULL,
  tree_depth = NULL,
  learn_rate = NULL,
  loss_reduction = NULL,
  sample_size = NULL,
  stop_iter = NULL
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "regression"

- engine:

  A single character string specifying what computational engine to use
  for fitting.

- mtry:

  A number for the number (or proportion) of predictors that will be
  randomly sampled at each split when creating the tree models (specific
  engines only).

- trees:

  An integer for the number of trees contained in the ensemble.

- min_n:

  An integer for the minimum number of data points in a node that is
  required for the node to be split further.

- tree_depth:

  An integer for the maximum depth of the tree (i.e. number of splits)
  (specific engines only).

- learn_rate:

  A number for the rate at which the boosting algorithm adapts from
  iteration-to-iteration (specific engines only). This is sometimes
  referred to as the shrinkage parameter.

- loss_reduction:

  A number for the reduction in the loss function required to split
  further (specific engines only).

- sample_size:

  A number for the number (or proportion) of data that is exposed to the
  fitting routine. For `xgboost`, the sampling is done at each iteration
  while `C5.0` samples once during training.

- stop_iter:

  The number of iterations without improvement before stopping (specific
  engines only).

## Value

A model specification object with the classes `boost_tree_offset` and
`model_spec`.

## Details

This function is similar to
[`parsnip::boost_tree()`](https://parsnip.tidymodels.org/reference/boost_tree.html)
except that specification of an offset column is required.

## See also

[`parsnip::boost_tree()`](https://parsnip.tidymodels.org/reference/boost_tree.html)

## Examples

``` r
parsnip::show_model_info("boost_tree_offset")
#> Information for `boost_tree_offset`
#>  modes: unknown, regression 
#> 
#>  engines: 
#>    regression: xgboost_offset¹
#> 
#> ¹The model can use case weights.
#> 
#>  arguments: 
#>    xgboost_offset: 
#>       tree_depth     --> max_depth
#>       trees          --> nrounds
#>       learn_rate     --> eta
#>       mtry           --> colsample_bynode
#>       min_n          --> min_child_weight
#>       loss_reduction --> gamma
#>       sample_size    --> subsample
#>       stop_iter      --> early_stop
#> 
#>  fit modules:
#>            engine       mode
#>    xgboost_offset regression
#> 
#>  prediction modules:
#>          mode         engine      methods
#>    regression xgboost_offset numeric, raw
#> 

boost_tree_offset()
#> Boosted Tree with Offsets Model Specification (regression)
#> 
#> Computational engine: xgboost_offset 
#> 
```
