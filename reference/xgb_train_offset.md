# Boosted Poisson Trees with Offsets via `xgboost`

`xgb_train_offset()` and `xgb_predict_offset()` are wrappers for
`xgboost` tree-based models where all of the model arguments are in the
main function. These functions are nearly identical to the parsnip
functions
[`parsnip::xgb_train()`](https://parsnip.tidymodels.org/reference/xgb_train.html)
and `parsnip::xg_predict_offset()` except that the objective
"count:poisson" is passed to
[`xgboost::xgb.train()`](https://rdrr.io/pkg/xgboost/man/xgb.train.html)
and an offset term is added to the data set.

## Usage

``` r
xgb_train_offset(
  x,
  y,
  offset_col = "offset",
  weights = NULL,
  max_depth = 6,
  nrounds = 15,
  eta = 0.3,
  colsample_bynode = NULL,
  colsample_bytree = NULL,
  min_child_weight = 1,
  gamma = 0,
  subsample = 1,
  validation = 0,
  early_stop = NULL,
  counts = TRUE,
  ...
)

xgb_predict_offset(object, new_data, offset_col = "offset", ...)
```

## Arguments

- x:

  A data frame or matrix of predictors

- y:

  A vector (numeric) or matrix (numeric) of outcome data.

- offset_col:

  Character string. The name of a column in `data` containing offsets.

- weights:

  A numeric vector of weights.

- max_depth:

  An integer for the maximum depth of the tree.

- nrounds:

  An integer for the number of boosting iterations.

- eta:

  A numeric value between zero and one to control the learning rate.

- colsample_bynode:

  Subsampling proportion of columns for each node within each tree. See
  the `counts` argument below. The default uses all columns.

- colsample_bytree:

  Subsampling proportion of columns for each tree. See the `counts`
  argument below. The default uses all columns.

- min_child_weight:

  A numeric value for the minimum sum of instance weights needed in a
  child to continue to split.

- gamma:

  A number for the minimum loss reduction required to make a further
  partition on a leaf node of the tree

- subsample:

  Subsampling proportion of rows. By default, all of the training data
  are used.

- validation:

  The *proportion* of the data that are used for performance assessment
  and potential early stopping.

- early_stop:

  An integer or `NULL`. If not `NULL`, it is the number of training
  iterations without improvement before stopping. If `validation` is
  used, performance is base on the validation set; otherwise, the
  training set is used.

- counts:

  A logical. If `FALSE`, `colsample_bynode` and `colsample_bytree` are
  both assumed to be *proportions* of the proportion of columns affects
  (instead of counts).

- ...:

  Other options to pass to `xgb.train()` or xgboost's method for
  [`predict()`](https://rdrr.io/r/stats/predict.html).

- object:

  An `xgboost` object.

- new_data:

  New data for predictions. Can be a data frame, matrix, `xgb.DMatrix`

## Value

A fitted `xgboost` object.

## Examples

``` r
if (interactive()) {
  us_deaths$off <- log(us_deaths$population)
  x <- model.matrix(~ age_group + gender + off, us_deaths)[, -1]

  mod <- xgb_train_offset(x, us_deaths$deaths, "off",
                          eta = 1, colsample_bynode = 1,
                          max_depth = 2, nrounds = 25,
                          counts = FALSE)

  xgb_predict_offset(mod, x, "off")
}
```
