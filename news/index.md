# Changelog

## offsetreg 1.1.1

CRAN release: 2025-03-02

- Behind-the-scenes compatibility update for parsnip 1.3.0, which is now
  the minimum required version.

## offsetreg 1.1.0

CRAN release: 2024-04-11

- [`boost_tree_offset()`](https://mattheaphy.github.io/offsetreg/reference/boost_tree_offset.md) -
  new model specification for boosted ensembles of decision trees.
  Currently xgboost (“xgboost_offset”) is supported.
- [`decision_tree_exposure()`](https://mattheaphy.github.io/offsetreg/reference/decision_tree_exposure.md) -
  new model specification for weighted decision trees with weighted
  exposures. Currently rpart (“rpart_exposure”) is supported.
- Added a vignette on when offsetreg should and shouldn’t be used.
- Added
  [`check_args()`](https://parsnip.tidymodels.org/reference/add_on_exports.html)
  methods to various model specifications.

## offsetreg 1.0.0

CRAN release: 2024-01-23

- Initial CRAN release
- offsetreg includes one model specification,
  [`poisson_reg_offset()`](https://mattheaphy.github.io/offsetreg/reference/poisson_reg_offset.md)
  with support for two engines:
  [`glm_offset()`](https://mattheaphy.github.io/offsetreg/reference/glm_offset.md)
  and
  [`glmnet_offset()`](https://mattheaphy.github.io/offsetreg/reference/glmnet_offset.md),
  which are wrappers around
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html) and
  [`glmnet::glmnet()`](https://rdrr.io/pkg/glmnet/man/glmnet.html),
  respectively.
