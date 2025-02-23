# offsetreg 1.1.1

- Behind-the-scenes compatibility update for parsnip 1.3.0, which is now the minimum required version.

# offsetreg 1.1.0

- `boost_tree_offset()` - new model specification for boosted ensembles of decision trees. Currently xgboost ("xgboost_offset") is supported.
- `decision_tree_exposure()` - new model specification for weighted decision trees with weighted exposures. Currently rpart ("rpart_exposure") is supported.
- Added a vignette on when offsetreg should and shouldn't be used.
- Added `check_args()` methods to various model specifications.


# offsetreg 1.0.0

- Initial CRAN release
- offsetreg includes one model specification, `poisson_reg_offset()` with support for two engines: `glm_offset()` and `glmnet_offset()`, which are wrappers around `stats::glm()` and `glmnet::glmnet()`, respectively.
