make_boost_tree_xgboost_offset <- function() {

  set_model_engine(
    "boost_tree_offset",
    mode = "regression",
    eng = "xgboost_offset")

  set_dependency(
    "boost_tree_offset",
    eng = "xgboost_offset",
    pkg = "xgboost",
    mode = "regression"
  )

  set_dependency(
    "boost_tree_offset",
    eng = "xgboost_offset",
    pkg = "offsetreg",
    mode = "regression"
  )

  set_model_arg(
    model = "boost_tree_offset",
    eng = "xgboost_offset",
    parsnip = "tree_depth",
    original = "max_depth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  set_model_arg(
    model = "boost_tree_offset",
    eng = "xgboost_offset",
    parsnip = "trees",
    original = "nrounds",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = TRUE
  )
  set_model_arg(
    model = "boost_tree_offset",
    eng = "xgboost_offset",
    parsnip = "learn_rate",
    original = "eta",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )
  set_model_arg(
    model = "boost_tree_offset",
    eng = "xgboost_offset",
    parsnip = "mtry",
    original = "colsample_bynode",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  set_model_arg(
    model = "boost_tree_offset",
    eng = "xgboost_offset",
    parsnip = "min_n",
    original = "min_child_weight",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  set_model_arg(
    model = "boost_tree_offset",
    eng = "xgboost_offset",
    parsnip = "loss_reduction",
    original = "gamma",
    func = list(pkg = "dials", fun = "loss_reduction"),
    has_submodel = FALSE
  )
  set_model_arg(
    model = "boost_tree_offset",
    eng = "xgboost_offset",
    parsnip = "sample_size",
    original = "subsample",
    func = list(pkg = "dials", fun = "sample_size"),
    has_submodel = FALSE
  )
  set_model_arg(
    model = "boost_tree_offset",
    eng = "xgboost_offset",
    parsnip = "stop_iter",
    original = "early_stop",
    func = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )


  set_fit(
    model = "boost_tree_offset",
    eng = "xgboost_offset",
    mode = "regression",
    value = list(
      interface = "matrix",
      protect = c("x", "y", "weights"),
      func = c(pkg = "offsetreg", fun = "xgb_train_offset"),
      defaults = list(nthread = 1, verbose = 0, offset_col = "offset")
    )
  )

  set_encoding(
    model = "boost_tree_offset",
    eng = "xgboost_offset",
    mode = "regression",
    options = list(
      predictor_indicators = "one_hot",
      compute_intercept = FALSE,
      remove_intercept = TRUE,
      allow_sparse_x = TRUE
    )
  )

  set_pred(
    model = "boost_tree_offset",
    eng = "xgboost_offset",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = .predict_pre_offset_rename,
      post = NULL,
      func = c(fun = "xgb_predict_offset"),
      args = list(object = expr(object$fit), new_data = expr(new_data),
                  offset_col = "offset")
    )
  )

  set_pred(
    model = "boost_tree_offset",
    eng = "xgboost_offset",
    mode = "regression",
    type = "raw",
    value = list(
      pre = .predict_pre_offset_rename,
      post = NULL,
      func = c(fun = "xgb_predict_offset"),
      args = list(object = expr(object$fit), new_data = expr(new_data),
                  offset_col = "offset")
    )
  )

}
