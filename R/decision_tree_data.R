make_decision_tree_rpart_exposure <- function() {

  set_model_engine(
    "decision_tree_exposure",
    mode = "regression",
    eng = "rpart_exposure")

  set_dependency(
    "decision_tree_exposure",
    eng = "rpart_exposure",
    pkg = "rpart",
    mode = "regression"
  )

  set_dependency(
    "decision_tree_exposure",
    eng = "rpart_exposure",
    pkg = "offsetreg",
    mode = "regression"
  )

  set_model_arg(
    model = "decision_tree_exposure",
    eng = "rpart_exposure",
    parsnip = "cost_complexity",
    original = "cp",
    func = list(pkg = "dials", fun = "cost_complexity"),
    has_submodel = FALSE
  )

  set_model_arg(
    model = "decision_tree_exposure",
    eng = "rpart_exposure",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  set_model_arg(
    model = "decision_tree_exposure",
    eng = "rpart_exposure",
    parsnip = "tree_depth",
    original = "maxdepth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )

  set_fit(
    model = "decision_tree_exposure",
    eng = "rpart_exposure",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "offsetreg", fun = "rpart_exposure"),
      defaults = list(exposure_col = "exposure")
    )
  )

  set_encoding(
    model = "decision_tree_exposure",
    eng = "rpart_exposure",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  set_pred(
    model = "decision_tree_exposure",
    eng = "rpart_exposure",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = c(object = expr(object$fit),
               newdata = expr(new_data))
    )
  )

  set_pred(
    model = "decision_tree_exposure",
    eng = "rpart_exposure",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = c(object = expr(object$fit),
               newdata = expr(new_data))
    )
  )
}
