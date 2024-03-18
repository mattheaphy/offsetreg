#' @noRd
make_poisson_reg_glm_offset <- function() {
  # add a new model to the env
  parsnip::set_model_engine(
    "poisson_reg_offset",
    mode = "regression",
    eng = "glm_offset"
  )
  # now there's a single row for the glm engine
  parsnip::set_dependency(
    "poisson_reg_offset",
    eng = "glm_offset",
    pkg = "stats",
    mode = "regression")
  parsnip::set_dependency(
    "poisson_reg_offset",
    eng = "glm_offset",
    pkg = "poissonreg",
    mode = "regression")
  parsnip::set_dependency(
    "poisson_reg_offset",
    eng = "glm_offset",
    pkg = "offsetreg",
    mode = "regression")

  parsnip::set_fit(
    model = "poisson_reg_offset",
    eng = "glm_offset",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "offsetreg", fun = "glm_offset"),
      defaults = list(family = "poisson", offset_col = "offset")
    )
  )

  parsnip::set_encoding(
    model = "poisson_reg_offset",
    eng = "glm_offset",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "poisson_reg_offset",
    eng = "glm_offset",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = .predict_pre_offset_rename,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
          type = "response"
        )
    )
  )

  parsnip::set_pred(
    model = "poisson_reg_offset",
    eng = "glm_offset",
    mode = "regression",
    type = "raw",
    value = list(
      pre = .predict_pre_offset_rename,
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = expr(object$fit), newdata = expr(new_data))
    )
  )
}


make_poisson_reg_glmnet_offset <- function() {

  parsnip::set_model_engine(
    "poisson_reg_offset",
    mode = "regression",
    eng = "glmnet_offset")

  parsnip::set_dependency(
    "poisson_reg_offset",
    eng = "glmnet_offset",
    pkg = "glmnet",
    mode = "regression"
  )
  parsnip::set_dependency(
    "poisson_reg_offset",
    eng = "glmnet_offset",
    pkg = "poissonreg",
    mode = "regression"
  )
  parsnip::set_dependency(
    "poisson_reg_offset",
    eng = "glmnet_offset",
    pkg = "offsetreg",
    mode = "regression"
  )

  parsnip::set_model_arg(
    model = "poisson_reg_offset",
    eng = "glmnet_offset",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = TRUE
  )

  parsnip::set_model_arg(
    model = "poisson_reg_offset",
    eng = "glmnet_offset",
    parsnip = "mixture",
    original = "alpha",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "poisson_reg_offset",
    eng = "glmnet_offset",
    mode = "regression",
    value = list(
      interface = "matrix",
      protect = c("x", "y", "weights"),
      func = c(pkg = "offsetreg", fun = "glmnet_offset"),
      defaults = list(family = "poisson", offset_col = "offset")
    )
  )

  parsnip::set_encoding(
    model = "poisson_reg_offset",
    eng = "glmnet_offset",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = TRUE
    )
  )

  pred_args <- list(
    object = expr(object$fit),
    newx = expr(as.matrix(new_data[, rownames(object$fit$beta), drop = FALSE])),
    newoffset = expr(new_data[, "offset", drop = TRUE])
  )

  parsnip::set_pred(
    model = "poisson_reg_offset",
    eng = "glmnet_offset",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = .predict_pre_offset_rename,
      post = parsnip::.organize_glmnet_pred,
      func = c(fun = "predict"),
      args = c(pred_args,
               type = "response",
               s = expr(object$spec$args$penalty)))
  )

  parsnip::set_pred(
    model = "poisson_reg_offset",
    eng = "glmnet_offset",
    mode = "regression",
    type = "raw",
    value = list(
      pre = .predict_pre_offset_rename,
      post = NULL,
      func = c(fun = "predict"),
      args = pred_args
    )
  )
}


# NULL coalesce function
`%||%` <- function(x, y) if (is.null(x)) y else x
