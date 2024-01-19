#' Poisson regression models with offsets
#'
#' `poisson_reg_offset()` defines a generalized linear model of count data with
#' an offset that follow a Poisson distribution.
#'
#' This function is similar to [parsnip::poisson_reg()] except that
#' specification of an offset column is required.
#'
#' @inheritParams parsnip::poisson_reg
#' @seealso [parsnip::poisson_reg()]
#' @export
poisson_reg_offset <- function(mode = "regression", penalty = NULL,
                               mixture = NULL, engine = "glm_offset") {

  if (mode  != "regression") {
    rlang::abort("`mode` should be 'regression'")
  }

  args <- list(penalty = rlang::enquo(penalty),
               mixture = rlang::enquo(mixture))

  # Save some empty slots for future parts of the specification
  new_model_spec(
    "poisson_reg_offset",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = engine
  )
}


make_poisson_reg_offset <- function() {
  if (is.null(get_model_env()[["poisson_reg_offset"]])) {
    set_new_model("poisson_reg_offset")
    set_model_mode(model = "poisson_reg_offset", mode = "regression")
  }
}


# code from the parsnip package
#' @export
translate.poisson_reg_offset <- function (x, engine = x$engine, ...) {
  x <- translate.default(x, engine, ...)
  if (engine == "glmnet_offset") {
    .check_glmnet_penalty_fit(x)
    x <- set_glmnet_penalty_path(x)
    x$args$penalty <- rlang::eval_tidy(x$args$penalty)
  }
  x
}

# code from the parsnip package
#' @export
update.poisson_reg_offset <- function(object,
                                      parameters = NULL,
                                      penalty = NULL, mixture = NULL,
                                      fresh = FALSE, ...) {

  args <- list(
    penalty = enquo(penalty),
    mixture = enquo(mixture)
  )

  update_spec(
    object = object,
    parameters = parameters,
    args_enquo_list = args,
    fresh = fresh,
    cls = "poisson_reg_offset",
    ...
  )
}


#' @export
print.poisson_reg_offset <- function(x, ...) {
  print_model_spec(x, desc = "Poisson Regression with Offsets", ...)

  invisible(x)
}
