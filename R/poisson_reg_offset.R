#' Poisson regression models with offsets
#'
#' `poisson_reg_offset()` defines a generalized linear model of count data with
#' an offset that follow a Poisson distribution.
#'
#' This function is similar to [parsnip::poisson_reg()] except that
#' specification of an offset column is required.
#'
#' @returns A model specification object with the classes `poisson_reg_offset`
#' and `model_spec`.
#'
#' @examples
#' parsnip::show_model_info("poisson_reg_offset")
#'
#' poisson_reg_offset()
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
check_args.poisson_reg_offset <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$penalty)) && any(args$penalty < 0))
    rlang::abort("The amount of regularization should be >= 0.")
  if (is.numeric(args$mixture) && (args$mixture < 0 | args$mixture > 1))
    rlang::abort("The mixture proportion should be within [0,1].")
  if (is.numeric(args$mixture) && length(args$mixture) > 1)
    rlang::abort("Only one value of `mixture` is allowed.")

  invisible(object)
}

# code from the parsnip package
#' @export
update.poisson_reg_offset <- function(object,
                                      parameters = NULL,
                                      penalty = NULL, mixture = NULL,
                                      fresh = FALSE, ...) {

  args <- list(
    penalty = rlang::enquo(penalty),
    mixture = rlang::enquo(mixture)
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

#' @export
min_grid.poisson_reg_offset <- function(x, grid, ...) {
  rlang::check_installed('tune')
  tune::fit_max_value(x, grid, ...)
}
