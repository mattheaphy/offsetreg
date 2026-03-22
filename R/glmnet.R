#' Fit Penalized Generalized Linear Models with an Offset
#'
#' This function is a wrapper around [glmnet::glmnet()] that uses a column from
#' `x` as an offset.
#'
#' @details
#' Outside of the `tidymodels` ecosystem, `glmnet_offset()` has no advantages
#' over [glmnet::glmnet()] since that function allows for offsets to be
#' specified in its `offset` argument.
#'
#' Within `tidymodels`, `glmnet_offset()` provides an advantage because it will
#' ensure that offsets are included in the data whenever resamples are created.
#'
#' The `x`, `y`, `family`, `lambda`, `alpha` and `weights` arguments have the
#' same meanings as [glmnet::glmnet()]. See that function's documentation for
#' full details.
#'
#' @param x Input matrix
#' @param y Response variable
#' @param lambda A numeric vector of regularization penalty values
#' @param alpha A number between zero and one denoting the proportion of L1
#' (lasso) versus L2 (ridge) regularization.
#' - `alpha = 1`: Pure lasso model
#' - `alpha = 0`: Pure ridge model
#' @inheritParams glm_offset
#'
#' @returns A `glmnet` object. See [glmnet::glmnet()] for full details.
#'
#' @examples
#' if (interactive()) {
#'   us_deaths$off <- log(us_deaths$population)
#'   x <- model.matrix(~ age_group + gender + off, us_deaths)[, -1]
#'   glmnet_offset(x, us_deaths$deaths, family = "poisson", offset_col = "off")
#' }
#'
#' @seealso [glmnet::glmnet()]
#' @export
glmnet_offset <- function(x, y, family, offset_col = "offset",
                          weights = NULL, lambda = NULL, alpha = 1) {

  rlang::check_installed("glmnet")

  if (!offset_col %in% colnames(x)) {
    rlang::abort(glue("A column named `{offset_col}` must be present."))
  }

  offsets <- x[, offset_col]
  x <- x[, !colnames(x) %in% offset_col, drop = FALSE]

  glmnet::glmnet(x, y, family = family, weights, offset = offsets,
                 lambda = lambda,
                 alpha = alpha)

}

# code from the parsnip package
set_glmnet_penalty_path <- function(x) {
  if (any(names(x$eng_args) == "path_values")) {
    # Since we decouple the parsnip `penalty` argument from being the same
    # as the glmnet `lambda` value, `path_values` allows users to set the
    # path differently from the default that glmnet uses. See
    # https://github.com/tidymodels/parsnip/issues/431
    x$method$fit$args$lambda <- x$eng_args$path_values
    x$eng_args$path_values <- NULL
    x$method$fit$args$path_values <- NULL
  } else {
    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    x$method$fit$args$lambda <- NULL
  }
  x
}
