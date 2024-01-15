#' @export
glmnet_offset <- function(x, y, family, offset_col = "offset",
                          weights = NULL, lambda = NULL, alpha = 1) {

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
