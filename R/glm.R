#' Fit Generalized Linear Models with an Offset
#'
#' This function is a wrapper around [stats::glm()] that uses a column from
#' `data` as an offset.
#'
#' @details
#' Outside of the `tidymodels` ecosystem, `glm_offset()` has no advantages over
#' [stats::glm()] since that function allows for offsets to be specified
#' in the formula interface or its `offset` argument.
#'
#' Within `tidymodels`, `glm_offset()` provides an advantage because it will
#' ensure that offsets are included in the data whenever resamples are created.
#'
#' The `formula`, `family`, `data`, and `weights` arguments have the same
#' meanings as [stats::glm()]. See that function's documentation for full
#' details.
#'
#' @param formula A model formula
#' @param family A function or character string describing the link function
#' and error distribution.
#' @param data Optional. A data frame containing variables used in the model.
#' @param offset_col Character string. The name of a column in `data` containing
#' offsets.
#' @param weights Weights to use in the fitting process.
#'
#' @returns A `glm` object. See [stats::glm()] for full details.
#'
#' @seealso [stats::glm()]
#' @export
glm_offset <- function(formula, family = "gaussian", data,
                       offset_col = "offset", weights) {

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.")
  }
  if (!offset_col %in% names(data)) {
    rlang::abort(glue("A column named `{offset_col}` must be present."))
  }

  # rename the offset column
  data <- .offset_rename(data, offset_col)
  # drop the offset term from the model formula
  formula_str <- as.character(formula)
  if (grepl("(\\s|^)\\.(\\s|$)", formula_str[[3]])) {
    # string manipulation is required because R will return an error
    # if update is called on a formula with `.` on the RHS
    formula <- paste(formula_str[[2]], "~", formula_str[[3]], "- offset") |>
      stats::as.formula()
  } else {
    formula <- stats::update(formula, ~ . - offset)
  }

  stats::glm(formula,
             family = family,
             offset = offset,
             data = data)

}

# internal function used to rename the specified offset column to "offset"
.offset_rename <- function(data, offset_col) {
  nm <- replace(names(data), names(data) == offset_col, "offset")
  stats::setNames(data, nm)
}

# internal function used to pre-process glm_offset data prior to predictions
.predict_pre_glm_offset <- function(data, object) {
  offset_col <- eval_tidy(object$spec$eng_args$offset_col) %||% "offset"
  .offset_rename(data, offset_col)
}
