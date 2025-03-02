#' Poisson Recursive Partitioning and Regression Trees with Exposures
#'
#' This function is a wrapper around [rpart::rpart()] for Poisson regression
#' trees using weighted exposures (observation times).
#'
#' @details
#' Outside of the `tidymodels` ecosystem, `rpart_exposure()` has no
#' advantages over [rpart::rpart()] since that function allows for exposures to
#' be specified in the formula interface by passing `cbind(exposure, y)` as a
#' response variable.
#'
#' Within `tidymodels`, `rpart_exposure()` provides an advantage because
#' it will ensure that exposures are included in the data whenever resamples are
#' created.
#'
#' The `formula`, `data`, `weights`, `control`, and `cost` arguments have the
#' same meanings as [rpart::rpart()]. `shrink` is passed to [rpart::rpart()]'s
#' `parms` argument via a named list. See that function's documentation for full
#' details.
#'
#' @param formula A model formula that contains a single response variable on
#' the left-hand side.
#' @param data Optional. A data frame containing variables used in the model.
#' @param exposure_col Character string. The name of a column in `data`
#' containing exposures.
#' @param weights Optional weights to use in the fitting process.
#' @param control A list of hyperparameters. See [rpart::rpart.control()].
#' @param cost A vector of non-negative costs for each variable in the model.
#' @param shrink Optional parameter for the splitting function. Coefficient of
#' variation of the prior distribution.
#' @param ... Alternative input for arguments passed to
#' [rpart::rpart.control()].
#'
#' @returns An `rpart` model
#'
#' @examples
#' if (interactive()) {
#'   rpart_exposure(deaths ~ age_group + gender, us_deaths,
#'                  exposure_col = "population")
#' }
#'
#' @seealso [rpart::rpart()]
#' @export
rpart_exposure <- function(formula, data,
                           exposure_col = "exposure", weights = NULL,
                           control, cost, shrink = 1, ...) {

  rlang::check_installed("rpart")

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.")
  }
  if (!exposure_col %in% names(data)) {
    rlang::abort(glue("A column named `{exposure_col}` must be present."))
  }

  # rename the exposure column
  data <- .offset_rename(data, exposure_col, "exposure")

  # modify the formula to remove exposures from the right-hand side and
  #   add exposures to the left-hand side via cbind
  formula <- .formula_cbind_left(formula)

  # bind weights to the formula's environment to avoid an error in model.frame
  rlang::env_bind(environment(formula), weights = weights)

  rpart::rpart(formula, data = data, weights = weights,
               parms = list(shrink = shrink), cost = cost, control = control,
               method = "poisson", ...)

}

# internal function
.formula_cbind_left <- function(formula) {
  if (length(formula[[2]]) > 1) {
    rlang::abort(paste0("The left-hand side of `formula` must contain a single",
                        " response variable."))
  }
  formula_str <- as.character(formula)
  glue("cbind(exposure, {formula_str[[2]]}) ~ {formula_str[[3]]}") |>
    stats::as.formula()
}
