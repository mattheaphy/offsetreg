#' Poisson Decision Trees with Exposures
#'
#' `decision_tree_exposure()` defines a Poisson decision tree model with
#' weighted exposures (observation times).
#'
#' This function is similar to [parsnip::decision_tree()] except that
#' specification of an exposure column is required.
#'
#' @param mode A single character string for the type of model. The only
#' possible value for this model is "regression"
#'
#' @returns A model specification object with the classes
#' `decision_tree_exposure` and `model_spec`.
#'
#' @examples
#' parsnip::show_model_info("decision_tree_exposure")
#'
#' decision_tree_exposure()
#'
#' @inheritParams parsnip::decision_tree
#' @seealso [parsnip::decision_tree()]
#' @export
decision_tree_exposure <- function(mode = "regression",
                                   engine = "rpart_exposure",
                                   cost_complexity = NULL,
                                   tree_depth = NULL,
                                   min_n = NULL) {

  if (mode  != "regression") {
    rlang::abort("`mode` should be 'regression'")
  }

  args <- list(cost_complexity = rlang::enquo(cost_complexity),
               tree_depth = rlang::enquo(tree_depth),
               min_n = rlang::enquo(min_n))

  # Save some empty slots for future parts of the specification
  new_model_spec(
    "decision_tree_exposure",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = engine
  )
}


make_decision_tree_exposure <- function() {
  if (is.null(get_model_env()[["decision_tree_exposure"]])) {
    set_new_model("decision_tree_exposure")
    set_model_mode(model = "decision_tree_exposure", mode = "regression")
  }
}


# code from the parsnip package
#' @export
update.decision_tree_exposure <- function(object,
                                      parameters = NULL,
                                      cost_complexity = NULL,
                                      tree_depth = NULL,
                                      min_n = NULL,
                                      fresh = FALSE, ...) {

  args <- list(cost_complexity = rlang::enquo(cost_complexity),
               tree_depth = rlang::enquo(tree_depth),
               min_n = rlang::enquo(min_n))

  update_spec(
    object = object,
    parameters = parameters,
    args_enquo_list = args,
    fresh = fresh,
    cls = "decision_tree_exposure",
    ...
  )
}


#' @export
print.decision_tree_exposure <- function(x, ...) {
  print_model_spec(x, desc = "Poisson Decision Tree with Offsets", ...)

  invisible(x)
}

#' @export
min_grid.decision_tree_exposure <- function(x, grid, ...) {
  rlang::check_installed('tune')
  tune::fit_max_value(x, grid, ...)
}
