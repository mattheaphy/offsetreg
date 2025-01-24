#' Boosted Poisson Trees with Offsets
#'
#' `boost_tree_offset()` defines a model that creates a series of Poisson
#' decision trees with pre-defined offsets forming an ensemble. Each tree
#' depends on the results of previous trees. All trees in the ensemble are
#' combined to produce a final prediction. This function can be used for count
#' regression models only.
#'
#' This function is similar to [parsnip::boost_tree()] except that
#' specification of an offset column is required.
#'
#' @param mode A single character string for the type of model. The only
#' possible value for this model is "regression"
#' @inheritParams parsnip::boost_tree
#'
#' @return A model specification object with the classes `boost_tree_offset` and
#' `model_spec`.
#'
#' @examples
#' parsnip::show_model_info("boost_tree_offset")
#'
#' boost_tree_offset()
#'
#' @seealso [parsnip::boost_tree()]
#' @export
boost_tree_offset <- function(mode = "regression",
                              engine = "xgboost_offset",
                              mtry = NULL,
                              trees = NULL,
                              min_n = NULL,
                              tree_depth = NULL,
                              learn_rate = NULL,
                              loss_reduction = NULL,
                              sample_size = NULL,
                              stop_iter = NULL) {

  if (mode  != "regression") {
    rlang::abort("`mode` should be 'regression'")
  }

  args <- list(mtry = rlang::enquo(mtry), trees = rlang::enquo(trees),
               min_n = rlang::enquo(min_n),
               tree_depth = rlang::enquo(tree_depth),
               learn_rate = rlang::enquo(learn_rate),
               loss_reduction = rlang::enquo(loss_reduction),
               sample_size = rlang::enquo(sample_size),
               stop_iter = rlang::enquo(stop_iter))

  # Save some empty slots for future parts of the specification
  new_model_spec(
    "boost_tree_offset",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = engine
  )
}

make_boost_tree_offset <- function() {
  if (is.null(get_model_env()[["boost_tree_offset"]])) {
    set_new_model("boost_tree_offset")
    set_model_mode(model = "boost_tree_offset", mode = "regression")
  }
}

#' @export
print.boost_tree_offset <- function(x, ...) {
  print_model_spec(x, desc = "Boosted Tree with Offsets", ...)

  invisible(x)
}

# code from the parsnip package
#' @export
update.boost_tree_offset <- function(object,
                              parameters = NULL,
                              mtry = NULL, trees = NULL, min_n = NULL,
                              tree_depth = NULL, learn_rate = NULL,
                              loss_reduction = NULL, sample_size = NULL,
                              stop_iter = NULL,
                              fresh = FALSE, ...) {

  args <- list(
    mtry = rlang::enquo(mtry),
    trees = rlang::enquo(trees),
    min_n = rlang::enquo(min_n),
    tree_depth = rlang::enquo(tree_depth),
    learn_rate = rlang::enquo(learn_rate),
    loss_reduction = rlang::enquo(loss_reduction),
    sample_size = rlang::enquo(sample_size),
    stop_iter = rlang::enquo(stop_iter)
  )

  update_spec(
    object = object,
    parameters = parameters,
    args_enquo_list = args,
    fresh = fresh,
    cls = "boost_tree_offset",
    ...
  )
}

# code from the parsnip package
#' @export
check_args.boost_tree_offset <- function(object, call = NULL) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$trees) && args$trees < 0) {
    rlang::abort("`trees` should be >= 1.")
  }
  if (is.numeric(args$sample_size) && (args$sample_size < 0 | args$sample_size > 1)) {
    rlang::abort("`sample_size` should be within [0,1].")
  }
  if (is.numeric(args$tree_depth) && args$tree_depth < 0) {
    rlang::abort("`tree_depth` should be >= 1.")
  }
  if (is.numeric(args$min_n) && args$min_n < 0) {
    rlang::abort("`min_n` should be >= 1.")
  }

  invisible(object)
}

#' @export
min_grid.boost_tree_offset <- function(x, grid, ...) {
  rlang::check_installed('tune')
  tune::fit_max_value(x, grid, ...)
}
