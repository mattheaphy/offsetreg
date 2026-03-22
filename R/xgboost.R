#' Boosted Poisson Trees with Offsets via `xgboost`
#'
#' `xgb_train_offset()` and `xgb_predict_offset()` are wrappers for `xgboost`
#' tree-based models where all of the model arguments are in the main function.
#' These functions are nearly identical to the parsnip functions
#' `parsnip::xgb_train()` and `parsnip::xg_predict_offset()` except that the
#' objective "count:poisson" is passed to `xgboost::xgb.train()` and an offset
#' term is added to the data set.
#'
#' @param y A vector (numeric) or matrix (numeric) of outcome data.
#' @param offset_col Character string. The name of a column in `data` containing
#' offsets.
#' @param weights A numeric vector of weights.
#' @param object An `xgboost` object.
#' @param new_data New data for predictions. Can be a data frame, matrix,
#' `xgb.DMatrix`
#' @inheritParams parsnip::xgb_train
#'
#' @return A fitted `xgboost` object.
#'
#' @examples
#' if (interactive()) {
#'   us_deaths$off <- log(us_deaths$population)
#'   x <- model.matrix(~ age_group + gender + off, us_deaths)[, -1]
#'
#'   mod <- xgb_train_offset(x, us_deaths$deaths, "off",
#'                           eta = 1, colsample_bynode = 1,
#'                           max_depth = 2, nrounds = 25,
#'                           counts = FALSE)
#'
#'   xgb_predict_offset(mod, x, "off")
#' }
#'
#' @export
xgb_train_offset <- function(
    x, y, offset_col = "offset", weights = NULL,
    max_depth = 6, nrounds = 15, eta  = 0.3, colsample_bynode = NULL,
    colsample_bytree = NULL, min_child_weight = 1, gamma = 0, subsample = 1,
    validation = 0, early_stop = NULL, counts = TRUE, ...) {

  rlang::check_installed("xgboost")

  others <- list(...)

  if (!is.numeric(validation) || validation < 0 || validation >= 1) {
    rlang::abort("`validation` should be on [0, 1).")
  }

  if (!is.null(early_stop)) {
    if (early_stop <= 1) {
      rlang::abort(paste0("`early_stop` should be on [2, ",  nrounds, ")."))
    } else if (early_stop >= nrounds) {
      early_stop <- nrounds - 1
      rlang::warn(paste0("`early_stop` was reduced to ", early_stop, "."))
    }
  }

  n <- nrow(x)
  # subtract one column for the offset
  p <- ncol(x) - 1L

  x <- as_xgb_data_offset(x, y, offset_col,
                          validation = validation,
                          weights = weights)


  if (!is.numeric(subsample) || subsample < 0 || subsample > 1) {
    rlang::abort("`subsample` should be on [0, 1].")
  }

  # initialize
  if (is.null(colsample_bytree)) {
    colsample_bytree <- 1
  } else {
    colsample_bytree <- recalc_param(colsample_bytree, counts, p)
  }
  if (is.null(colsample_bynode)) {
    colsample_bynode <- 1
  } else {
    colsample_bynode <- recalc_param(colsample_bynode, counts, p)
  }

  if (min_child_weight > n) {
    msg <- paste0(min_child_weight, " samples were requested but there were ",
                  n, " rows in the data. ", n, " will be used.")
    rlang::warn(msg)
    min_child_weight <- min(min_child_weight, n)
  }

  arg_list <- list(
    eta = eta,
    max_depth = max_depth,
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    colsample_bynode = colsample_bynode,
    min_child_weight = min(min_child_weight, n),
    subsample = subsample
  )

  others <- process_others(others, arg_list)

  main_args <- c(
    list(
      data = quote(x$data),
      watchlist = quote(x$watchlist),
      params = arg_list,
      nrounds = nrounds,
      early_stopping_rounds = early_stop,
      objective = "count:poisson"
    ),
    others
  )

  call <- make_call(fun = "xgb.train", ns = "xgboost", main_args)

  eval_tidy(call, env = rlang::current_env())
}

# Original code from parsnip - modified for offsets
as_xgb_data_offset <- function(x, y, offset_col,
                               validation = 0, weights = NULL) {

  n <- nrow(x)

  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  if (!offset_col %in% colnames(x)) {
    rlang::abort(glue("A column named `{offset_col}` must be present."))
  }
  offsets <- x[, offset_col]
  x <- x[, !colnames(x) %in% offset_col, drop = FALSE]

  # exit early for predictions where y's aren't supplied
  if (missing(y)) {
    return(xgboost::xgb.DMatrix(x, info = list(base_margin = offsets)))
  }

  if (!inherits(x, "xgb.DMatrix")) {
    if (validation > 0) {
      # Split data
      m <- floor(n * (1 - validation)) + 1
      trn_index <- sample(seq_len(n), size = max(m, 2))
      val_data <- xgboost::xgb.DMatrix(
        data = x[-trn_index, , drop = FALSE],
        label = y[-trn_index],
        missing = NA,
        info = list(base_margin = offsets[-trn_index])
      )
      watch_list <- list(validation = val_data)

      info_list <- list(label = y[trn_index],
                        base_margin = offsets[trn_index])
      if (!is.null(weights)) {
        info_list$weight <- weights[trn_index]
      }
      dat <- xgboost::xgb.DMatrix(
        data = x[trn_index, , drop = FALSE],
        missing = NA,
        info = info_list
      )

    } else {
      info_list <- list(label = y, base_margin = offsets)
      if (!is.null(weights)) {
        info_list$weight <- weights
      }
      dat <- xgboost::xgb.DMatrix(x, missing = NA, info = info_list)
      watch_list <- list(training = dat)
    }
  } else {
    dat <- xgboost::setinfo(x, "label", y)
    dat <- xgboost::setinfo(x, "base_margin", offsets)
    if (!is.null(weights)) {
      dat <- xgboost::setinfo(x, "weight", weights)
    }
    watch_list <- list(training = dat)
  }

  list(data = dat, watchlist = watch_list)
}

# code from the parsnip package
recalc_param <- function(x, counts, denom) {
  nm <- as.character(match.call()$x)
  if (is.null(x)) {
    x <- 1
  } else {
    if (counts) {
      maybe_proportion(x, nm)
      x <- min(denom, x) / denom
    }
  }
  x
}

# code from the parsnip package
maybe_proportion <- function(x, nm) {
  if (x < 1) {
    msg <- paste0(
      "The option `counts = TRUE` was used but parameter `", nm,
      "` was given as ", signif(x, 3), ". Please use a value >= 1 or use ",
      "`counts = FALSE`."
    )
    rlang::abort(msg)
  }
}

# code from the parsnip package with small edits
process_others <- function(others, arg_list) {

  guarded <- c("data", "weights", "watchlist", "objective")
  guarded_supplied <- names(others)[names(others) %in% guarded]

  n <- length(guarded_supplied)
  if (n > 0) {
    rlang::warn(
      c("!" = glue(
        "The following arguments are guarded and will not be passed to ",
        "`xgb.train()`: {paste(guarded_supplied, collapse = ', ')}.")
      ),
      class = "xgboost_guarded_warning"
    )
  }

  others <- others[!(names(others) %in% guarded)]

  if (!is.null(others$params)) {
    rlang::warn(
      c("!" = paste0("Please supply elements of the `params` list argument as ",
                     "main arguments to `set_engine()` rather than as part of ",
                     "`params`.")
      ),
      class = "xgboost_params_warning"
    )

    params <- others$params[!names(others$params) %in% names(arg_list)]
    others <- c(others[names(others) != "params"], params)
  }

  if (!(any(names(others) == "verbose"))) {
    others$verbose <- 0
  }

  others
}

#' @rdname xgb_train_offset
#' @export
xgb_predict_offset <- function(object, new_data, offset_col = "offset", ...) {

  if (!inherits(new_data, "xgb.DMatrix")) {
    new_data <- maybe_matrix(new_data) |>
      as_xgb_data_offset(offset_col = offset_col)
  } else {
    if (is.null(xgboost::getinfo(new_data, "base_margin"))) {
      rlang::abort(paste0("If `new_data` is an `xgb.DMatrix`, it must have an ",
                          "offset (base_margin) defined."))
    }
  }

  stats::predict(object, new_data, ...)

}
