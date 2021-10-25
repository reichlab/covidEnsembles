#' Check if object is of class qra_fit
#'
#' @param object an object that may be a qra_fit object
#'
#' @return boolean; whether object is inherits qra_fit class
#'
#' @export
is.qra_fit <- function(object) {
  if (inherits(object, "qra_fit")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Check if object is of class rescaled_qra_fit
#'
#' @param object an object that may be a rescaled_qra_fit object
#'
#' @return boolean; whether object is inherits rescaled_qra_fit class
#'
#' @export
is.rescaled_qra_fit <- function(object) {
  if (inherits(object, "rescaled_qra_fit")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Check if object is of class median_qra_fit
#'
#' @param object an object that may be a median_qra_fit object
#'
#' @return boolean; whether object is inherits median_qra_fit class
#'
#' @export
is.median_qra_fit <- function(object) {
  if (inherits(object, "median_qra_fit")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Check if object is of class weighted_median_qra_fit
#'
#' @param object an object that may be a weighted_median_qra_fit object
#'
#' @return boolean; whether object is inherits weighted_median_qra_fit class
#'
#' @export
is.weighted_median_qra_fit <- function(object) {
  if (inherits(object, "weighted_median_qra_fit")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Check if object is of class qenspy_qra_fit
#'
#' @param object an object that may be a qenspy_qra_fit object
#'
#' @return boolean; whether object is inherits qenspy_qra_fit class
#'
#' @export
is.qenspy_qra_fit <- function(object) {
  if (inherits(object, "qenspy_qra_fit")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Validate qra_fit object
#'
#' @param qra_fit
#'
#' @return invisible(TRUE) if qra_fit is valid;
#'   otherwise, an error is thrown
validate_qra_fit <- function(qra_fit) {
  message('validate_qra_fit not yet implemented')
}


#' Validate rescaled_qra_fit object
#'
#' @param rescaled_qra_fit
#'
#' @return invisible(TRUE) if qra_fit is valid;
#'   otherwise, an error is thrown
validate_rescaled_qra_fit <- function(rescaled_qra_fit) {
  message('validate_rescaled_qra_fit not yet implemented')
}


#' Create a qra_fit object
#'
#' @param parameters named list with two components:
#'   * 'intercept': data frame optionally with a column for quantile values,
#'     and corresponding intercepts
#'   * 'coefficients': data frame of models, optionally quantile values,
#'     and corresponding model coefficients
#' @param convex logical; if TRUE, predictions are done by passing weights
#'   through a softmax transformation; otherwise, no constraint is imposed.
#'
#' @return qra_fit object
#'
#' @export
new_qra_fit <- function(
  parameters,
  convex
) {
  qra_fit <- structure(
    parameters,
    convex=convex,
    class = 'qra_fit'
  )

#  validate_qra_fit(qra_fit)

  return(qra_fit)
}


#' Create a qenspy_qra_fit object
#'
#' @param param_vec numeric vector of "raw" parameter estimates
#' @param M integer number of models
#' @param model_names name for each model
#' @param quantile_levels numeric vector of quantile levels
#' @param quantile_groups number of quantile groups
#' @param combine_method string specifying combination method;
#' either "convex_mean" or "convex_median"
#' @param loss_trace numeric vector of loss trace per iteration of estimation
#'
#' @return qenspy_qra_fit object
#'
#' @export
new_qenspy_qra_fit <- function(
  param_vec,
  M,
  model_names = rep("", M),
  quantile_levels,
  quantile_groups,
  combine_method,
  loss_trace = NULL
) {
  qra_fit <- structure(
    list(
      param_vec = param_vec,
      M = M,
      model_names = model_names,
      quantile_levels = quantile_levels,
      quantile_groups = quantile_groups,
      combine_method = combine_method,
      loss_trace = loss_trace
    ),
    class = 'qenspy_qra_fit'
  )

#  validate_qenspy_qra_fit(qra_fit)

  return(qra_fit)
}


#' Create a rescaled_qra_fit object
#'
#' @param parameters named list with two components:
#'   * 'intercept': data frame optionally with a column for quantile values,
#'     and corresponding intercepts
#'   * 'coefficients': data frame of models, optionally quantile values,
#'     and corresponding model coefficients
#' @param convex logical; if TRUE, predictions are done by passing weights
#'   through a softmax transformation; otherwise, no constraint is imposed.
#'   Currently, only convex=TRUE is supported.
#'
#' @return rescaled_qra_fit object
#'
#' @export
new_rescaled_qra_fit <- function(
  parameters,
  convex=TRUE
) {
  if(!convex) {
    stop("only convex rescaled_qra_fit is supported")
  }
  rescaled_qra_fit <- structure(
    parameters,
    convex=convex,
    class = 'rescaled_qra_fit'
  )

  #  validate_qra_fit(rescaled_qra_fit)

  return(rescaled_qra_fit)
}




#' Create a weighted_median_qra_fit object
#'
#' @param parameters named list with two components:
#'   * 'intercept': data frame optionally with a column for quantile values,
#'     and corresponding intercepts
#'   * 'coefficients': data frame of models, optionally quantile values,
#'     and corresponding model coefficients
#'
#' @return rescaled_qra_fit object
#'
#' @export
new_weighted_median_qra_fit <- function(
  parameters
) {
  weighted_median_qra_fit <- structure(
    parameters,
    class = 'weighted_median_qra_fit'
  )

  #  validate_qra_fit(rescaled_qra_fit)

  return(weighted_median_qra_fit)
}


#' Create a median_qra_fit object
#'
#' @param ... any arguments are ignored
#'
#' @return median_qra_fit object
#'
#' @export
new_median_qra_fit <- function(...) {
  median_qra_fit <- structure(
    NA,
    class = 'median_qra_fit'
  )

  return(median_qra_fit)
}



#' Extract weights from a quantile regression averaging fit from qenspy
#'
#' @param qra_fit object of class qra_fit
#'
#' @return object of class QuantileForecastMatrix with quantile forecasts
#'
#' @export
extract_weights_qenspy_qra_fit <- function(qra_fit) {
  # create qenspy object of appropriate class
  qens <- reticulate::import("qenspy.qens", delay_load = TRUE)
  if (qra_fit$combine_method == "convex_mean") {
    qens_model <- qens$MeanQEns()
  } else if (qra_fit$combine_method == "convex_median") {
    qens_model <- qens$MedianQEns()
  } else {
    stop("combine_method must be convex_mean or convex_median")
  }

  weights <- qens_model$unpack_params(
    qra_fit$param_vec,
    qra_fit$M,
    qra_fit$quantile_groups)
  weights <- weights$w$numpy()

  weights_df <- weights %>%
    as.data.frame() %>%
    `colnames<-`(qra_fit$model_names) %>%
    dplyr::mutate(quantile_level = qra_fit$quantile_levels) %>%
    tidyr::pivot_longer(
      cols = qra_fit$model_names,
      names_to = "model",
      values_to = "weight")

  return(weights_df)
}


#' Predict based on a quantile regression averaging fit
#'
#' @param qra_fit object of class qra_fit
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#' @param sort_quantiles logical; if TRUE, the predictive quantiles are sorted
#' in order of the quantile level.  Otherwise, the raw predictive quantiles are
#' returned.
#'
#' @return object of class QuantileForecastMatrix with quantile forecasts
#'
#' @export
predict.qra_fit <- function(qra_fit, qfm, sort_quantiles = TRUE) {
  # construct sparse matrix representing model weights across quantiles
  ## pull out parameter values
  row_index <- attr(qfm, 'row_index')
  col_index <- attr(qfm, 'col_index')
  unique_models <- unique(col_index[[attr(qfm, 'model_col')]])
  unique_quantiles <- unique(col_index[[attr(qfm, 'quantile_name_col')]])

  row_inds <- seq_len(nrow(col_index))
  col_inds <- rep(seq_along(unique_quantiles), times=length(unique_models))
  if(attr(qfm, 'quantile_name_col') %in% names(qra_fit$coefficients)) {
    params <- qra_fit$coefficients$beta
  } else {
    params <- rep(qra_fit$coefficients$beta, each = length(unique_quantiles))
  }

  ## apply softmax constraint if required
  ## note in cases with the same weight for all quantiles, we could save some
  ## compute time by doing this before repeating for all quantiles above
  if(attr(qra_fit, 'convex')) {
    ### Convert to matrix where each column is one model, each row one quantile
    dim(params) <- c(length(unique_quantiles), length(unique_models))

    ### Apply softmax to each row; model weights sum to 1 within each quantile
    params <- softmax_matrix_rows(params)

    ### convert back to vector
    params <- as.numeric(params)
  }

  ## make sparse matrix of parameter values
  Beta <- Matrix::sparseMatrix(i=row_inds, j=col_inds, x=params)

  # Multiply quantiles by weights
  result_matrix <- as.matrix(Matrix::Matrix(unclass(qfm)) %*% Beta)

  # Add intercept
  if(!isTRUE(all.equal(qra_fit$intercept$beta,
                       rep(0.0, nrow(qra_fit$intercept))))) {
    if(attr(qfm, 'quantile_name_col') %in% names(qra_fit$intercept)) {
      intercept <- qra_fit$intercept$beta
    } else {
      intercept <- rep(qra_fit$intercept$beta,
        each = length(unique_quantiles))
    }

    result_matrix <- sweep(result_matrix, 2, intercept, FUN='+')
  }

  # Create QuantileForecastMatrix with result
  model_col <- attr(qfm, 'model_col')
  new_col_index <- col_index[
    col_index[[model_col]] == col_index[[model_col]][1], ]
  new_col_index[[model_col]] <- 'qra'

  result_qfm <- new_QuantileForecastMatrix(
    qfm = result_matrix,
    row_index=row_index,
    col_index=new_col_index,
    model_col=attr(qfm, 'model_col'),
    quantile_name_col=attr(qfm, 'quantile_name_col'),
    quantile_value_col=attr(qfm, 'quantile_value_col')
  )

  # sort predictive quantiles if requested
  if (sort_quantiles) {
    result_qfm <- sort(result_qfm)
  }

  return(result_qfm)
}


#' Predict based on a quantile regression averaging fit
#'
#' @param qra_fit object of class weighted_median_qra_fit
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#' @param sort_quantiles logical; if TRUE, the predictive quantiles are sorted
#' in order of the quantile level.  Otherwise, the raw predictive quantiles are
#' returned.
#'
#' @return object of class QuantileForecastMatrix with quantile forecasts
#'
#' @export
predict.weighted_median_qra_fit <- function(qra_fit, qfm, sort_quantiles = TRUE) {
  # construct matrix representing model weights across quantiles
  ## pull out parameter values
  row_index <- attr(qfm, 'row_index')
  col_index <- attr(qfm, 'col_index')
  unique_models <- unique(col_index[[attr(qfm, 'model_col')]])
  unique_quantiles <- unique(col_index[[attr(qfm, 'quantile_name_col')]])

  if(attr(qfm, 'quantile_name_col') %in% names(qra_fit$coefficients)) {
    params <- qra_fit$coefficients$beta
  } else {
    params <- rep(qra_fit$coefficients$beta, each = length(unique_quantiles))
  }

  ## Convert to matrix where each column is one model, each row one quantile
  dim(params) <- c(length(unique_quantiles), length(unique_models))

  # for each quantile level and row, calculate weighted median
  result_matrix <- matrix(NA, nrow = nrow(qfm), ncol = length(unique_quantiles))
  for (j in seq_along(unique_quantiles)) {
    col_inds <- which(col_index[[attr(qfm, 'quantile_name_col')]] == unique_quantiles[j])
    for (i in seq_len(nrow(qfm))) {
      result_matrix[i, j] = matrixStats::weightedMedian(
        x = unclass(qfm)[i, col_inds],
        w = params[j, ],
        na.rm = TRUE
      )
    }
  }

  # Create QuantileForecastMatrix with result
  model_col <- attr(qfm, 'model_col')
  new_col_index <- col_index[
    col_index[[model_col]] == col_index[[model_col]][1], ]
  new_col_index[[model_col]] <- 'qra'

  result_qfm <- new_QuantileForecastMatrix(
    qfm = result_matrix,
    row_index=row_index,
    col_index=new_col_index,
    model_col=attr(qfm, 'model_col'),
    quantile_name_col=attr(qfm, 'quantile_name_col'),
    quantile_value_col=attr(qfm, 'quantile_value_col')
  )

  # sort predictive quantiles if requested
  if (sort_quantiles) {
    result_qfm <- sort(result_qfm)
  }

  return(result_qfm)
}


#' Predict based on a quantile regression averaging fit
#'
#' @param qra_fit object of class qra_fit
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#' @param sort_quantiles logical; if TRUE, the predictive quantiles are sorted
#' in order of the quantile level.  Otherwise, the raw predictive quantiles are
#' returned.
#'
#' @return object of class QuantileForecastMatrix with quantile forecasts
#'
#' @export
predict.qenspy_qra_fit <- function(qra_fit, qfm, sort_quantiles) {
  # construct sparse matrix representing model weights across quantiles
  ## pull out parameter values
  row_index <- attr(qfm, 'row_index')
  col_index <- attr(qfm, 'col_index')
  num_models <- qra_fit$M

  quantiles <- col_index[[attr(qfm, 'quantile_name_col')]] %>%
    unique() %>%
    sort() %>%
    as.numeric()
  num_quantiles <- length(quantiles)

  qarr <- unclass(qfm)
  dim(qarr) <- c(nrow(qarr), num_quantiles, num_models)

  # create qenspy object of appropriate class
  qens <- reticulate::import("qenspy.qens", delay_load = TRUE)
  if (qra_fit$combine_method == "convex_mean") {
    qens_model <- qens$MeanQEns()
  } else if (qra_fit$combine_method == "convex_median") {
    qens_model <- qens$MedianQEns()
  } else {
    stop("combine_method must be convex_mean or convex_median")
  }

  # get_predictions
  predictions_raw <- qens_model$predict(
    q = qarr,
    w = qens_model$unpack_params(
      qra_fit$param_vec,
      M = num_models,
      tau_groups = qra_fit$quantile_groups
    )
  )
  result_matrix <- predictions_raw$numpy()
  
  # Create QuantileForecastMatrix with result
  model_col <- attr(qfm, 'model_col')
  new_col_index <- col_index[
    col_index[[model_col]] == col_index[[model_col]][1], ]
  new_col_index[[model_col]] <- 'qra'

  result_qfm <- new_QuantileForecastMatrix(
    qfm = result_matrix,
    row_index = row_index,
    col_index = new_col_index,
    model_col = attr(qfm, 'model_col'),
    quantile_name_col = attr(qfm, 'quantile_name_col'),
    quantile_value_col = attr(qfm, 'quantile_value_col')
  )

  # sort predictive quantiles if requested
  if (sort_quantiles) {
    result_qfm <- sort(result_qfm)
  }

  return(result_qfm)
}


#' Predict based on a rescaled quantile regression averaging fit
#'
#' @param qra_fit_rescalable object of class qra_fit_rescalable
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#' @param sort_quantiles logical; if TRUE, the predictive quantiles are sorted
#' in order of the quantile level.  Otherwise, the raw predictive quantiles are
#' returned.
#' @param ... ignored additional arguments
#'
#' @return object of class QuantileForecastMatrix with quantile forecasts
#'
#' @export
predict.rescaled_qra_fit <- function(qra_fit, qfm, sort_quantiles = TRUE, ...) {
  # construct matrix representing model weights across quantiles
  ## pull out parameter values
  row_index <- attr(qfm, 'row_index')
  col_index <- attr(qfm, 'col_index')
  unique_models <- unique(col_index[[attr(qfm, 'model_col')]])
  unique_quantiles <- unique(col_index[[attr(qfm, 'quantile_name_col')]])

  if(attr(qfm, 'quantile_name_col') %in% names(qra_fit$coefficients)) {
    params <- qra_fit$coefficients$beta
  } else {
    params <- rep(qra_fit$coefficients$beta, each = length(unique_quantiles))
  }

  ## Convert to matrix where each column is one model, each row one quantile
  dim(params) <- c(length(unique_quantiles), length(unique_models))

  # for each quantile level and row, calculate weighted median
  result_matrix <- matrix(NA, nrow = nrow(qfm), ncol = length(unique_quantiles))
  for (j in seq_along(unique_quantiles)) {
    col_inds <- which(col_index[[attr(qfm, 'quantile_name_col')]] == unique_quantiles[j])
    for (i in seq_len(nrow(qfm))) {
      result_matrix[i, j] = weighted.mean(
        x = unclass(qfm)[i, col_inds],
        w = params[j, ],
        na.rm = TRUE
      )
    }
  }

  # Create QuantileForecastMatrix with result
  model_col <- attr(qfm, 'model_col')
  new_col_index <- col_index[
    col_index[[model_col]] == col_index[[model_col]][1], ]
  new_col_index[[model_col]] <- 'qra'

  result_qfm <- new_QuantileForecastMatrix(
    qfm = result_matrix,
    row_index=row_index,
    col_index=new_col_index,
    model_col=attr(qfm, 'model_col'),
    quantile_name_col=attr(qfm, 'quantile_name_col'),
    quantile_value_col=attr(qfm, 'quantile_value_col')
  )

  # sort predictive quantiles if requested
  if (sort_quantiles) {
    result_qfm <- sort(result_qfm)
  }

  return(result_qfm)
}



#' Predict based on a quantile regression averaging fit
#'
#' @param qra_fit object of class median_qra_fit
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#' @param ... ignored additional arguments (in particular, absorbs the
#' sort_quantiles argument used by \code{predict.qra_fit})
#'
#' @return object of class QuantileForecastMatrix with quantile forecasts
#'
#' @export
predict.median_qra_fit <- function(qra_fit, qfm, ...) {
  # construct sparse matrix representing model weights across quantiles
  ## pull out parameter values
  row_index <- attr(qfm, 'row_index')
  col_index <- attr(qfm, 'col_index')
  unique_models <- unique(col_index[[attr(qfm, 'model_col')]])
  unique_quantiles <- unique(col_index[[attr(qfm, 'quantile_name_col')]])

  col_inds <- rep(seq_along(unique_quantiles), times=length(unique_models))

  # Compute result as per-quantile median of component forecasts
  result_matrix <- matrix(
    NA_real_,
    nrow = nrow(row_index),
    ncol = length(unique_quantiles)
  )

  for(quantile_ind in seq_along(unique_quantiles)) {
    result_matrix[, quantile_ind] <- apply(
      unclass(qfm)[, which(col_inds == quantile_ind), drop = FALSE],
      1,
      median,
      na.rm = TRUE
    )
  }

  # Create QuantileForecastMatrix with result and return
  model_col <- attr(qfm, 'model_col')
  new_col_index <- col_index[
    col_index[[model_col]] == col_index[[model_col]][1], ]
  new_col_index[[model_col]] <- 'qra'

  result_qfm <- new_QuantileForecastMatrix(
    qfm = result_matrix,
    row_index=row_index,
    col_index=new_col_index,
    model_col=attr(qfm, 'model_col'),
    quantile_name_col=attr(qfm, 'quantile_name_col'),
    quantile_value_col=attr(qfm, 'quantile_value_col')
  )

  return(result_qfm)
}


#' Estimate Quantile Regression Averaging model
#'
#' @param qfm_train QuantileForecastMatrix with training set predictions
#' @param y_train numeric vector of responses for training set
#' @param qfm_test QuantileForecastMatrix with test set predictions
#' @param intercept logical specifying whether an intercept is included
#' @param combine_method character specifying the approach to model
#' combination: "equal", "convex", "positive", "unconstrained", or "median".
#' The first four form a linear combination of quantiles across component
#' models with varying levels of restrictions on the combination coefficients.
#' "median" takes the median across models at each quantile level.
#' @param quantile_groups Vector of group labels for quantiles, having the same
#' length as the number of quantiles.  Common labels indicate that the ensemble
#' weights for the corresponding quantile levels should be tied together.
#' Default is rep(1,length(quantiles)), which means that a common set of
#' ensemble weights should be used across all levels.  This is the argument
#' `tau_groups` for `quantgen::quantile_ensemble`, and may only be supplied if
#' `backend = 'quantgen`
#' @param noncross string specifying approach to handling quantile noncrossing:
#' one of "constrain" or "sort". "constrain" means estimation is done subject
#' to constraints ruling out quantile crossing.  "sort" means no such
#' constraints are imposed during estimation, but the resulting forecasts are
#' sorted.
#' @param backend implementation used for estimation; currently either
#'    'optim', using L-BFGS-B as provided by the optim function in R;
#'    'NlcOptim', using NlcOptim::solnl; or 'quantgen', using
#'    quantgen::quantile_ensemble
#'
#' @return object of class qra_fit
#'
#' @export
estimate_qra <- function(
  qfm_train,
  y_train,
  qfm_test = NULL,
  intercept = FALSE,
  combine_method = c("ew", "convex", "positive", "unconstrained", "median"),
  quantile_groups = NULL,
  noncross = "constrain",
  backend = 'optim',
  partial_save_frequency,
  partial_save_filename,
  ...
) {
  combine_method <- match.arg(
    combine_method,
    choices = c("ew", "convex", "positive", "unconstrained", "median",
      "convex_median", "rel_wis_weighted_median", "rel_wis_weighted_mean",
      "mean_weights_weighted_median"))
  backend <- match.arg(
    backend,
    choices = c("optim", "NlcOptim", "qra", "quantgen", "qenspy", "grid_search"))

  if (backend == "qenspy") {
    combine_method <- match.arg(
      combine_method,
      choices = c("convex", "convex_median", "mean_weights_weighted_median"))
    if (combine_method == "convex") {
      combine_method <- qenspy_combine_method <- "convex_mean"
    } else if (combine_method == "mean_weights_weighted_median") {
      qenspy_combine_method <- "convex_mean"
    }

    if (noncross != "sort") {
      stop('For backend "qenspy", noncross method must be "sort"')
    }
    result <- estimate_qra_qenspy(
      qfm_train = qfm_train,
      y_train = y_train,
      combine_method = qenspy_combine_method,
      quantile_groups = quantile_groups,
      partial_save_frequency = partial_save_frequency,
      partial_save_filename = partial_save_filename,
      ...)
  } else if(backend == "quantgen") {
    combine_method <- match.arg(
      combine_method,
      choices = c("convex", "positive", "unconstrained"))
    result <- estimate_qra_quantgen(
      qfm_train = qfm_train,
      y_train = y_train,
      qfm_test = qfm_test,
      intercept = intercept,
      constraint = combine_method,
      quantile_groups = quantile_groups,
      noncross = noncross)
    col_index <- attr(qfm_train, "col_index")
  } else if (backend == "qra") {
    stop("backend = 'qra' is not yet supported")
    qra_data <-
      result <- qra:::qra_estimate_weights(
        x = qra_data,
        per_quantile_weights = per_quantile_weights,
        enforce_normalisation = (combine_method == "convex")
      )
  } else {
    if(combine_method == "ew") {
      result <- estimate_qra_ew(qfm_train, ...)
    } else {
      # stop("backend not supported")
      result <- estimate_qra_optimized(
        qfm_train = qfm_train,
        y_train = y_train,
        quantile_groups = quantile_groups,
        qra_model = combine_method,
        backend = backend)
    }
  }

  # convert to weighted median if combine method is mean_weights_weighted_median
  if (combine_method == "mean_weights_weighted_median") {
    if (backend == "qenspy") {
      coefficients <- extract_weights_qenspy_qra_fit(result)
      colnames(coefficients) <- c(
        attr(qfm_train, "quantile_name_col"),
        attr(qfm_train, "model_col"),
        "beta"
      )
    } else {
      stop("For mean_weights_weighted_median method, only backend qenspy is supported.")
    }
    # coefficients <- data.frame(
    #   a = unique_models,
    #   beta = weights,
    #   stringsAsFactors = FALSE
    # )
    # colnames(coefficients)[1] <- model_col

    result <- new_weighted_median_qra_fit(
      parameters = list(coefficients = coefficients)
    )
  }

  return(result)
}


#' Estimate Quantile Regression Averaging model with equal weights
#'
#' @param qfm_train
#'
#' @return object of class qra_fit
#'
#' @export
estimate_qra_ew <- function(qfm_train, ...) {
  col_index <- attr(qfm_train, 'col_index')
  model_col <- attr(qfm_train, 'model_col')
  unique_models <- unique(col_index[[model_col]])

  coefficients <- data.frame(
    a = unique_models,
    beta = 1 / length(unique_models),
    stringsAsFactors = FALSE
  )
  colnames(coefficients)[1] <- model_col
  intercept <- data.frame(beta = 0.0, stringsAsFactors = FALSE)

  qra_fit <- new_qra_fit(
    parameters = list(coefficients=coefficients, intercept=intercept),
    convex = FALSE
  )

  return(qra_fit)
}


#' Estimate qra parameters by optimizing weighted interval score loss
#'
#' @param qfm_train QuantileForecastMatrix with training set predictions from
#'    component models
#' @param y_train numeric vector of responses for training set
#' @param quantile_groups Vector of group labels for quantiles, having the same
#' length as the number of quantiles.  Common labels indicate that the ensemble
#' weights for the corresponding quantile levels should be tied together.
#' Default is rep(1,length(quantiles)), which means that a common set of
#' ensemble weights should be used across all levels.  This is the argument
#' `tau_groups` for `quantgen::quantile_ensemble`, and may only be supplied if
#' `backend = 'quantgen`
#' @param qra_model quantile averaging model
#' @param backend implementation used for estimation; currently either
#'    'optim', using L-BFGS-B as provided by the optim function in R, or
#'    'NlcOptim', using NlcOptim::solnl, or 'grid_search', using
#'    covidEnsemble::grid_search
#'
#' @return object of class qra_fit
#'
#' @export
estimate_qra_optimized <- function(
  qfm_train,
  y_train,
  quantile_groups,
  qra_model = c('convex_per_model', 'unconstrained_per_model', 'rescaled_convex_per_model', 'rel_wis_weighted_median'),
  backend = c('optim', 'NlcOptim', 'grid_search')
) {
  qra_model <- match.arg(qra_model, choices = c('convex_per_model',
    'unconstrained_per_model', 'rescaled_convex_per_model',
    'rel_wis_weighted_median', 'rel_wis_weighted_mean',
    'mean_weights_weighted_median'))
  backend <- match.arg(backend, choices = c('optim', 'NlcOptim', 'grid_search'))

  if (backend %in% c('optim', 'NlcOptim')) {
    init_par_constructor <- getFromNamespace(
      paste0('init_par_constructor_', qra_model),
      ns='covidEnsembles'
    )
  } else if(backend == 'grid_search') {
    init_par_constructor <- getFromNamespace(
      paste0('par_grid_constructor_', qra_model),
      ns='covidEnsembles'
    )
  }
  model_constructor <- getFromNamespace(
    paste0('model_constructor_', qra_model),
    ns='covidEnsembles'
  )

  # do estimation separately for each quantile group, then recombine
  col_index <- attr(qfm_train, 'col_index')
  quantile_name_col <- attr(qfm_train, 'quantile_name_col')
  quantiles <- sort(unique(col_index[[quantile_name_col]]))
  model_fit_per_quantile_group <- purrr::map(
    unique(quantile_groups),
    function(quantile_group) {
      # extract quantile levels for this group
      quantile_group_quantiles <-
        quantiles[which(quantile_groups == quantile_group)]
      
      # create a QFM with just the predictive quantiles for this group
      col_inds <- which(col_index[[quantile_name_col]] %in%
                        quantile_group_quantiles)
      qg_qfm_train <- qfm_train[, col_inds]

      # do estimation for this quantile group
      init_par <- init_par_constructor(qfm_train, y_train)
      if (backend == 'optim') {
        if (qra_model == 'rel_wis_weighted_median') {
          optim_method <- 'SANN'
        } else {
          optim_method <- 'L-BFGS-B'
        }
        optim_output <- optim(
          par=init_par,
          fn=wis_loss,
          model_constructor=model_constructor,
          qfm_train=qg_qfm_train,
          y_train=y_train,
          method=optim_method)

        if(optim_output$convergence != 0) {
          warning('optim convergence non-zero; estimation may have failed!')
        }
      } else if (backend == 'NlcOptim') {
        wis_loss_wrapper <- function(x) {
          wis_loss(
            x,
            model_constructor=model_constructor,
            qfm_train=qg_qfm_train,
            y_train=y_train)
        }
        optim_output <- NlcOptim::solnl(
          X=init_par,
          objfun=wis_loss_wrapper
        )
      } else if (backend == 'grid_search') {
        if (qra_model %in% c('rel_wis_weighted_median', 'rel_wis_weighted_mean')) {
          optim_output <- grid_search_rel_wis_weights(
            par_grid=init_par,
            fn=wis_loss,
            model_constructor=model_constructor,
            qfm_train=qg_qfm_train,
            y_train=y_train)
        } else {
          stop('grid search backend is only available if model is rel_wis_weighted_median')
        }
      }

      model_fit = model_constructor(optim_output$par, qg_qfm_train, y_train)
      if (backend == 'grid_search') {
        model_fit['losses'] <- optim_output['losses']
        model_fit['rel_wis'] <- optim_output['rel_wis']
      }

      return(list(
        quantile_levels = quantile_group_quantiles,
        model_fit = model_fit
      ))
    })
  
  # assemble into a single fit
  if (length(unique(quantile_groups)) == 1) {
    return(model_fit_per_quantile_group[[1]]$model_fit)
  } else {
    # the following is specific to the `relative_wis_weighted_median` approach
    # other weighted approaches are expected to be fit using other backends
    # we could add support for other methods without too much effort though...
    if (combine_method != "rel_wis_weighted_median") {
      stop("For multiple quantile groups and optimization within R, must have combine_method = relative_wis_weighted_median")
    }
    combined_coefficients <- purrr::map_dfr(
      model_fit_per_quantile_group,
      function(model_fit) {
        quantile_levels <- model_fit$quantile_levels
        model_fit <- model_fit$model_fit
        # get updated model fit coefficients table with all combinations of
        # quantile level and model
        new_coefficients <- model_fit$coefficients %>%
          dplyr::mutate(join_field = "a") %>%
          dplyr::full_join(
            data.frame(
              quantile = quantile_levels,
              join_field = "a"
            ),
            by = "join_field"
          ) %>%
          dplyr::select(model, quantile, beta)
        return(new_coefficients)
      }
    )
    combined_par <- sapply(
      model_fit_per_quantile_group,
      function(model_fit) { model_fit$model_fit$par }
    )
    combined_model_fit <- model_fit_per_quantile_group[[1]]$model_fit
    combined_model_fit$coefficients <- combined_coefficients
    combined_model_fit$par <- combined_par
    return(combined_model_fit)
  }
}


#' Conduct a grid search by evaluating the objective function at all parameter
#' values in the supplied parameter grid and selecting the parameter value
#' with lowest function value.  Works only with one-dimensional parameters.
#' 
#' @param par_grid numeric vector of parameter values to evaluate
#' @param fn objective function to minimize
#' @param model_constructor function accepts a real-valued parameter and
#' returns a model of class qra_fit
#' @param qfm_train QuantileForecastMatrix with training set predictions from
#'    component models
#' @param y_train numeric vector of responses for training set
#' 
#' @return single parameter value with lowest function value
grid_search_rel_wis_weights <- function(
  par_grid,
  fn,
  model_constructor,
  qfm_train,
  y_train) {
  rel_wis <- calc_relative_wis(y_train, qfm_train)

  loss_by_par <- furrr::future_map_dbl(
    par_grid,
    fn,
    model_constructor = model_constructor,
    qfm_train = qfm_train,
    y_train = y_train,
    rel_wis = rel_wis,
    .progress = TRUE)

  min_ind <- which.min(loss_by_par)

  num_exts <- 0
  while (
    (loss_by_par[length(par_grid)] - loss_by_par[min_ind] < .2*(loss_by_par[1] - loss_by_par[min_ind])) &
    num_exts < 6
  ) {
    num_exts <- num_exts + 1
    ext_grid <- par_grid[length(par_grid)] + (1:50)*min(diff(par_grid))
    par_grid <- c(par_grid, ext_grid)
    loss_by_par_ext <- furrr::future_map_dbl(
      ext_grid,
      fn,
      model_constructor = model_constructor,
      qfm_train = qfm_train,
      y_train = y_train,
      rel_wis = rel_wis,
      .progress = TRUE)
    loss_by_par <- c(loss_by_par, loss_by_par_ext)
    min_ind <- which.min(loss_by_par)
  }

  return(list(
    par = par_grid[min_ind],
    losses = tibble(theta = par_grid, loss = loss_by_par),
    rel_wis = as_tibble(rel_wis)
    )
  )
}


#' Calculate wis_loss as a function of parameters used to construct a qra model
#'
#' @param par real-valued vector of parameters
#' @param model_constructor a function that accepts a real-valued vector of
#'    parameters and returns a model of class qra_fit
#' @param qfm_train QuantileForecastMatrix with training set predictions from
#'    component models
#' @param y_train numeric vector of responses for training set
#' @param ... arguments passed on to the model_constructor
#'
#' @return scalar wis loss for given parameter values
wis_loss <- function(par, model_constructor, qfm_train, y_train, ...) {
  qra_model <- model_constructor(par, qfm_train, y_train, ...)
  qfm_aggregated <- predict(qra_model, qfm_train)
  return(sum(covidEnsembles::wis(y_train, qfm_aggregated)))
}


#' Initial parameter constructor for qra_convex_per_model approach
#'
#' @param qfm_train QuantileForecastMatrix with training set predictions from
#'    component models
#' @param ... mop up other arguments that are ignored
#'
#' @return vector of real-valued initial values for parameters
init_par_constructor_convex_per_model <- function(qfm_train, ...) {
  # extract number of unique models in qfm_train
  col_index <- attr(qfm_train, 'col_index')
  model_col <- attr(qfm_train, 'model_col')

  unique_models <- unique(col_index[[model_col]])
  M <- length(unique_models)

  # initial parameter values are 0 for each model;
  # after softmax, this corresponds to weight 1/M for each model
  init_par <- rep(0.0, M)

  return(init_par)
}


#' Model constructor for convex_per_model approach
#'
#' @param par vector of real numbers
#' @param qfm_train object of class QuantileForecastMatrix
#'
#' @return object of class qra_fit
#'
#' @export
model_constructor_convex_per_model <- function(par, qfm_train) {
  col_index <- attr(qfm_train, 'col_index')
  model_col <- attr(qfm_train, 'model_col')
  unique_models <- unique(col_index[[model_col]])

  coefficients <- data.frame(
    a = unique_models,
    beta = par,
    stringsAsFactors = FALSE
  )
  colnames(coefficients)[1] <- model_col
  intercept <- data.frame(beta = 0.0, stringsAsFactors = FALSE)

  qra_fit <- new_qra_fit(
    parameters = list(coefficients=coefficients, intercept=intercept),
    convex = TRUE
  )

  return(qra_fit)
}



#' Initial parameter constructor for rescaled_convex_per_model approach
#'
#' @param qfm_train QuantileForecastMatrix with training set predictions from
#'    component models
#' @param ... mop up other arguments that are ignored
#'
#' @return vector of real-valued initial values for parameters
init_par_constructor_rescaled_convex_per_model <- function(qfm_train, ...) {
  # extract number of unique models in qfm_train
  col_index <- attr(qfm_train, 'col_index')
  model_col <- attr(qfm_train, 'model_col')
  unique_models <- unique(col_index[[model_col]])
  M <- length(unique_models)

  # initial parameter values are 0 for each model;
  # after softmax, this corresponds to weight 1/M for each model
  init_par <- rep(0.0, M)

  return(init_par)
}

#' Model constructor for rescaled_convex_per_model approach
#'
#' @param par vector of real numbers
#' @param qfm_train object of class QuantileForecastMatrix
#'
#' @return object of class rescaled_qra_fit
#'
#' @export
model_constructor_rescaled_convex_per_model <- function(par, qfm_train) {
  col_index <- attr(qfm_train, 'col_index')
  model_col <- attr(qfm_train, 'model_col')
  unique_models <- unique(col_index[[model_col]])

  coefficients <- data.frame(
    a = unique_models,
    beta = par,
    stringsAsFactors = FALSE
  )
  colnames(coefficients)[1] <- model_col
  intercept <- data.frame(beta = 0.0, stringsAsFactors = FALSE)

  rescaled_qra_fit <- new_rescaled_qra_fit(
    parameters = list(coefficients=coefficients, intercept=intercept),
    convex = TRUE
  )

  return(rescaled_qra_fit)
}


#' Initial parameter constructor for unconstrained_per_model approach
#'
#' @param qfm_train QuantileForecastMatrix with training set predictions from
#'    component models
#' @param ... mop up other arguments that are ignored
#'
#' @return vector of real-valued initial values for parameters
init_par_constructor_unconstrained_per_model <- function(qfm_train, ...) {
  # extract number of unique models in qfm_train
  col_index <- attr(qfm_train, 'col_index')
  model_col <- attr(qfm_train, 'model_col')
  unique_models <- unique(col_index[[model_col]])
  M <- length(unique_models)

  # initial parameter values are 0 for intercept and 1/M for each model
  init_par <- c(0.0, rep(1/M, M))

  return(init_par)
}


#' Model constructor for unconstrained_per_model approach
#'
#' @param par vector of real numbers
#' @param qfm_train object of class QuantileForecastMatrix
#'
#' @return object of class qra_fit
#'
#' @export
model_constructor_unconstrained_per_model <- function(par, qfm_train) {
  col_index <- attr(qfm_train, 'col_index')
  model_col <- attr(qfm_train, 'model_col')
  unique_models <- unique(col_index[[model_col]])

  coefficients <- data.frame(
    a = unique_models,
    beta = par[seq_along(unique_models)],
    stringsAsFactors = FALSE
  )
  colnames(coefficients)[1] <- model_col
  intercept <- data.frame(beta = par[length(unique_models)+1],
                          stringsAsFactors = FALSE)

  qra_fit <- new_qra_fit(
    parameters = list(coefficients=coefficients, intercept=intercept),
    convex = FALSE
  )

  return(qra_fit)
}


#' Initial parameter constructor for rel_wis_weighted_median approach
#'
#' @param qfm_train QuantileForecastMatrix with training set predictions from
#'    component models
#' @param ... mop up other arguments that are ignored
#'
#' @return vector of real-valued initial values for parameters
init_par_constructor_rel_wis_weighted_median <- function(qfm_train, ...) {
  return(0.0)
}


#' Parameter search grid constructor for rel_wis_weighted_median approach
#'
#' @param qfm_train QuantileForecastMatrix with training set predictions from
#'    component models
#' @param ... mop up other arguments that are ignored
#'
#' @return vector of real-valued initial values for parameters
par_grid_constructor_rel_wis_weighted_median <- function(qfm_train, ...) {
  return(seq(from = 0.0, to = 20.0, by = 0.1))
}


#' Model constructor for rel_wis_weighted_median approach
#'
#' @param par vector of real numbers
#' @param qfm_train object of class QuantileForecastMatrix
#'
#' @return object of class qra_fit
#'
#' @export
model_constructor_rel_wis_weighted_median <- function(par, qfm_train, y_train, rel_wis = calc_relative_wis(y_train, qfm_train)) {
  col_index <- attr(qfm_train, 'col_index')
  model_col <- attr(qfm_train, 'model_col')
  unique_models <- unique(col_index[[model_col]])

  rel_wis_vec <- data.frame(model = unique_models) %>%
    dplyr::left_join(rel_wis, by = 'model') %>%
    dplyr::pull(rel_wis)
  weights <- softmax_matrix_rows(
    matrix(-1 * par * rel_wis_vec, nrow = 1)
  )
  dim(weights) <- prod(dim(weights))

  coefficients <- data.frame(
    a = unique_models,
    beta = weights,
    stringsAsFactors = FALSE
  )
  colnames(coefficients)[1] <- model_col

  qra_fit <- new_weighted_median_qra_fit(
    parameters = list(coefficients = coefficients, par = par)
  )

  return(qra_fit)
}


#' Initial parameter constructor for rel_wis_weighted_mean approach
#'
#' @param qfm_train QuantileForecastMatrix with training set predictions from
#'    component models
#' @param ... mop up other arguments that are ignored
#'
#' @return vector of real-valued initial values for parameters
init_par_constructor_rel_wis_weighted_mean <- function(qfm_train, ...) {
  return(0.0)
}


#' Parameter search grid constructor for rel_wis_weighted_mean approach
#'
#' @param qfm_train QuantileForecastMatrix with training set predictions from
#'    component models
#' @param ... mop up other arguments that are ignored
#'
#' @return vector of real-valued initial values for parameters
par_grid_constructor_rel_wis_weighted_mean <- function(qfm_train, ...) {
  return(seq(from = 0.0, to = 20.0, by = 0.1))
}


#' Model constructor for rel_wis_weighted_mean approach
#'
#' @param par vector of real numbers
#' @param qfm_train object of class QuantileForecastMatrix
#'
#' @return object of class qra_fit
#'
#' @export
model_constructor_rel_wis_weighted_mean <- function(par, qfm_train, y_train, rel_wis = calc_relative_wis(y_train, qfm_train)) {
  col_index <- attr(qfm_train, 'col_index')
  model_col <- attr(qfm_train, 'model_col')
  unique_models <- unique(col_index[[model_col]])

  rel_wis_vec <- data.frame(model = unique_models) %>%
    dplyr::left_join(rel_wis, by = 'model') %>%
    dplyr::pull(rel_wis)
  weights <- softmax_matrix_rows(
    matrix(-1 * par * rel_wis_vec, nrow = 1)
  )
  dim(weights) <- prod(dim(weights))

  coefficients <- data.frame(
    a = unique_models,
    beta = weights,
    stringsAsFactors = FALSE
  )
  colnames(coefficients)[1] <- model_col

  intercept <- data.frame(beta = 0.0, stringsAsFactors = FALSE)
  qra_fit <- new_rescaled_qra_fit(
    parameters = list(
      coefficients = coefficients,
      intercept = intercept,
      par = par),
    convex = TRUE
  )

  return(qra_fit)
}


#' Estimate qra model using quantgen package as backend
#'
#' @param qfm_train QuantileForecastMatrix with training set predictions
#' @param y_train numeric vector of responses for training set
#' @param qfm_test QuantileForecastMatrix with test set predictions
#' @param intercept logical specifying whether an intercept is included
#' @param constraint character specifying constraints on parameters; 'convex',
#' 'positive' or 'unconstrained'
#' @param quantile_groups Vector of group labels for quantiles, having the same
#' length as the number of quantiles.  Common labels indicate that the ensemble
#' weights for the corresponding quantile levels should be tied together.
#' Default is rep(1,length(quantiles)), which means that a common set of
#' ensemble weights should be used across all levels.  This is the argument
#' `tau_groups` for `quantgen::quantile_ensemble`
#' @param noncross string specifying approach to handling quantile noncrossing:
#' one of "constrain" or "sort". "constrain" means estimation is done subject
#' to constraints ruling out quantile crossing.  "sort" means no such
#' constraints are imposed during estimation, but the resulting forecasts are
#' sorted.
#'
#' @return object of class qra_fit
estimate_qra_quantgen <- function(
    qfm_train,
    y_train,
    qfm_test,
    intercept,
    constraint,
    quantile_groups,
    noncross = "constrain") {
  # unpack and process arguments
  col_index <- attr(qfm_train, 'col_index')
  model_col <- attr(qfm_train, 'model_col')
  quantile_name_col <- attr(qfm_train, 'quantile_name_col')

  models <- unique(col_index[[model_col]])
  num_models <- length(models)

  quantiles <- sort(unique(col_index[[quantile_name_col]]))
  num_quantiles <- length(quantiles)

  quantgen_intercept <- intercept

  if(constraint == 'convex') {
    quantgen_nonneg = TRUE
    quantgen_unit_sum = TRUE
  } else if(constraint == 'positive') {
    quantgen_nonneg = TRUE
    quantgen_unit_sum = FALSE
  } else if(constraint == 'unconstrained') {
    quantgen_nonneg = FALSE
    quantgen_unit_sum = FALSE
  }

  noncross <- match.arg(noncross, choices = c("constrain", "sort"))

  # reformat training set predictive quantiles from component models as 3d
  # array in format required for quantgen package
  qarr_train <- unclass(qfm_train)
  dim(qarr_train) <- c(nrow(qarr_train), num_quantiles, num_models)
  qarr_train <- aperm(qarr_train, c(1, 3, 2))

  if(missing(qfm_test) || is.null(qfm_test)) {
    q0 <- qarr_train
  } else {
    qarr_test <- unclass(qfm_test)
    dim(qarr_test) <- c(nrow(qarr_test), num_quantiles, num_models)
    qarr_test <- aperm(qarr_test, c(1, 3, 2))

    n_train <- dim(qarr_train)[1]
    n_test <- dim(qarr_test)[1]

    if (noncross == "constrain") {
      q0 <- array(dim = c(n_train + n_test, num_models, num_quantiles))
      q0[seq_len(n_train), , ] <- qarr_train
      q0[n_train + seq_len(n_test), , ] <- qarr_test
    } else {
      q0 <- NULL
    }
  }

  # estimate ensemble parameters
  quantgen_fit <- quantgen::quantile_ensemble(
    qarr = qarr_train,
    y = y_train,
    tau = as.numeric(quantiles),
    tau_groups = quantile_groups,
    intercept = quantgen_intercept,
    nonneg = quantgen_nonneg,
    unit_sum = quantgen_unit_sum,
    noncross = (noncross == "constrain"),
    q0 = q0,
    verbose = FALSE,
    lp_solver = "gurobi"
  )


  # unpack result from quantgen and store in our format
  if(length(unique(quantile_groups)) > 1) {
    if(quantgen_intercept) {
      intercept <- data.frame(
        q = quantiles,
        beta = quantgen_fit$alpha[1, ],
        stringsAsFactors = FALSE)
      colnames(intercept)[1] <- quantile_name_col
      coefficients <- col_index %>%
        mutate(
          beta = as.vector(t(quantgen_fit$alpha[-1, , drop = FALSE]))
        )
    } else {
      intercept <- data.frame(beta = 0.0, stringsAsFactors = FALSE)
      coefficients <- col_index %>%
        mutate(
          beta = as.vector(t(quantgen_fit$alpha))
        )
    }
  } else {
    if(quantgen_intercept) {
      intercept <- data.frame(beta = quantgen_fit$alpha[1], stringsAsFactors = FALSE)
      coefficients <- data.frame(model=models, beta = quantgen_fit$alpha[-1], stringsAsFactors = FALSE)
      names(coefficients)[1] <- model_col
    } else {
      intercept <- data.frame(beta = 0.0, stringsAsFactors = FALSE)
      coefficients <- data.frame(model=models, beta = quantgen_fit$alpha, stringsAsFactors = FALSE)
      names(coefficients)[1] <- model_col
    }
  }

  qra_fit <- new_qra_fit(
    parameters = list(
      coefficients = coefficients,
      intercept = intercept,
      quantgen_qarr_train = qarr_train,
      quantgen_y_train = y_train),
    convex = FALSE
  )

  return(qra_fit)
}




#' Estimate qra model using qenspy package as backend
#'
#' @param qfm_train QuantileForecastMatrix with training set predictions
#' @param y_train numeric vector of responses for training set
#' @param qfm_test QuantileForecastMatrix with test set predictions
#' @param intercept logical specifying whether an intercept is included
#' @param constraint character specifying constraints on parameters; 'convex',
#' 'positive' or 'unconstrained'
#' @param quantile_groups Vector of group labels for quantiles, having the same
#' length as the number of quantiles.  Common labels indicate that the ensemble
#' weights for the corresponding quantile levels should be tied together.
#' Default is rep(1,length(quantiles)), which means that a common set of
#' ensemble weights should be used across all levels.  This is the argument
#' `tau_groups` for `quantgen::quantile_ensemble`
#'
#' @return object of class qra_fit
estimate_qra_qenspy <- function(
    qfm_train,
    y_train,
    combine_method,
    quantile_groups,
    init_param_vec = NULL,
    optim_method = "adam",
    num_iter = 1000,
    learning_rate = 0.1,
    verbose = FALSE,
    partial_save_frequency,
    partial_save_filename
    ) {
  require(reticulate)
  
  # unpack and process arguments
  col_index <- attr(qfm_train, 'col_index')
  model_col <- attr(qfm_train, 'model_col')
  quantile_name_col <- attr(qfm_train, 'quantile_name_col')

  models <- unique(col_index[[model_col]])
  num_models <- length(models)

  quantiles <- as.numeric(sort(unique(col_index[[quantile_name_col]])))
  num_quantiles <- length(quantiles)

  quantile_groups <- as.integer(factor(quantile_groups)) - 1

  # reformat training set predictive quantiles from component models as 3d
  # array in format required for qenspy
  qarr_train <- unclass(qfm_train)
  dim(qarr_train) <- c(nrow(qarr_train), num_quantiles, num_models)

  # estimate ensemble parameters
  qens <- reticulate::import("qenspy.qens", delay_load = TRUE)
  if (combine_method == "convex_mean") {
    qens_model <- qens$MeanQEns()
  } else if (combine_method == "convex_median") {
    qens_model <- qens$MedianQEns()
  }

  qens_model$fit(
    y_train,
    qarr_train,
    quantiles,
    quantile_groups,
    init_param_vec =
      rep(0.0, (dim(qarr_train)[3] - 1) * length(unique(quantile_groups))),
    optim_method = optim_method,
    num_iter = as.integer(num_iter),
    learning_rate = learning_rate,
    verbose = verbose,
    save_frequency = partial_save_frequency,
    save_path = partial_save_filename)

  # Create R object representing the model fit
  qra_fit <- new_qenspy_qra_fit(
    param_vec = qens_model$get_param_estimates_vec(),
    M = dim(qarr_train)[3],
    model_names = models,
    quantile_levels = quantiles,
    quantile_groups = quantile_groups,
    combine_method = combine_method,
    loss_trace = qens_model$loss_trace
  )

  return(qra_fit)
}
