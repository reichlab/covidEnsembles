#' Check if object is of class qra_ew
#'
#' @param object an object that may be a qra_ew object
#'
#' @return boolean; whether object is inherits qra_ew class
#'
#' @export
is.qra_fit <- function(object) {
  if (inherits(object, "qra_fit")) {
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


#' Create a qra_fit object
#'
#' @param parameters named list with two components:
#'   * 'intercept': data frame optionally with a column for quantile values,
#'     and corresponding intercepts
#'   * 'coefficients': data frame of models, optionally quantile values,
#'     and corresponding model coefficients
#' @param constrained logical; if TRUE, predictions are done by passing weights
#'   through a softmax transformation; otherwise, no constraint is imposed.
#'
#' @return qra_fit object
#'
#' @export
new_qra_fit <- function(
  parameters,
  constrained
) {
  qra_fit <- structure(
    parameters,
    constrained=constrained,
    class = 'qra_fit'
  )

#  validate_qra_fit(qra_fit)

  return(qra_fit)
}


#' Predict based on a quantile regression averaging fit
#'
#' @param qra_fit object of class qra_fit
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#'
#' @return object of class QuantileForecastMatrix with quantile forecasts
#'
#' @export
predict.qra_fit <- function(qra_fit, qfm) {
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
  if(attr(qra_fit, 'constrained')) {
    ### Convert to matrix where each column is one model, each row one quantile
    dim(params) <- c(length(unique_models), length(unique_quantiles))

    ### Apply softmax to each row; model weights sum to 1 within each quantile
    softmax_matrix_rows(params)

    ### convert back to vector
    dim(params) <- prod(dim(params))
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
#' @param method estimation method: currently only 'ew' is supported
#'
#' @return object of class qra_fit
#'
#' @export
estimate_qra <- function(
  qfm_train,
  y_train,
  method = c('ew'),
  ...
) {
  method <- match.arg(method, choices = c('ew'))

  if(method == 'ew') {
    result <- fit_qra_ew(qfm_train, ...)
  } else {
    stop('Invalid method')
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
fit_qra_ew <- function(qfm_train, ...) {
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
    constrained = FALSE
  )

  return(qra_fit)
}
