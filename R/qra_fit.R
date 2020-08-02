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


#' Predict based on a quantile regression averaging fit
#'
#' @param qra_fit_rescalable object of class qra_fit_rescalable
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#'
#' @return object of class QuantileForecastMatrix with quantile forecasts
#'
#' @export
predict.rescaled_qra_fit <- function(qra_fit, qfm) {
  # construct sparse matrix representing model weights across quantiles
  ## pull out parameter values
  col_index <- attr(qfm, 'col_index')
  model_id_name <- attr(qfm, 'model_col')
  eligbility_by_row <- purrr::map_dfr(
    seq_len(nrow(qfm)),
    function(row_ind) {
      purrr::map_dfr(
        unique(col_index[[model_id_name]]),
        function(model_id) {
          col_inds <- which(col_index[[model_id_name]] == model_id)

          result <- attr(qfm, 'row_index')[row_ind, ]
          result[[model_id_name]] <- model_id
          result[['eligible']] <- !any(is.na(qfm[row_ind, col_inds]))

          return(result)
        }
      )
    }
  )

  # convert model eligibility to wide format logical with human readable names
  wide_model_eligibility <- eligbility_by_row %>%
    pivot_wider(names_from='model', values_from='eligible')

  # group locations by which models are included per row
  row_groups <- wide_model_eligibility %>%
    dplyr::mutate(row_num = dplyr::row_number()) %>%
    group_by_if(is.logical) %>%
    summarize(
      rows = list(row_num)#,
#      ids = list(.[, c('location', 'forecast_week_end_date', 'target')])
    ) %>%
    ungroup()

  # predict separately for each row group
  all_preds <- purrr::map(
    seq_len(nrow(row_groups)),
    function(i) {
      temp <- row_groups %>% slice(i) %>% select(-rows)#, -ids)
      models <- colnames(temp)[which(as.matrix(temp))]
      col_inds <- which(col_index[[model_id_name]] %in% models)

      reduced_qfm <- qfm[row_groups$rows[[i]], col_inds]

      reduced_coefficients <- qra_fit$coefficients[
        qra_fit$coefficients[[1]] %in% models, , drop = FALSE
      ]

      reduced_qra_fit <- new_qra_fit(
        parameters = list(coefficients=reduced_coefficients,
                          intercept=qra_fit$intercept),
        convex = TRUE
      )

      return(predict(reduced_qra_fit, reduced_qfm))
    }
  )

  # assemble into full matrix of results, arrange by original row
  combined_row_index <- purrr::map_dfr(
    all_preds,
    function(pred_qfm) {
      attr(pred_qfm, 'row_index')
    })

  col_index <- attr(all_preds[[1]], 'col_index')

  combined_fm <- do.call(
    rbind,
    purrr::map(all_preds, unclass)
  )

  # sorting
  row_idx <- sort(unlist(row_groups$rows), index.return = TRUE)$ix
  combined_row_index <- combined_row_index[row_idx, , drop = FALSE]
  combined_fm <- combined_fm[row_idx, , drop = FALSE]

  result_qfm <- new_QuantileForecastMatrix(
    qfm = combined_fm,
    row_index=combined_row_index,
    col_index=col_index,
    model_col=attr(all_preds[[1]], 'model_col'),
    quantile_name_col=attr(all_preds[[1]], 'quantile_name_col'),
    quantile_value_col=attr(all_preds[[1]], 'quantile_value_col')
  )

  return(result_qfm)
}



#' Predict based on a quantile regression averaging fit
#'
#' @param qra_fit object of class median_qra_fit
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#'
#' @return object of class QuantileForecastMatrix with quantile forecasts
#'
#' @export
predict.median_qra_fit <- function(qra_fit, qfm) {
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
#' @param constraint character specifying constraints on parameters; 'ew',
#' 'convex', 'positive' or 'unconstrained'
#' @param quantile_groups Vector of group labels for quantiles, having the same
#' length as the number of quantiles.  Common labels indicate that the ensemble
#' weights for the corresponding quantile levels should be tied together.
#' Default is rep(1,length(quantiles)), which means that a common set of
#' ensemble weights should be used across all levels.  This is the argument
#' `tau_groups` for `quantmod::quantile_ensemble`, and may only be supplied if
#' `backend = 'quantmod`
#' @param backend implementation used for estimation; currently either
#'    'optim', using L-BFGS-B as provided by the optim function in R;
#'    'NlcOptim', using NlcOptim::solnl; or 'quantmod', using
#'    quantmod::quantile_ensemble
#'
#' @return object of class qra_fit
#'
#' @export
estimate_qra <- function(
  qfm_train,
  y_train,
  qfm_test = NULL,
  intercept = FALSE,
  constraint = c('ew', 'convex', 'positive', 'unconstrained'),
  quantile_groups = NULL,
  backend = 'optim',
  ...
) {
  constraint <- match.arg(constraint, choices = c('ew', 'convex', 'positive', 'unconstrained'))
  backend <- match.arg(backend, choices = c('optim', 'NlcOptim', 'qra', 'quantmod'))

  if(backend == 'quantmod') {
    result <- estimate_qra_quantmod(
      qfm_train=qfm_train,
      y_train=y_train,
      qfm_test=qfm_test,
      intercept=intercept,
      constraint=constraint,
      quantile_groups=quantile_groups)
    col_index <- attr(qfm_train, 'col_index')
  } else if(backend == 'qra') {
    stop("backend = 'qra' is not yet supported")
    qra_data <-
      result <- qra:::qra_estimate_weights(
        x = qra_data,
        per_quantile_weights = per_quantile_weights,
        enforce_normalisation = (constraint == 'convex')
      )
  } else {
    if(constraint == 'ew') {
      result <- estimate_qra_ew(qfm_train, ...)
    } else {
      stop('backend not supported')
      result <- estimate_qra_optimized(qfm_train, y_train, constraint, backend)
    }
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
#' @param qra_model quantile averaging model
#' @param backend implementation used for estimation; currently either
#'    'optim', using L-BFGS-B as provided by the optim function in R, or
#'    'NlcOptim', using NlcOptim::solnl
#'
#' @return object of class qra_fit
#'
#' @export
estimate_qra_optimized <- function(
  qfm_train,
  y_train,
  qra_model = c('convex_per_model', 'unconstrained_per_model', 'rescaled_convex_per_model'),
  backend = c('optim', 'NlcOptim')
) {
  qra_model <- match.arg(qra_model, choices = c('convex_per_model', 'unconstrained_per_model', 'rescaled_convex_per_model'))
  backend <- match.arg(backend, choices = c('optim', 'NlcOptim'))

  init_par_constructor <- getFromNamespace(
    paste0('init_par_constructor_', qra_model),
    ns='covidEnsembles'
  )
  model_constructor <- getFromNamespace(
    paste0('model_constructor_', qra_model),
    ns='covidEnsembles'
  )

  init_par <- init_par_constructor(qfm_train, y_train)
  if(backend == 'optim') {
    optim_output <- optim(
      par=init_par,
      fn=wis_loss,
      model_constructor=model_constructor,
      qfm_train=qfm_train,
      y_train=y_train,
      method='L-BFGS-B')

    if(optim_output$convergence != 0) {
      warning('optim convergence non-zero; estimation may have failed!')
    }
  } else if(backend == 'NlcOptim') {
    wis_loss_wrapper <- function(x) {
      wis_loss(
        x,
        model_constructor=model_constructor,
        qfm_train=qfm_train,
        y_train=y_train)
    }
    optim_output <- NlcOptim::solnl(
      X=init_par,
      objfun=wis_loss_wrapper
    )
  }

  return(model_constructor(optim_output$par, qfm_train))
}


#' Calculate wis_loss as a function of parameters used to construct a qra model
#'
#' @param par real-valued vector of parameters
#' @param model_constructor a function that accepts a real-valued vector of
#'    parameters and returns a model of class qra_fit
#' @param qfm_train QuantileForecastMatrix with training set predictions from
#'    component models
#' @param y_train numeric vector of responses for training set
#'
#' @return scalar wis loss for given parameter values
wis_loss <- function(par, model_constructor, qfm_train, y_train) {
  qra_model <- model_constructor(par, qfm_train)
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


#' Estimate qra model using quantmod package as backend
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
#' `tau_groups` for `quantmod::quantile_ensemble`
#'
#' @return object of class qra_fit
estimate_qra_quantmod <- function(qfm_train, y_train, qfm_test, intercept, constraint, quantile_groups) {
  # unpack and process arguments
  col_index <- attr(qfm_train, 'col_index')
  model_col <- attr(qfm_train, 'model_col')
  quantile_name_col <- attr(qfm_train, 'quantile_name_col')

  models <- unique(col_index[[model_col]])
  num_models <- length(models)

  quantiles <- sort(unique(col_index[[quantile_name_col]]))
  num_quantiles <- length(quantiles)

  quantmod_intercept <- intercept

  if(constraint == 'convex') {
    quantmod_nonneg = TRUE
    quantmod_unit_sum = TRUE
  } else if(constraint == 'positive') {
    quantmod_nonneg = TRUE
    quantmod_unit_sum = FALSE
  } else if(constraint == 'unconstrained') {
    quantmod_nonneg = FALSE
    quantmod_unit_sum = FALSE
  }

  # reformat training set predictive quantiles from component models as 3d
  # array in format required for quantmod package
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

    q0 <- array(dim = c(n_train + n_test, num_models, num_quantiles))
    q0[seq_len(n_train), , ] <- qarr_train
    q0[n_train + seq_len(n_test), , ] <- qarr_test
  }

  # estimate ensemble parameters
  quantmod_fit <- quantmod::quantile_ensemble(
    qarr=qarr_train,
    y=y_train,
    tau=as.numeric(quantiles),
    tau_groups=quantile_groups,
    intercept = quantmod_intercept,
    nonneg = quantmod_nonneg,
    unit_sum = quantmod_unit_sum,
    noncross = TRUE,
    q0 = q0,
    verbose=FALSE
  )


  # unpack result from quantmod and store in our format
  if(length(unique(quantile_groups)) > 1) {
    if(quantmod_intercept) {
      intercept <- data.frame(
        q = quantiles,
        beta = quantmod_fit$alpha[1, ],
        stringsAsFactors = FALSE)
      colnames(intercept)[1] <- quantile_name_col
      coefficients <- col_index %>%
        mutate(
          beta = as.vector(t(quantmod_fit$alpha[-1, , drop = FALSE]))
        )
    } else {
      intercept <- data.frame(beta = 0.0, stringsAsFactors = FALSE)
      coefficients <- col_index %>%
        mutate(
          beta = as.vector(t(quantmod_fit$alpha))
        )
    }
  } else {
    if(quantmod_intercept) {
      intercept <- data.frame(beta = quantmod_fit$alpha[1], stringsAsFactors = FALSE)
      coefficients <- data.frame(model=models, beta = quantmod_fit$alpha[-1], stringsAsFactors = FALSE)
      names(coefficients)[1] <- model_col
    } else {
      intercept <- data.frame(beta = 0.0, stringsAsFactors = FALSE)
      coefficients <- data.frame(model=models, beta = quantmod_fit$alpha, stringsAsFactors = FALSE)
      names(coefficients)[1] <- model_col
    }
  }

  qra_fit <- new_qra_fit(
    parameters = list(coefficients=coefficients, intercept=intercept),
    convex = FALSE
  )

  return(qra_fit)
}
