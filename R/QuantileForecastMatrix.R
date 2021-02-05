
#' Check if object is of class QuantileForecastMatrix
#'
#' @param qfm an object that may be a QuantileForecastMatrix
#'
#' @return boolean; whether object is inherits QuantileForecastMatrix class
#'
#' @export
is.QuantileForecastMatrix <- function(qfm) {
  if (inherits(qfm, "QuantileForecastMatrix")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Validate QuantileForecastMatrix object
#'
#' @param qfm object of class QuantileForecastMatrix
#' @param strict logical; if FALSE (default), only checks that the arguments
#'    satisfy minimum requirements of formatting; if TRUE, also checks
#'    desirable properties of a quantile forecast such as quantile crossing
#'
#' @return invisible(TRUE) if QuantileForecastMatrix is valid;
#'   otherwise, an error is thrown
validate_QuantileForecastMatrix <- function(qfm, strict=FALSE) {
  # correct class
  if(!is.QuantileForecastMatrix(qfm)) {
    stop("object is not a QuantileForecastMatrix object")
  }

  errors <- NULL
  message('validation not yet implemented')

#  if(strict) {
#    # validate quantile crossing per model and target
#    errors <- c(errors, "message")
#  }

  if(is.null(errors)) {
    return(invisible(TRUE))
  } else {
    stop(paste0('Errors in validate_QuantileForecastMatrix: ',
      paste0(errors, collapse = '; ')))
  }
}


#' Validate quantile crossing
#'
#' @param qfm object of class QuantileForecastMatrix
#'
#' @return invisible(TRUE) if no quantile crossing; otherwise, error
#'
#' @export
validate_quantile_crossing <- function(qfm) {
  col_index <- attr(qfm, 'col_index')
  model_col <- attr(qfm, 'model_col')
  for(model in unique(col_index[[model_col]])) {
    model_inds <- which(col_index[[model_col]] == model)
    diffs <- qfm[, model_inds[-1]] - qfm[, model_inds[-length(model_inds)]]
    if(!(all(diffs >= 0.0))) {
      stop(paste0('Quantile crossing detected for model ', model))
    }
  }
  return(invisible(TRUE))
}


#' Create a QuantileForecastMatrix object
#'
#' @param qfm a matrix of forecasts.  Distinct combinations of unit,
#' @param row_index data frame giving unique combination of identifiers such as
#'    location, forecast date, and target for each row of \code{qfm}
#' @param col_index data frame giving the combination of model and quantile
#'    corresponding to each column of \code{qfm}
#' @param model_col name of column identifying model in long format
#'    representation of forecasts; also appears in \code{col_index}
#' @param quantile_name_col name of column for quantile probability in long
#'    format data frame representation of forecasts
#' @param quantile_value_col name of column for quantile value in long format
#'    data frame representation of forecasts
#'
#' @return QuantileForecastMatrix object
#'
#' @export
new_QuantileForecastMatrix <- function(
  qfm,
  row_index,
  col_index,
  model_col,
  quantile_name_col,
  quantile_value_col) {
  qfm <- structure(
    qfm,
    row_index=row_index,
    col_index=col_index,
    model_col=model_col,
    quantile_name_col=quantile_name_col,
    quantile_value_col=quantile_value_col,
    class = c('QuantileForecastMatrix', 'matrix')
  )

#  validate_QuantileForecastMatrix(qfm)

  return(qfm)
}


#' Translate from a tidy data frame of forecasts to a wide matrix representation
#' suitable for use by \code{calc_qra_from_matrices}
#'
#' @param forecast_df data frame of forecasts from component models in 'CDC format'
#' @param model_col character name of column in forecast_df identifying model
#' @param id_cols character vector of columns in forecast_df identifying unique
#'    forecast settings such as location, time zero, and horizon
#' @param quantile_name_col character name of column in forecast_df containing
#'    probability for quantile, e.g. 0.95
#' @param quantile_value_col character name of column in forecast_df
#'    identifying value for quantile, e.g. 195
#' @param drop_missing_id_levels logical.  If FALSE, all combinations of unique
#'    values found in columns specified by id_cols are included, with NA values
#'    where corresponding forecasts are not provided.  If TRUE, only the
#'    combinations of values for id_cols in the data frame are retained.
#'
#' @return Object of class QuantileForecastMatrix
#'
#' @export
new_QuantileForecastMatrix_from_df <- function(
  forecast_df,
  model_col,
  id_cols,
  quantile_name_col,
  quantile_value_col,
  drop_missing_id_levels=FALSE
) {
  if(!all(c(model_col, id_cols, quantile_name_col, quantile_value_col) %in%
          names(forecast_df))) {
    stop('provided column names are not columns of forecast_df')
  }

  # all unique combinations of model and quantile probability,
  # regardless of whether they are represented in forecast_df
  models <- unique(forecast_df[[model_col]])
  quantile_probs <- as.character(unique(forecast_df[[quantile_name_col]]))
  col_grid <- expand.grid(
    quantile_prob = quantile_probs,
    model = models,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) %>%
    `colnames<-`(c(quantile_name_col, model_col))

  if(drop_missing_id_levels) {
    id_grid <- dplyr::distinct(forecast_df[, id_cols, drop=FALSE])
  } else {
    # all unique combinations of id column values,
    # regardless of whether they are represented in forecast_df
    id_grid <- expand.grid(
      purrr::map(id_cols, function(col) { unique(forecast_df[[col]]) }) %>%
        `names<-`(id_cols),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
  }

  # pivot the quantiles wider; each quantile is now in its own column
  forecast_df <- forecast_df %>%
    tidyr::pivot_wider(
      names_from = UQ(quantile_name_col),
      values_from = UQ(quantile_value_col))

  # assemble matrix of results; map models to list of matrices and then reduce with cbind to single matrix
  forecast_matrix <- purrr::map(
    models,
    function(model) {
      # augment id grid with model
      full_grid <- id_grid
      full_grid[[model_col]] <- model

      # augment grid with forecasted quantiles, then keep only quantiles
      result <- full_grid %>%
        dplyr::left_join(forecast_df, by = c(model_col, id_cols)) %>%
        dplyr::select(UQ(quantile_probs))

      # throw error on duplicated forecasts
      # e.g. multiple submissions from same model in 1 week,
      # or insufficient id_cols to uniquely identify forecast setting
      if(nrow(result) != nrow(id_grid)) {
        stop(paste0('Error: forecast_df may contain multiple forecasts for the same combination of id_cols from model ', model))
      }
      
      # convert to matrix 
      result <- as.matrix(result)
      # remove column names (so as to avoid conflict in cbind)
      dimnames(result) <- NULL

      # return
      return(result)
    }
  ) %>% purrr::reduce(cbind)

  # create result as QuantileForecastMatrix object and return
  qfm <- new_QuantileForecastMatrix(
    qfm=forecast_matrix,
    row_index=id_grid,
    col_index=col_grid,
    model_col=model_col,
    quantile_name_col=quantile_name_col,
    quantile_value_col=quantile_value_col)

  return(qfm)
}


#' Translate from a QuantileForecastMatrix representation of forecasts to a
#' tidy data frame
#'
#' @param qfm wide matrix representation of forecasts
#'
#' @return tidy data frame of forecasts
#'
#' @export
as.data.frame.QuantileForecastMatrix <- function(qfm) {
  dplyr::full_join(attr(qfm, "col_index"), attr(qfm, "row_index"), by = character()) %>% 
  dplyr::mutate(
    !!attr(qfm, "quantile_value_col") := qfm %>% `attributes<-`(NULL),
    !!attr(qfm, "quantile_name_col") := as.numeric(!!sym(attr(qfm, "quantile_name_col")))
  ) %>% 
  dplyr::relocate(
    attr(qfm, "model_col"),
    attr(qfm, "quantile_name_col"),
    attr(qfm, "quantile_value_col")
  ) %>% 
  dplyr::filter(!is.na(!!sym(attr(qfm, "quantile_value_col"))))
}

#' Convenience `str` method for QuantileForecastMatrix class which
#' avoids `seq_len(ncol(qfm))` errors and warnings
#' 
#' @param qfm wide matrix representation of forecasts
#' 
#' @return none
#' 
#' @export
str.QuantileForecastMatrix <- function(qfm) {
  str(unclass(qfm))
}


#' Extract parts of a QuantileForecastMatrix object
#'
#' @param qfm an object of class QuantileForecastMatrix
#' @param i integer indices of rows to extract
#' @param j integer indices of columns to extract
#' @param ... other ignored arguments; used to mop up a required drop argument,
#'    which is not supported.
#'
#' @return object of class QuantileForecastMatrix
#'
#' @export
`[.QuantileForecastMatrix` <- function(
  qfm,
  i = seq_len(nrow(qfm)),
  j = seq_len(ncol(qfm)),
  ...) {
  new_qfm <- new_QuantileForecastMatrix(
    NextMethod('[', drop=FALSE),
    row_index=attr(qfm, 'row_index')[i, , drop=FALSE],
    col_index=attr(qfm, 'col_index')[j, , drop=FALSE],
    model_col=attr(qfm, 'model_col'),
    quantile_name_col=attr(qfm, 'quantile_name_col'),
    quantile_value_col=attr(qfm, 'quantile_value_col'))

  return(new_qfm)
}

