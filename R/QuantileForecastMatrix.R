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
#'
#' @return invisible(TRUE) if QuantileForecastMatrix is valid;
#'   otherwise, an error is thrown
validate_QuantileForecastMatrix <- function(qfm) {
  # correct class
  if(!is.QuantileForecastMatrix(qfm)) {
    stop("object is not a QuantileForecastMatrix object")
  }

  errors <- NULL
  message('validation not yet implemented')

  # validate quantile crossing per model and target
#  if(check) {
#    errors <- c(errors, "message")
#  }

  if(is.null(errors)) {
    return(invisible(TRUE))
  } else {
    stop(paste0('Errors in validate_QuantileForecastMatrix: ',
      paste0(errors, collapse = '; ')))
  }
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
#' @param id_cols character vector of columns in forecast_df identifying unique forecast
#'   settings such as location, time zero, and horizon
#' @param quantile_name_col character name of column in forecast_df containing probability
#'   for quantile, e.g. 0.95
#' @param quantile_value_col character name of column in forecast_df identifying value for
#'   quantile, e.g. 195
#'
#' @return Object of class QuantileForecastMatrix
#'
#' @export
new_QuantileForecastMatrix_from_df <- function(
  forecast_df,
  model_col,
  id_cols,
  quantile_name_col,
  quantile_value_col
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

  # all unique combinations of id column values,
  # regardless of whether they are represented in forecast_df
  id_grid <- expand.grid(
    purrr::map(id_cols, function(col) { unique(forecast_df[[col]]) }) %>%
      `names<-`(id_cols),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  # pivot the quantiles wider; each quantile is now in its own column
  forecast_df <- forecast_df %>%
    tidyr::pivot_wider(
      names_from = UQ(quantile_name_col),
      values_from = UQ(quantile_value_col))

  # assemble matrix of results; dfc combines by stacking horizontally in columns
  forecast_matrix <- purrr::map_dfc(
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

      # return
      return(result)
    }
  ) %>%
    as.matrix() %>%
    `dimnames<-`(NULL)

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
  purrr::map_dfr(seq_len(nrow(qfm)),
    function(i) {
      purrr::map_dfr(seq_len(ncol(qfm)),
        function(j) {
          if(is.na(qfm[i,j])) {
            return(NULL)
          }
          result <- bind_cols(
              attr(qfm, 'row_index')[i, ],
              attr(qfm, 'col_index')[j, ]
            )

          result[[attr(qfm, 'quantile_value_col')]] <-
            unclass(qfm[i, j])

          result[[attr(qfm, 'quantile_name_col')]] <-
            as.numeric(result[[attr(qfm, 'quantile_name_col')]])

          return(result)
        }
      )
    }
  )
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

