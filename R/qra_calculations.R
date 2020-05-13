#' Calculate quantile regression average of forecast quantiles from component
#' models.
#'
#' @param forecast_df data frame of forecasts from component models. Each row
#'   corresponds to a single forecast.  At minimum, must contain:
#'    * a column with a model identifier
#'    * a separate column for each quantile to be aggregated
#'   Optionally, also contains columns uniquely identifying the forecast such as
#'   a location, time, and horizon.
#' @param weights_df data frame of weights assigned to each model, including an
#'   intercept term
calc_qra <- function(
  forecast_df,
  weight_df,
  model_col,
  id_cols,
  quantile_cols) {
  stop('not yet implemented')
}


#' An internal low level function to calculate quantile regression averaged
#' forecasts from suitably defined matrices of forecasts and weights.
#'
#'
calc_qra_from_forecast_matrix <- function(
  forecast_matrix,
  intercepts,
  weights
) {
  if(is.list(forecast_matrix) && 'forecast_matrix' %in% names(forecast_matrix)) {
    forecast_matrix <- forecast_matrix$forecast_matrix
  }
  if(!is.matrix(forecast_matrix)) {
    stop('forecast_matrix must be a matrix')
  }

  stop('not yet implemented')
}
