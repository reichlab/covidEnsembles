#' Generate a data frame with a row for each model and an indicator of whether
#' that model is eligible for inclusion in an ensemble, with explanation if not
#'
#' @param forecast_matrix wide format matrix of forecasts
#' @param observed data frame of observed values
#' @param lookback_length non-negative integer number of historic weeks that
#'   are examined for forecast missingness; 0 is appropriate for equal weight
#'   ensembles where no historical data is required.  If two past weeks of
#'   forecast data are required to estimate ensemble parameters, lookback_length
#'   should be 2
#' @param model_id_name character name of column in forecast matrix col_index
#'   specifying column
#'
#' @return data frame with two columns:
#'   * one with name given by model_id_name recording model id for each model
#'   * a second called 'eligibility' that is either the string 'eligible' or a
#'     brief description of why the model can't be included
#'
#' @export
calc_model_eligibility_for_ensemble <- function(
  qfm,
  observed,
  lookback_length,
  model_id_name
) {
  # identify missing forecasts by unit, forecast week end date, and model
  missingness <- calc_forecast_missingness(qfm, lookback_length, model_id_name)

  # check whether 10th quantile is less than most recent observation
  q10_check <- calc_q10_check(qfm, observed, model_id_name)

  # combine missingness and q10 eligibility check results
  # missingness takes precedent in that if both checks are violated,
  # only the failure for missingness is reported
  eligibility <- missingness %>%
    dplyr::left_join(q10_check) %>%
    transmute(
      unit=unit,
      model_id_name=UQ(model_id_name),
      eligibility = ifelse(
        missingness_eligibility == 'eligible',
        q10_eligibility,
        missingness_eligibility
      )
    )
  names(eligibility)[names(eligibility) == 'model_id_name'] <-
    model_id_name

  return(eligibility)
}


#' Compute forecast missingness for each combination of unit and model
#'
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#' @param observed data frame of observed values
#' @param model_id_name character name of column in forecast matrix col_index
#'   specifying column
#'
#' @return data frame with a row for each combination of
#'   unit, forecast week end date, and model and a logical column called
#'   'any_missing' with entry TRUE if any forecasts were missing across all
#'   quantiles and targets
#'
#' @export
calc_q10_check <- function(
  qfm,
  observed,
  model_id_name
) {
  ## subset to forecasts for most recent week
  row_index <- attr(qfm, 'row_index')
  rows_to_keep <- which(
    row_index$forecast_week_end_date %in% last_lookback_forecast_weeks)
  qfm <- qfm[rows_to_keep, ]

  ## keep forecasts only for quantile 0.1
  col_index <- attr(qfm, 'col_index')
  quantile_name_col <- attr(qfm, 'quantile_name_col')
  cols_to_keep <- which(abs(col_index[[quantile_name_col]] - 0.1) < 1e-8)
  qfm_q10 <- qfm[, cols_to_keep]

  ## extract data frame with indicator of which models have quantile 0.1 less
  ## than most recent observed value
  row_index <- attr(qfm, 'row_index')
  truths <- row_index %>%
    left_join(truths) %>%
    pull(value)

  q10_less_than_obs <- sweep(qfm_q10, MARGIN = 1, FUN = `<`, truths) %>%
    as.data.frame()
  names(eligibility)[names(eligibility) == 'get(model_id_name)'] <-
    model_id_name
}


#' Compute forecast missingness for each combination of unit and model
#'
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#' @param lookback_length non-negative integer number of historic weeks that
#'   are examined for forecast missingness; 0 is appropriate for equal weight
#'   ensembles where no historical data is required.  If two past weeks of
#'   forecast data are required to estimate ensemble parameters, lookback_length
#'   should be 2
#' @param model_id_name character name of column in forecast matrix col_index
#'   specifying column
#'
#' @return data frame with a row for each combination of
#'   unit, forecast week end date, and model and a logical column called
#'   'any_missing' with entry TRUE if any forecasts were missing across all
#'   quantiles and targets
#'
#' @export
calc_forecast_missingness <- function(
  qfm,
  lookback_length = 0,
  model_id_name
) {
  # subset to rows representing forecasts within lookback_length
  row_index <- attr(qfm, 'row_index')
  if(lookback_length+1 > length(unique(row_index$forecast_week_end_date))) {
    stop('not enough forecast weeks in qfm to support requested lookback length')
  }

  last_lookback_forecast_weeks <- row_index %>%
    distinct(forecast_week_end_date) %>%
    top_n(lookback_length+1) %>%
    pull(forecast_week_end_date)
  rows_to_keep <- which(
    row_index$forecast_week_end_date %in% last_lookback_forecast_weeks)
  qfm <- qfm[rows_to_keep, ]

  # calculate missingness for each combination of unit, forecast week end date,
  # and model
  row_index <- attr(qfm, 'row_index')
  col_index <- attr(qfm, 'col_index')

  missingness_by_unit_forecast_week <- purrr::pmap_dfr(
    row_index %>% distinct(unit, forecast_week_end_date),
    function(unit, forecast_week_end_date) {
      row_inds <- which(row_index$unit == unit &
        row_index$forecast_week_end_date == forecast_week_end_date)

      purrr::map_dfr(
        unique(col_index[[model_id_name]]),
        function(model_id) {
          col_inds <- which(col_index[[model_id_name]] == model_id)

          result <- attr(qfm, 'row_index')[row_inds[1], ]
          result[[model_id_name]] <- model_id
          result[['any_missing']] <- any(is.na(qfm[row_inds, col_inds]))

          return(result)
        }
      )
    }
  )

  # calculate missingness for each combination of unit and model
  missingness_by_unit <- missingness_by_unit_forecast_week %>%
    dplyr::group_by(unit, get(model_id_name)) %>%
    dplyr::summarize(
      missingness_eligibility = ifelse(
        any(any_missing),
        'missing required forecasts',
        'eligible'
      )
    )
  names(missingness_by_unit)[names(missingness_by_unit) == 'get(model_id_name)'] <-
    model_id_name

  return(missingness_by_unit)
}
