#' Generate a data frame with a row for each model and an indicator of whether
#' that model is eligible for inclusion in an ensemble, with explanation if not
#'
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#' @param observed_by_location_target_end_date data frame of observed values
#' @param do_q10_check logical; if TRUE, check condition that quantile at
#'   level 0.1 is at least as large as the most recent observed value
#' @param do_nondecreasing_quantile_check logical; if TRUE, check condition
#'   that quantiles for consecutive targets (1 wk ahead, 2 wk ahead, etc) are
#'  non-decreasing for each combination of location, forecast_week_end_date,
#'  model, and quantile probability level
#' @param window_size non-negative integer number of historic weeks that
#'   are examined for forecast missingness; 0 is appropriate for equal weight
#'   ensembles where no historical data is required.  If two past weeks of
#'   forecast data are required to estimate ensemble parameters, window_size
#'   should be 2
#' @param decrease_tol numeric; decreases of up to specified tolerance are
#'   allowed
#'
#' @return data frame with two columns:
#'   * one with name given by model_id_name recording model id for each model
#'   * a second called 'eligibility' that is either the string 'eligible' or a
#'     brief description of why the model can't be included
#'
#' @export
calc_model_eligibility_for_ensemble <- function(
  qfm,
  observed_by_location_target_end_date,
  do_q10_check = TRUE,
  do_nondecreasing_quantile_check = TRUE,
  window_size = 0,
  decrease_tol = 1.0
) {
  # subset to rows representing forecasts within window_size
  row_index <- attr(qfm, 'row_index')
  if(window_size+1 > length(unique(row_index$forecast_week_end_date))) {
    stop('not enough forecast weeks in qfm to support requested lookback length')
  }

  last_lookback_forecast_weeks <- row_index %>%
    distinct(forecast_week_end_date) %>%
    top_n(window_size+1) %>%
    pull(forecast_week_end_date)
  rows_to_keep <- which(
    row_index$forecast_week_end_date %in% last_lookback_forecast_weeks)
  qfm <- qfm[rows_to_keep, ]

  # identify missing forecasts by location, forecast week end date, and model
  model_id_name <- attr(qfm, 'model_col')
  eligibility <- calc_forecast_missingness(qfm)

  # check whether 10th quantile is less than most recent observation
  if(do_q10_check) {
    q10_check <- calc_q10_check(qfm, observed_by_location_target_end_date)

    # combine eligibility check results
    eligibility <- eligibility %>%
      dplyr::left_join(q10_check, by = c("location", model_id_name))
  } else {
    eligibility$q10_eligibility <- 'NA'
  }

  # check for nondecreasing quantiles over forecast horizon
  if(do_nondecreasing_quantile_check) {
    nondecreasing_quantile_check <- calc_nondecreasing_quantile_check(
      qfm,
      decrease_tol
    )

    # combine eligibility check results
    eligibility <- eligibility %>%
      dplyr::left_join(nondecreasing_quantile_check,
        by = c("location", model_id_name))
  } else {
    eligibility$nondecreasing_quantiles_eligibility <- 'NA'
  }

  eligibility$overall_eligibility <- ifelse(
    eligibility$missingness_eligibility == 'eligible' &
      eligibility$q10_eligibility %in% c('eligible', 'NA') &
      eligibility$nondecreasing_quantiles_eligibility %in% c('eligible', 'NA'),
    'eligible',
    'ineligible'
  )

  return(eligibility)
}


#' Compute check of whether quantile 0.1 for 1 week ahead forecast is at least
#' the median of the most recent observed cumulative deaths
#'
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#' @param observed_by_location_target_end_date data frame of observed values
#'
#' @return data frame with a row for each combination of
#'   location, forecast week end date, and model and a logical column called
#'   'any_missing' with entry TRUE if any forecasts were missing across all
#'   quantiles and targets
#'
#' @export
calc_q10_check <- function(
  qfm,
  observed_by_location_target_end_date
) {
  ## subset to forecasts for most recent week and one week ahead target,
  ## and forecasts for quantile 0.1
  row_index <- attr(qfm, 'row_index')
  rows_to_keep <- which(
    row_index$target == '1 wk ahead cum death'
  )

  col_index <- attr(qfm, 'col_index')
  quantile_name_col <- attr(qfm, 'quantile_name_col')
  cols_to_keep <- which(col_index[[quantile_name_col]] == "0.1")

  qfm_q10 <- qfm[rows_to_keep, cols_to_keep]

  ## extract data frame with indicator of which models have quantile 0.1 less
  ## than most recent observed value
  row_index <- attr(qfm_q10, 'row_index')
  col_index <- attr(qfm_q10, 'col_index')
  model_id_name <- attr(qfm, 'model_col')
  observed <- row_index %>%
    dplyr::left_join(observed_by_location_target_end_date,
      by = c('location'='location', 'forecast_week_end_date'='target_end_date')) %>%
    dplyr::pull(observed)

  q10_less_than_obs <- sweep(qfm_q10, MARGIN = 1, FUN = `<`, observed) %>%
    as.data.frame()

  eligibility <- purrr::pmap_dfr(
    row_index %>% distinct(location, forecast_week_end_date),
    function(location, forecast_week_end_date) {
      row_inds <- which(row_index$location == location &
                          row_index$forecast_week_end_date == forecast_week_end_date)

      purrr::map_dfr(
        unique(col_index[[model_id_name]]),
        function(model_id) {
          col_inds <- which(col_index[[model_id_name]] == model_id)

          result <- row_index[row_inds[1], ]
          result[[model_id_name]] <- model_id
          if(is.na(q10_less_than_obs[row_inds, col_inds])) {
            result[['q10_eligibility']] <- 'quantile 0.1 of forecast for horizon 1 is missing'
          } else if(q10_less_than_obs[row_inds, col_inds]) {
            result[['q10_eligibility']] <- 'quantile 0.1 of forecast for horizon 1 is less than most recent observed'
          } else {
            result[['q10_eligibility']] <- 'eligible'
          }

          return(result)
        }
      )
    }
  )

  # calculate q10 eligiblity for each combination of location and model
  eligibility <- eligibility %>%
    dplyr::group_by(location, get(model_id_name)) %>%
    dplyr::summarize(
      q10_eligibility = ifelse(
        any(q10_eligibility == 'quantile 0.1 of forecast for horizon 1 is less than most recent observed'),
        'quantile 0.1 of forecast for horizon 1 is less than most recent observed',
        ifelse(
          any(q10_eligibility == 'quantile 0.1 of forecast for horizon 1 is missing'),
          'quantile 0.1 of forecast for horizon 1 is missing',
          'eligible'
        )
      )
    ) %>%
    dplyr::ungroup()
  names(eligibility)[names(eligibility) == 'get(model_id_name)'] <-
    model_id_name

  return(eligibility)
}


#' Compute forecast missingness for each combination of location and model
#'
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#' @param by_week logical; if TRUE, results are returned by forecast week, and
#'   if FALSE results are summarized across weeks.
#'
#' @return data frame with a row for each combination of
#'   location, forecast week end date (if requestrd), and model and a logical column called
#'   'any_missing' with entry TRUE if any forecasts were missing across all
#'   quantiles and targets
#'
#' @export
calc_forecast_missingness <- function(
  qfm,
  by_week = FALSE
) {
  # calculate missingness for each combination of location, forecast week end date,
  # and model
  row_index <- attr(qfm, 'row_index')
  col_index <- attr(qfm, 'col_index')
  model_id_name <- attr(qfm, 'model_col')

  missingness_by_location_forecast_week <- purrr::pmap_dfr(
    row_index %>% distinct(location, forecast_week_end_date),
    function(location, forecast_week_end_date) {
      row_inds <- which(row_index$location == location &
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

  if(by_week) {
    return(missingness_by_location_forecast_week)
  }

  # calculate missingness for each combination of location and model
  missingness_by_location <- missingness_by_location_forecast_week %>%
    dplyr::group_by(location, get(model_id_name)) %>%
    dplyr::summarize(
      missingness_eligibility = ifelse(
        any(any_missing),
        'missing required forecasts',
        'eligible'
      )
    ) %>%
    dplyr::ungroup()
  names(missingness_by_location)[names(missingness_by_location) == 'get(model_id_name)'] <-
    model_id_name

  return(missingness_by_location)
}


#' Compute check of whether quantiles for consecutive targets
#' (1 wk ahead, 2 wk ahead, etc) are non-decreasing for each combination of
#' location, forecast_week_end_date, model, and quantile probability level
#'
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#' @param decrease_tol numeric; decreases of up to specified tolerance are
#'   allowed
#'
#' @return data frame with a row for each combination of
#'   location, forecast week end date, and model and a character column called
#'   'eligibility' with entry 'decreasing quantiles over time' if any forecasted
#'   quantiles were decreasing for the same target across multiple levels
#'
#' @export
calc_nondecreasing_quantile_check <- function(
  qfm,
  decrease_tol = 1
) {
  ## subset to forecasts for most recent week and one week ahead target,
  ## and forecasts for quantile 0.1
  row_index <- attr(qfm, 'row_index')
  col_index <- attr(qfm, 'col_index')
  model_id_name <- attr(qfm, 'model_col')

  eligibility <- purrr::pmap_dfr(
    row_index %>% distinct(location, forecast_week_end_date),
    function(location, forecast_week_end_date) {
      row_inds <- which(row_index$location == location &
        row_index$forecast_week_end_date == forecast_week_end_date)
      row_inds <- row_inds[
        sort(row_index$target[row_inds], index.return=TRUE)$ix
      ]

      purrr::map_dfr(
        unique(col_index[[model_id_name]]),
        function(model_id) {
          col_inds <- which(col_index[[model_id_name]] == model_id)
          diffs <- diff(qfm[row_inds, col_inds])

          result <- row_index[row_inds[1], ] %>% select(-target)
          result[[model_id_name]] <- model_id

          if(any(is.na(diffs))) {
            result[['nondecreasing_quantiles_eligibility']] <- 'missing forecasts; cannot evaluate decreasing quantiles criterion'
          } else if(any(diffs < -decrease_tol)) {
            result[['nondecreasing_quantiles_eligibility']] <- 'decreasing quantiles over time'
          } else {
            result[['nondecreasing_quantiles_eligibility']] <- 'eligible'
          }

          return(result)
        }
      )
    }
  )

  # calculate eligibility for each combination of location and model
  eligibility <- eligibility %>%
    dplyr::group_by(location, get(model_id_name)) %>%
    dplyr::summarize(
      nondecreasing_quantiles_eligibility = ifelse(
        any(nondecreasing_quantiles_eligibility == 'decreasing quantiles over time'),
        'decreasing quantiles over time',
        ifelse(
          any(nondecreasing_quantiles_eligibility == 'missing forecasts; cannot evaluate decreasing quantiles criterion'),
          'missing forecasts; cannot evaluate decreasing quantiles criterion',
          'eligible'
        )
      )
    ) %>%
    dplyr::ungroup()
  names(eligibility)[names(eligibility) == 'get(model_id_name)'] <-
    model_id_name

  return(eligibility)
}
