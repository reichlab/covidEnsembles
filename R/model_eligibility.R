#' Generate a data frame with a row for each model and an indicator of whether
#' that model is eligible for inclusion in an ensemble, with explanation if not
#'
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#' @param observed_by_location_target_end_date data frame of observed values
#' @param missingness_by_target logical; if TRUE, check condition that
#' forecasts are not missing for each combination of model, week, and target;
#' otherwise, 
#' @param do_q10_check logical; if TRUE, check condition that quantile at
#'   level 0.1 is at least as large as the most recent observed value
#' @param do_nondecreasing_quantile_check logical; if TRUE, check condition
#'   that quantiles for consecutive targets (1 wk ahead, 2 wk ahead, etc) are
#'   non-decreasing for each combination of location, forecast_week_end_date,
#'   model, and quantile probability level
#' @param do_baseline_check logical; if TRUE, check condition that WIS for model
#'   is within specified tolerance of WIS for baseline
#' @param do_sd_check logical; if TRUE, check condition that mean of next (7) predicted
#'   medians is not more than (4) sample standard deviations below the mean of the last
#'   (14) observations
#' @param window_size non-negative integer number of historic weeks that
#'   are examined for forecast missingness; 0 is appropriate for equal weight
#'   ensembles where no historical data is required.  If two past weeks of
#'   forecast data are required to estimate ensemble parameters, window_size
#'   should be 2
#' @param decrease_tol numeric; decreases of up to specified tolerance are
#'   allowed
#' @param baseline_tol numeric; for baseline check, model's mean WIS for
#'   forecasts within the window size must be at most baseline_tol times the
#'   mean wis for the baseline model on the corresponding forecasts
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
  missingness_by_target = FALSE,
  do_q10_check = TRUE,
  do_nondecreasing_quantile_check = TRUE,
  do_baseline_check = FALSE,
  do_sd_check = FALSE,
  window_size = 0,
  decrease_tol = 1.0,
  baseline_tol = 1.2
) {
  # subset to rows representing forecasts within window_size
  row_index <- attr(qfm, 'row_index')
  if(window_size+1 > length(unique(row_index$forecast_week_end_date))) {
    stop('not enough forecast weeks in qfm to support requested window size')
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
  eligibility <- calc_forecast_missingness(
    qfm,
    by_target = missingness_by_target,
    by_week = missingness_by_target)

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

  # check for forecast skill comparable to baseline
  if(do_baseline_check) {
    baseline_check <- calc_baseline_check(
      qfm,
      observed_by_location_target_end_date,
      baseline_tol)

    # combine eligibility check results
    eligibility <- eligibility %>%
      dplyr::left_join(baseline_check,
                       by = c("location", model_id_name))
  } else {
    eligibility$baseline_eligibility <- 'NA'
  } 

  if(do_sd_check) {
    sd_check <- calc_sd_check(
      qfm,
      observed_by_location_target_end_date)

    # combine eligibility check results
    eligibility <- eligibility %>%
      dplyr::left_join(sd_check,
                       by = c("location", model_id_name))
  } else {
    eligibility$sd_eligibility <- 'NA'
  }

  eligibility$overall_eligibility <- ifelse(
    eligibility$missingness_eligibility == 'eligible' &
      eligibility$q10_eligibility %in% c('eligible', 'NA') &
      eligibility$nondecreasing_quantiles_eligibility %in% c('eligible', 'NA') &
      eligibility$baseline_eligibility %in% c('eligible', 'NA') &
      eligibility$sd_eligibility %in% c('eligible', 'NA'),
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
    dplyr::left_join(
      observed_by_location_target_end_date %>%
        dplyr::mutate(target_end_date = lubridate::ymd(target_end_date)),
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
#' if FALSE results are summarized across weeks.
#' @param by_target logical; if TRUE, results are returned by target, and if
#' FALSE results are summarized across targets.  by_target may only be set to
#' TRUE if by_week is TRUE.
#'
#' @return data frame with a row for each combination of
#'   location, forecast week end date (if requestrd), and model and a logical column called
#'   'any_missing' with entry TRUE if any forecasts were missing across all
#'   quantiles and targets
#'
#' @export
calc_forecast_missingness <- function(
  qfm,
  by_target = FALSE,
  by_week = FALSE
) {
  # validate that if by_target is TRUE, so is by_week
  if(by_target & !by_week) {
    stop("by_target may only be set to TRUE if by_week is TRUE.")
  }
  
  # calculate missingness for each combination of location, forecast week end
  # date, and model.  Also group by target if requested.
  row_index <- attr(qfm, 'row_index')
  col_index <- attr(qfm, 'col_index')
  model_id_name <- attr(qfm, 'model_col')

  if(by_target) {
    missingness_by_location_forecast_week <- purrr::pmap_dfr(
      row_index %>% distinct(location, forecast_week_end_date, target),
      function(location, forecast_week_end_date, target) {
        row_inds <- which(row_index$location == location &
          row_index$forecast_week_end_date == forecast_week_end_date &
          row_index$target == target)

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
  } else {
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
  }

  if(by_week) {
    missingness_by_location_forecast_week <-
      missingness_by_location_forecast_week %>%
        dplyr::mutate(
          missingness_eligibility = ifelse(
            any_missing,
            "missing required forecasts",
            "eligible"
          )
        ) %>%
        dplyr::select(-any_missing)
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


#' Compute check of whether predictive performance is comparable to a baseline
#' model
#'
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#' @param observed_by_location_target_end_date data frame of observed values
#' @param baseline_tol numeric; for baseline check, model's mean WIS for
#'   forecasts within the window size must be at most baseline_tol times the
#'   mean wis for the baseline model on the corresponding forecasts
#'
#' @return data frame with a row for each combination of
#'   location and model, and a character column called
#'   'baseline_eligibility' with entry 'windowed wis greater than baseline'
#'   if the mean wis for forecasts made by this model in the past window weeks
#'   is greater than the mean wis for the corresponding forecasts from a
#'   baseline model
#'
#' @export
calc_baseline_check <- function(
  qfm,
  observed_by_location_target_end_date,
  baseline_tol = 1
) {
  row_index <- attr(qfm, 'row_index')
  col_index <- attr(qfm, 'col_index')
  model_id_name <- attr(qfm, 'model_col')

  all_locations <- unique(row_index$location)
  forecast_week_end_dates <- unique(row_index$forecast_week_end_date)

  baseline_scores <- purrr::map_dfr(
    covidEnsembles::baseline_forecasts %>%
      dplyr::filter(forecast_week_end_date %in% forecast_week_end_dates) %>%
      pull(forecasts),
    function(forecast_df) {
      baseline_qfm <- covidEnsembles::new_QuantileForecastMatrix_from_df(
        forecast_df = forecast_df,
        model_col = 'model',
        id_cols = c('location', 'forecast_week_end_date', 'target'),
        quantile_name_col = 'quantile',
        quantile_value_col = 'value',
        drop_missing_id_levels = TRUE
      )

      get_all_wis_components(
        qfm = baseline_qfm,
        observed_by_location_target_end_date = observed_by_location_target_end_date)
    }
  )

  eligibility <- purrr::map_dfr(
    unique(col_index[[model_id_name]]),
    function(model_id) {
      # keep predictions from this model
      col_inds <- which(col_index[[model_id_name]] == model_id)
      qfm <- qfm[, col_inds]

      # keep only locations/targets where the model submitted all 23 quantiles
      rows_to_keep <- apply(
        unclass(qfm),
        1,
        function(qfm_row) {
          all(!is.na(qfm_row))
        }
      ) %>% which()
      qfm <- qfm[rows_to_keep, ]

      if(nrow(qfm) == 0) {
        return(data.frame(
          model = model_id,
          location = all_locations,
          baseline_eligibility = 'missing forecasts',
          stringsAsFactors = FALSE
        ))
      } else {
        wis_ratio <- get_all_wis_components(
            qfm = qfm,
            observed_by_location_target_end_date = observed_by_location_target_end_date) %>%
          dplyr::mutate(model = model_id) %>%
          dplyr::select(model, location, forecast_week_end_date, target, wis) %>%
          dplyr::mutate(base_target = substr(target, 3, nchar(target))) %>%
          dplyr::left_join(
            baseline_scores %>%
              dplyr::select(location, forecast_week_end_date, target, wis),
            by = c('location', 'forecast_week_end_date', 'target')) %>%
          dplyr::ungroup() %>%
          dplyr::summarize(
            mean_wis_model = mean(wis.x),
            mean_wis_baseline = mean(wis.y)) %>%
          dplyr::mutate(wis_ratio_model_to_baseline = mean_wis_model/mean_wis_baseline) %>%
          dplyr::pull(wis_ratio_model_to_baseline)

        return(data.frame(
          model = model_id,
          location = all_locations,
          baseline_eligibility = ifelse(
            wis_ratio <= baseline_tol,
            'eligible',
            'mean wis for model > mean wis for baseline'
          ),
          stringsAsFactors = FALSE
        ))
      }
    })

  names(eligibility)[names(eligibility) == 'model'] <- model_id_name

  return(eligibility)
}



#' Compute check of whether mean of forecast medians in `lookahead_window`
#' is more than `num_sd` sample standard deviations below the sample mean of observations
#' in `lookback_window`.
#'
#' @param qfm matrix of model forecasts of class QuantileForecastMatrix
#' @param observed_by_location_target_end_date data frame of observed values
#'
#' @return data frame with a row for each combination of
#'   location and model and a logical column called
#'   'eligibility' with entry "more than four SDs below 2-week mean of 
#'   hospitalization point forecasts" if check fails
#'
#' @export
calc_sd_check <- function(
  qfm,
  observed_by_location_target_end_date,
  lookback_window = 14,
  lookahead_window = 7,
  num_sd = 4,
  show_stats = FALSE,
  show_plot = FALSE
) {
  ## subset to median forecasts over lookahead_window 
  row_index <- attr(qfm, 'row_index')
  rows_to_keep <- as.numeric(stringr::word(row_index$target)) <= lookahead_window
  row_index <- row_index[rows_to_keep,]

  col_index <- attr(qfm, 'col_index')
  quantile_name_col <- attr(qfm, 'quantile_name_col')
  cols_to_keep <- which(col_index[[quantile_name_col]] == "0.5")
  col_index <- col_index[cols_to_keep, ]
  
  model_id_name <- attr(qfm, 'model_col')

  qfm <- qfm[rows_to_keep, cols_to_keep]
  
  day0 <- unique(row_index$forecast_week_end_date)
  if (length(day0) > 1) {
    stop("SD exclusion only implemented for single time zero")
  }

  # get summary stats for lookback period of observed values
  past <- observed_by_location_target_end_date %>% 
    dplyr::filter(between(lubridate::ymd(target_end_date),
                          lubridate::ymd(day0) - lookback_window - 1,
                          lubridate::ymd(day0))) %>% 
    dplyr::group_by(location) %>% 
    dplyr::summarise(past_mean = mean(observed, na.rm = TRUE),
                     sd = sd(observed, na.rm = TRUE),
                     m_sd = past_mean - num_sd*sd,
                     .groups = "drop")
  
  eligibility <- purrr::pmap_dfr(
    row_index %>% dplyr::distinct(location, forecast_week_end_date),
    function(location, forecast_week_end_date) {
      row_inds <- which(row_index$location == location &
                          row_index$forecast_week_end_date == forecast_week_end_date)
      row_inds <- row_inds[
        sort(row_index$target[row_inds], index.return=TRUE)$ix
      ]
      
      purrr::map_dfr(
        unique(col_index[[model_id_name]]),
        function(model_id) {
          col_ind <- which(col_index[[model_id_name]] == model_id)
          mean_ahead <- mean(qfm[row_inds, col_ind], na.rm = TRUE)
          cutoff <- past %>% dplyr::filter(location == !!location) %>% pull(m_sd)
          criterion <- mean_ahead < cutoff
          
          result <- row_index[row_inds[1], ] %>% dplyr::select(-target)
          result[[model_id_name]] <- model_id
          
          if(is.nan(mean_ahead)) {
            result[['sd_eligibility']] <- paste0('missing forecasts; cannot evaluate less than ',
                                               num_sd,
                                               ' SD below criterion')
          } else if(criterion) {
            result[['sd_eligibility']] <- paste0('mean of next ',
                                               lookahead_window,
                                               ' forecasted medians more than ',
                                               num_sd,
                                               ' SD below mean of past ',
                                               lookback_window,
                                               ' observations')
          } else {
            result[['sd_eligibility']] <- 'eligible'
          }

          if (show_stats | show_plot) result$mean_ahead <- mean_ahead
          
          return(result)
        }
      )
    }
  )

  eligibility <- eligibility %>% as_tibble()
  if (show_stats | show_plot) {
    eligibility <- eligibility %>% 
      dplyr::left_join(past, by = "location") %>% 
      dplyr::left_join(fips_codes[,1:2])
  }

  if (show_plot) {
    past <- past %>% dplyr::left_join(fips_codes[,1:2])
    p <- ggplot(eligibility, aes(x = model, y = mean_ahead, color = model)) +
      geom_point() +
      geom_hline(data = past, aes(yintercept = past_mean), alpha = .4) +
      geom_hline(data = past, aes(yintercept = past_mean - num_sd*sd), linetype = "dashed") +
      geom_hline(data = past, aes(yintercept = past_mean + num_sd*sd), linetype = "dashed") +
      facet_wrap(~location_name, scales = "free_y") +
      theme_bw() +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    return(list(tbl = eligibility, plot = p))
  }

  # return(list(qfm, past, day0))
  return(eligibility)

}
