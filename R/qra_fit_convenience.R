#' Utility function to construct and execute a Zoltar query for all quantile
#' forecasts submitted to a project within a specified number of days of a
#' given date.
#'
#' @param model_abbrs Character vector of model abbreviations
#' @param last_timezero The last timezero date of forecasts to retrieve
#' @param timezero_window_size The number of days back to go.  A window size of
#' 0 will retrieve only forecasts submitted on the `last_timezero` date.
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death')
#' @param zoltar_connection zoltar connection
#' @param project_url zoltar project url
#' @param verbose if TRUE, print messages on job status poll
#'
#' @return data frame of forecasts
do_zoltar_query <- function(
  model_abbrs,
  last_timezero,
  timezero_window_size,
  targets,
  zoltar_connection,
  project_url,
  verbose) {
  # Assemble query
  list_query <- list(
    "model_abbrs" = model_abbrs,
    "types" = list("quantile"),
    "timezeros" = as.character(
      last_timezero + seq(from = -timezero_window_size, to = 0)),
    "targets" = targets
  )

  zoltar_query <- zoltr::query_with_ids(zoltar_connection, project_url, list_query)

  # submit query
  job_url <- zoltr::submit_query(zoltar_connection, project_url, zoltar_query)

  # Poll job
  while (TRUE) {
    the_job_info <- job_info(zoltar_connection, job_url)
    if(verbose) {
      cat(paste0(the_job_info$status, "\n"))
    }
    if (the_job_info$status == "FAILED") {
      stop(paste0("job failed: job_url=", job_url, ", failure_message=", the_job_info$failure_message), call. = FALSE)
    }
    if (the_job_info$status == "SUCCESS") {
      break
    }
    Sys.sleep(1)
  }

  # retrieve results
  all_forecasts <- job_data(zoltar_connection, job_url)

  # remove duplicate rows if there are any
  dup_inds <- which(duplicated(all_forecasts))
  if(length(dup_inds) > 0) {
    all_forecasts <- all_forecasts[-dup_inds, , drop = FALSE]
  }

  return(all_forecasts)
}


#' Download covid forecasts from zoltar and fit one ensemble
#'
#' @param candidate_model_abbreviations_to_include List of model abbreviations
#' for models that may be included in ensemble forecast
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death')
#' @param forecast_week_end_date date of a s
#' @param timezero_window_size The number of days back to go.  A window size of
#' 0 will retrieve only forecasts submitted on the `last_timezero` date.
#' @param window_size size of window
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
#' @param missingness character specifying approach to handling missing
#' forecasts: 'by_location_group', 'rescale', or 'impute'
#' @param impute_method character string specifying method for imputing missing
#' forecasts; currently only 'mean' is supported.
#' @param backend back end used for optimization.
#' @param project_url zoltar project url
#' @param required_quantiles numeric vector of quantiles component models are
#' required to have submitted
#' @param do_q10_check if TRUE, do q10 check
#' @param do_nondecreasing_quantile_check if TRUE, do nondecreasing quantile check
#' @param return_eligibility if TRUE, return model eligibility
#'
#' @return data frame with ensemble forecasts by location
#'
#' @export
build_covid_ensemble_from_zoltar <- function(
  candidate_model_abbreviations_to_include,
  targets,
  forecast_week_end_date,
  timezero_window_size = 1,
  window_size,
  intercept=FALSE,
  constraint,
  quantile_groups,
  missingness,
  impute_method,
  backend,
  project_url = 'https://www.zoltardata.com/api/project/44/',
  required_quantiles,
  do_q10_check,
  do_nondecreasing_quantile_check,
  manual_eligibility_adjust,
  return_eligibility = TRUE,
  return_all = TRUE
) {
  # Set up Zoltar
  zoltar_connection <- new_connection()
  zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

  # Get observed values ("truth" in Zoltar's parlance)
  observed_by_location_target_end_date <-
    covidEnsembles::historical_truths(
      issue_date = as.character(lubridate::ymd(forecast_week_end_date)+1),
      spatial_resolution = c('state', 'national'),
      temporal_resolution = 'weekly'
    ) %>%
    tidyr::pivot_longer(
      cols = c('cum_deaths', 'inc_deaths'),
      names_to = 'base_target',
      values_to = 'observed') %>%
    dplyr::transmute(
      location = location,
      base_target = ifelse(base_target == 'cum_deaths', 'wk ahead cum death', 'wk ahead inc death'),
      target_end_date = date,
      observed = observed
    )
  # observed_by_location_target_end_date <-
  #   zoltr::truth(zoltar_connection, project_url) %>%
  #   dplyr::filter(target %in% targets) %>%
  #   dplyr::transmute(
  #     timezero = timezero,
  #     location = unit,
  #     horizon = as.integer(substr(target, 1, 1)),
  #     base_target = substr(target, 3, nchar(target)),
  #     observed = value,
  #     forecast_week_end_date = calc_forecast_week_end_date(timezero),
  #     target_end_date = calc_target_week_end_date(timezero, horizon)
  #   ) %>%
  #   dplyr::distinct(location, base_target, target_end_date, observed)

  # Dates specifying mondays when forecasts were submitted that are relevant to
  # this analysis.  If forecast_week_end_date is a Saturday, + 2 is a Monday;
  # we then add in the previous window_size weeks
  monday_dates <- forecast_week_end_date + 2 +
    seq(from = -window_size, to = 0, by = 1) * 7

  # obtain the quantile forecasts for required quantiles,
  # and the filter to last submission from each model for each week
  forecasts <-
    # get forecasts from zoltar
    purrr::map_dfr(
      monday_dates,
      do_zoltar_query,
      model_abbrs = candidate_model_abbreviations_to_include,
      timezero_window_size = timezero_window_size,
      targets = targets,
      zoltar_connection = zoltar_connection,
      project_url = project_url,
      verbose = TRUE
    ) %>%
    # keep only required columns and required quantiles
    dplyr::select(model, timezero, location=unit, target, quantile, value) %>%
    dplyr::filter(
      format(quantile, digits=3, nsmall=3) %in%
        format(required_quantiles, digits=3, nsmall=3)) %>%
    # put quantiles in columns
    tidyr::pivot_wider(names_from = quantile, values_from = value) %>%
    # create columns for horizon, forecast week end date of the forecast, and
    # target end date of the forecast
    dplyr::mutate(
      horizon = as.integer(substr(target, 1, 1)),
      forecast_week_end_date = calc_forecast_week_end_date(timezero),
      target_end_date = calc_target_week_end_date(timezero, horizon)
    ) %>%
    # keep only the last submission for each model for a given location, target,
    # and forecast week end date
    dplyr::group_by(
      location, target, forecast_week_end_date, model
    ) %>%
    dplyr::top_n(1, timezero) %>%
    # pivot longer; quantiles are in rows again
    tidyr::pivot_longer(
      cols = all_of(as.character(required_quantiles)),
      names_to = 'quantile',
      values_to = 'value') %>%
    ungroup() %>%
    # add columns with location name and abbreviation
    left_join(fips_codes, by = 'location')

  # obtain ensemble fit(s)
  results <- get_ensemble_fit_and_predictions(
    forecasts=forecasts,
    observed_by_location_target_end_date=observed_by_location_target_end_date,
    forecast_week_end_date=forecast_week_end_date,
    window_size=window_size,
    intercept=intercept,
    constraint=constraint,
    quantile_groups=quantile_groups,
    missingness=missingness,
    impute_method=impute_method,
    backend=backend,
    do_q10_check = do_q10_check,
    do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
    manual_eligibility_adjust = manual_eligibility_adjust,
    return_eligibility = return_eligibility,
    return_all = return_all)

  # return
  return(c(
    results,
    list(forecasts = forecasts)
  ))
}


#' Calculate ensemble fits for a single window size
#'
#' @param forecasts data frame with columns 'model', 'location',
#' 'forecast_week_end_date', 'target', 'quantile', and 'value'
#' @param observed_by_location_target_end_date data frame with columns
#' 'location', 'base_target', 'target_end_date', and 'observed'
#' @param forecast_week_end_date Date object: date of the saturday for the end
#' of the forecast week; week-ahead targets are with respect to this date
#' @param window_size size of window
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
#' @param missingness character specifying approach to handling missing
#' forecasts: 'by_location_group', 'rescale', and 'impute'
#' @param impute_method character string specifying method for imputing missing
#' forecasts; currently only 'mean' is supported.
#' @param backend back end used for optimization.
#' @param do_q10_check if TRUE, do q10 check
#' @param do_nondecreasing_quantile_check if TRUE, do nondecreasing quantile check
#' @param manual_eligibility_adjust character vector of model abbreviations for
#' models eliminated based on visual inspection
#' @param return_eligibility if TRUE, return model eligibility
#' @param return_all if TRUE, return model fits
#'
#' @return tibble or data frame with ensemble fits and results
#'
#' @export
get_ensemble_fit_and_predictions <- function(
  forecasts,
  observed_by_location_target_end_date,
  forecast_week_end_date,
  window_size,
  intercept = FALSE,
  constraint = c('ew', 'convex', 'positive', 'unconstrained'),
  quantile_groups = NULL,
  missingness = c('by_location_group', 'rescale', 'mean_impute'),
  impute_method = 'mean',
  backend = 'quantmod',
  do_q10_check,
  do_nondecreasing_quantile_check,
  manual_eligibility_adjust,
  return_eligibility = TRUE,
  return_all = FALSE) {
  if(missing(forecasts) ||
     missing(forecast_week_end_date) ||
     missing(window_size)) {
    stop("The arguments `forecasts`, `forecast_week_end_date`, and `window_size` must all be provided.")
  }

  constraint <- match.arg(
    constraint,
    choices = c('ew', 'convex', 'positive', 'unconstrained'),
    several.ok = FALSE)

  if(missingness == 'by_location_group') {
    results <- get_by_location_group_ensemble_fits_and_predictions(
      forecasts=forecasts,
      observed_by_location_target_end_date=observed_by_location_target_end_date,
      forecast_week_end_date=forecast_week_end_date,
      window_size=window_size,
      intercept=intercept,
      constraint=constraint,
      quantile_groups=quantile_groups,
      backend=backend,
      do_q10_check = do_q10_check,
      do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
      manual_eligibility_adjust = manual_eligibility_adjust,
      return_eligibility=return_eligibility,
      return_all=return_all)
  } else if(missingness == 'impute') {
    results <- get_imputed_ensemble_fits_and_predictions(
      forecasts=forecasts,
      observed_by_location_target_end_date=observed_by_location_target_end_date,
      forecast_week_end_date=forecast_week_end_date,
      window_size=window_size,
      intercept=intercept,
      constraint=constraint,
      quantile_groups=quantile_groups,
      impute_method=impute_method,
      backend=backend,
      do_q10_check = do_q10_check,
      do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
      manual_eligibility_adjust = manual_eligibility_adjust,
      return_eligibility=return_eligibility,
      return_all=return_all)
  } else if(missingness == 'rescale') {
    results <- get_rescaled_ensemble_fits_and_predictions(
      forecasts=forecasts,
      observed_by_location_target_end_date=observed_by_location_target_end_date,
      forecast_week_end_date=forecast_week_end_date,
      window_size=window_size,
      do_q10_check = do_q10_check,
      do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
      manual_eligibility_adjust = manual_eligibility_adjust,
      return_eligibility=return_eligibility,
      return_all=return_all)
  } else {
    stop('invalid value for argument missingness')
  }

  return(results)
}


#' Calculate ensemble fits separately by location group
#'
#' @param forecasts data frame with columns 'model', 'location',
#' 'forecast_week_end_date', 'target', 'quantile', and 'value'
#' @param observed_by_location_target_end_date data frame with columns
#' 'location', 'base_target', 'target_end_date', and 'observed'
#' @param forecast_week_end_date Date object: date of the saturday for the end
#' of the forecast week; week-ahead targets are with respect to this date
#' @param window_size size of window
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
#' @param backend back end used for optimization.
#' @param do_q10_check if TRUE, do q10 check
#' @param do_nondecreasing_quantile_check if TRUE, do nondecreasing quantile check
#' @param return_all if TRUE, return all quantities; if FALSE, return only some
#' useful summaries
#' @param return_eligibility if TRUE, return model eligibility
#'
#' @return tibble or data frame with ensemble fits and results
get_by_location_group_ensemble_fits_and_predictions <- function(
  forecasts,
  observed_by_location_target_end_date,
  forecast_week_end_date,
  window_size,
  intercept = FALSE,
  constraint = c('ew', 'convex', 'positive', 'unconstrained'),
  quantile_groups = NULL,
  backend = 'quantmod',
  do_q10_check,
  do_nondecreasing_quantile_check,
  manual_eligibility_adjust,
  return_all=FALSE,
  return_eligibility = TRUE) {
  if(missing(forecasts) ||
     missing(forecast_week_end_date) ||
     missing(window_size)) {
    stop("The arguments `forecasts`, `forecast_week_end_date`, and `window_size` must all be provided.")
  }

  constraint <- match.arg(
    constraint,
    choices = c('ew', 'convex', 'positive', 'unconstrained'),
    several.ok = TRUE)

  # obtain model eligibility by location
  # since we have not yet filtered by horizon/target, eligibility is based on
  # all four targets 1 - 4 wk ahead cum deaths
  forecast_matrix <- covidEnsembles::new_QuantileForecastMatrix_from_df(
    forecast_df = forecasts,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'quantile',
    quantile_value_col = 'value'
  )

  model_eligibility <- covidEnsembles::calc_model_eligibility_for_ensemble(
    qfm = forecast_matrix,
    observed_by_location_target_end_date =
      observed_by_location_target_end_date %>%
        dplyr::filter(base_target == paste0('wk ahead cum death')),
    do_q10_check = do_q10_check,
    do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
    window_size = window_size,
    decrease_tol = 0.0
  )

  if(length(manual_eligibility_adjust) > 0) {
    for(i in seq_len(nrow(manual_eligibility_adjust))) {
      el_inds <- which(
        model_eligibility$model == manual_eligibility_adjust$model[i] &
          model_eligibility$location == manual_eligibility_adjust$location[i]
      )
      model_eligibility$overall_eligibility[el_inds] <-
        'Visual misalignment of predictive quantiles with JHU reference data.'
    }
  }

  # convert model eligibility to wide format logical with human readable names
  wide_model_eligibility <- model_eligibility %>%
    dplyr::transmute(
      model = model,
      location = location,
      eligibility = (overall_eligibility == 'eligible')) %>%
    tidyr::pivot_wider(names_from='model', values_from='eligibility')

  # group locations by which models are included per location
  location_groups <- wide_model_eligibility %>%
    dplyr::group_by_if(is.logical) %>%
    dplyr::summarize(locations = list(location)) %>%
    dplyr::ungroup()

  # drop groups with no eligible models
  location_groups <- location_groups[
    apply(as.matrix(location_groups %>% select(-locations)), 1, sum) > 0,
    , drop = FALSE]

  # train/test set up:
  #
  # train:
  #  - if window_size >= 1, training set comprises only forecasts where
  # target_end_date <= forecast_week_end_date
  #  - else if lookback length == 0, just keep horizon 1 for train set
  #
  # test:
  #  - keep all forecasts for current week
  this_week_forecasts_test <-
    forecasts %>%
      dplyr::filter(forecast_week_end_date == UQ(forecast_week_end_date))
  if(window_size >= 1) {
    this_week_forecasts_train <-
      forecasts %>%
        dplyr::filter(target_end_date <= UQ(forecast_week_end_date))
  } else if(window_size == 0) {
    this_week_forecasts_train <- forecasts %>%
      dplyr::filter(horizon == 1)
  }

  # training set QuantileForecastMatrix per location group
  location_groups$qfm_train <- purrr::map(
    seq_len(nrow(location_groups)),
    function(i) {
      model_inds <- location_groups[i, seq_len(ncol(location_groups)-1)] %>%
        as.matrix() %>%
        which()
      models <- colnames(location_groups)[model_inds]
      locations <- location_groups$locations[[i]]

      covidEnsembles::new_QuantileForecastMatrix_from_df(
        forecast_df = this_week_forecasts_train %>%
          dplyr::filter(
            model %in% models,
            location %in% locations),
        model_col = 'model',
        id_cols = c('location', 'forecast_week_end_date', 'target'),
        quantile_name_col = 'quantile',
        quantile_value_col = 'value',
        drop_missing_id_levels = TRUE
      )
    })

  # test set QuantileForecastMatrix per location group
  location_groups$qfm_test <- purrr::map(
    seq_len(nrow(location_groups)),
    function(i) {
      model_inds <- location_groups[i, seq_len(ncol(location_groups)-2)] %>%
        as.matrix() %>%
        which()
      models <- colnames(location_groups)[model_inds]
      locations <- location_groups$locations[[i]]

      covidEnsembles::new_QuantileForecastMatrix_from_df(
        forecast_df = this_week_forecasts_test %>%
          dplyr::filter(
            model %in% models,
            location %in% locations),
        model_col = 'model',
        id_cols = c('location', 'forecast_week_end_date', 'target'),
        quantile_name_col = 'quantile',
        quantile_value_col = 'value'
      )
    })

  # observed responses to date
  location_groups$y_train <- purrr::map(
    location_groups$qfm_train,
    function(qfm_train) {
      attr(qfm_train, 'row_index') %>%
        dplyr::mutate(
          target_end_date = as.character(
            lubridate::ymd(forecast_week_end_date) + as.numeric(substr(target, 1, 1))*7
          ),
          base_target = substr(target, 3, nchar(target))
        ) %>%
        dplyr::left_join(
          observed_by_location_target_end_date,
          by = c('location', 'target_end_date', 'base_target')) %>%
        dplyr::pull(observed)
    })

  # fit ensembles and obtain predictions per group
  if(constraint == 'ew') {
    location_groups$qra_fit <- purrr::map(
      location_groups$qfm_train,
      estimate_qra,
      constraint = 'ew')
  } else {
    location_groups[['qra_fit']] <- purrr::pmap(
      location_groups %>% select(qfm_train, y_train, qfm_test),
      function(qfm_train, y_train, qfm_test) {
        estimate_qra(
          qfm_train = qfm_train,
          y_train = y_train,
          qfm_test = qfm_test,
          intercept = intercept,
          constraint = constraint,
          quantile_groups = quantile_groups,
          backend = backend)
      })
  }

  # obtain predictions
  location_groups[['qra_forecast']] <- purrr::pmap(
    location_groups %>% dplyr::select(qra_fit, qfm_test),
    function(qra_fit, qfm_test) {
      predict(qra_fit, qfm_test) %>%
        as.data.frame()
    }
  )

  # return
  if(return_all) {
    result <- list(
      model_eligibility = model_eligibility,
      wide_model_eligibility = wide_model_eligibility,
      location_groups = location_groups)
  } else {
    # unpack across location groups
    if(return_eligibility) {
      result <- list(
        forecasts = dplyr::bind_rows(location_groups[['qra_forecast']]),
        model_eligibility = model_eligibility,
        wide_model_eligibility = wide_model_eligibility
      )
    } else {
      result <- dplyr::bind_rows(location_groups[['qra_forecast']])
    }
  }

  return(result)
}


#' Impute missing values for each quantile level in a quantile forecast matrix
#' It is assumed that in each row, all quantiles for a given model are either
#' missing or available.
#'
#' @param qfm a QuantileForecastMatrix
#' @param impute_method character string specifying method for imputing;
#' currently only 'mean' is supported
#'
#' @return list of two items:
#' 1. 'qfm_imputed' the input QuantileForecastMatrix object with missing values
#' imputed
#' 2. 'weight_transfer' a square matrix of dimension equal to the number of
#' unique models in qfm.  Entry [i, j] is the proportion of imputed observations
#' for model j that are attributable to model i.
#'
#' @export
impute_missing_per_quantile <- function(qfm, impute_method = 'mean') {
  col_index <- attr(qfm, 'col_index')
  model_col <- attr(qfm, 'model_col')
  quantile_name_col <- attr(qfm, 'quantile_name_col')
  quantile_levels <- col_index[[quantile_name_col]]
  unique_quantile_levels <- unique(quantile_levels)
  num_models <- length(unique(col_index[[model_col]]))

  X_na <- is.na(qfm)

  missingness_groups <- X_na %>%
    as.data.frame() %>%
    mutate(row_num = dplyr::row_number()) %>%
    dplyr::group_by_at(seq_len(ncol(.) - 1)) %>%
    dplyr::summarise(row_inds = list(row_num))

  qfm_imputed <- qfm
  qfm_imputed[is.na(qfm_imputed)] <- 0.0

  weight_transfer <- matrix(0, nrow = num_models, ncol = num_models)

  for(i in seq_len(nrow(missingness_groups))) {
    row_inds <- missingness_groups$row_inds[[i]]

    impute_mat <- diag(num_models)

    col_inds <- which(quantile_levels == unique_quantile_levels[1])
    temp <- !is.na(unclass(qfm)[row_inds[1], col_inds])
    temp <- temp / sum(temp)

    for(j_ind in seq_along(col_inds)) {
      j <- col_inds[j_ind]
      if(is.na(qfm[row_inds[1], j])) {
        impute_mat[, j_ind] <- temp
      }
    }

    for(quantile_level in unique_quantile_levels) {
      col_inds <- which(quantile_levels == quantile_level)
      qfm_imputed[row_inds, col_inds] <-
        qfm_imputed[row_inds, col_inds, drop = FALSE] %*% impute_mat
    }

    weight_transfer <- weight_transfer + length(row_inds) * impute_mat
  }
  weight_transfer <- weight_transfer / nrow(qfm)

  return(list(
    qfm_imputed = qfm_imputed,
    weight_transfer = weight_transfer
  ))
}


#' Calculate ensemble fits after imputing missing forecasts
#'
#' @param forecasts data frame with columns 'model', 'location',
#' 'forecast_week_end_date', 'target', 'quantile', and 'value'
#' @param observed_by_location_target_end_date data frame with columns
#' 'location', 'base_target', 'target_end_date', and 'observed'
#' @param forecast_week_end_date Date object: date of the saturday for the end
#' of the forecast week; week-ahead targets are with respect to this date
#' @param window_size size of window
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
#' @param impute_method character string specifying method for imputing missing
#' forecasts; currently only 'mean' is supported.
#' @param backend back end used for optimization.
#' @param do_q10_check if TRUE, do q10 check
#' @param do_nondecreasing_quantile_check if TRUE, do nondecreasing quantile check
#' @param return_all if TRUE, return all quantities; if FALSE, return only some
#' useful summaries
#' @param return_eligibility if TRUE, return model eligibility
#'
#' @return tibble or data frame with ensemble fits and results
#'
#' @export
get_imputed_ensemble_fits_and_predictions <- function(
  forecasts,
  observed_by_location_target_end_date,
  forecast_week_end_date,
  window_size,
  intercept = FALSE,
  constraint = c('ew', 'convex', 'positive', 'unconstrained'),
  quantile_groups = NULL,
  impute_method = 'mean',
  backend = 'quantmod',
  do_q10_check,
  do_nondecreasing_quantile_check,
  manual_eligibility_adjust,
  return_all=FALSE,
  return_eligibility = TRUE) {
  if(missing(forecasts) ||
     missing(forecast_week_end_date) ||
     missing(window_size)) {
    stop("The arguments `forecasts`, `forecast_week_end_date`, and `window_size` must all be provided.")
  }

  constraint <- match.arg(
    constraint,
    choices = c('ew', 'convex', 'positive', 'unconstrained'),
    several.ok = TRUE)

  # obtain model eligibility by location
  # since we have not yet filtered by horizon/target, eligibility is based on
  # all four targets 1 - 4 wk ahead cum deaths
  forecast_matrix <- covidEnsembles::new_QuantileForecastMatrix_from_df(
    forecast_df = forecasts,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'quantile',
    quantile_value_col = 'value'
  )

  model_eligibility <- covidEnsembles::calc_model_eligibility_for_ensemble(
    qfm = forecast_matrix,
    observed_by_location_target_end_date =
      observed_by_location_target_end_date %>%
      dplyr::filter(base_target == paste0('wk ahead cum death')),
    do_q10_check = do_q10_check,
    do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
    window_size = window_size,
    decrease_tol = 0.0
  )

  if(length(manual_eligibility_adjust) > 0) {
    for(i in seq_len(nrow(manual_eligibility_adjust))) {
      el_inds <- which(
        model_eligibility$model == manual_eligibility_adjust$model[i] &
          model_eligibility$location == manual_eligibility_adjust$location[i]
      )
      model_eligibility$overall_eligibility[el_inds] <-
        'Visual misalignment of predictive quantiles with JHU reference data.'
    }
  }

  # convert model eligibility to wide format logical with human readable names
  wide_model_eligibility <- model_eligibility %>%
    dplyr::transmute(
      model = model,
      location = location,
      eligibility = (overall_eligibility == 'eligible')) %>%
    tidyr::pivot_wider(names_from='model', values_from='eligibility')

  # keep only models that are eligible for inclusion in at least one location
  models_to_keep <- apply(
    wide_model_eligibility %>% select(-location),
    2,
    function(el) {any(el != FALSE)}) %>%
    which() %>%
    names()

  wide_model_eligibility <- wide_model_eligibility[, c('location', models_to_keep)]

  col_index <- attr(forecast_matrix, 'col_index')
  cols_to_keep <- which(col_index[['model']] %in% models_to_keep)
  forecast_matrix <- forecast_matrix[, cols_to_keep]

  # drop rows with no eligible models
  rows_to_keep <- apply(forecast_matrix, 1, function(qfm_row) any(!is.na(qfm_row))) %>%
    which()

  if(length(rows_to_keep) != nrow(forecast_matrix)) {
#    dropped_rows <- forecast_matrix[-rows_to_keep, ]
    forecast_matrix <- forecast_matrix[rows_to_keep, ]
  }

  # get train/test inds
  # train:
  #  - if window_size >= 1, training set comprises only forecasts where
  # target_end_date <= forecast_week_end_date
  #  - else if window_size == 0, just keep horizon 1 for train set
  col_index <- attr(forecast_matrix, 'col_index')
  row_index <- attr(forecast_matrix, 'row_index')
  if(window_size >= 1) {
    target_end_date <- row_index %>%
      dplyr::mutate(
        target_end_date = as.character(
          lubridate::ymd(forecast_week_end_date) + as.numeric(substr(target, 1, 1))*7
        )
      ) %>%
      pull(target_end_date)

    train_row_inds <- which(target_end_date < forecast_week_end_date)
    test_row_inds <- which(row_index[['forecast_week_end_date']] == forecast_week_end_date)
  } else {
    train_row_inds <- which(row_index[['forecast_week_end_date']] == forecast_week_end_date)
    test_row_inds <- which(substr(row_index$target, 1, 4) == '1 wk')
  }

  # training set and test set QuantileForecastMatrix
  qfm_train <- forecast_matrix[train_row_inds, ]
  qfm_test <- forecast_matrix[test_row_inds, ]

  # impute missing values
  c(imputed_qfm_train, weight_transfer) %<-% impute_missing_per_quantile(
    qfm=qfm_train,
    impute_method = 'mean')
  c(imputed_qfm_test, test_weight_transfer) %<-% impute_missing_per_quantile(
    qfm=qfm_test,
    impute_method = 'mean')

  # observed responses to date
  y_train <- attr(qfm_train, 'row_index') %>%
    dplyr::mutate(
      target_end_date = as.character(
        lubridate::ymd(forecast_week_end_date) + as.numeric(substr(target, 1, 1))*7
      ),
      base_target = substr(target, 3, nchar(target))
    ) %>%
    dplyr::left_join(
      observed_by_location_target_end_date,
      by = c('location', 'target_end_date', 'base_target')
    ) %>%
    dplyr::pull(observed)

  # fit ensembles and obtain predictions per group
  if(constraint == 'ew') {
    qra_fit <- estimate_qra(imputed_qfm_train, constraint = 'ew')
  } else {
    qra_fit <- estimate_qra(
      qfm_train = imputed_qfm_train,
      y_train = y_train,
      qfm_test = imputed_qfm_test,
      intercept = intercept,
      constraint = constraint,
      quantile_groups = quantile_groups,
      backend = backend)

    # do weight transfer among models
    if(nrow(qra_fit$coefficients) == nrow(weight_transfer)) {
      # single weight per model
      qra_fit$coefficients$beta <-
        weight_transfer %*% matrix(qra_fit$coefficients$beta)
    } else {
      # weight per quantile; adjust by iterating through quantile levels
      qs <- qra_fit[[attr(qfm_train, 'quantile_name_col')]]
      for(q in unique(qs)) {
        row_inds <- which(qs == q)
        qra_fit$coefficients$beta[row_inds] <-
          weight_transfer %*% matrix(qra_fit$coefficients$beta[row_inds])
      }
    }
  }

  # obtain predictions
  qra_forecast <- predict(qra_fit, imputed_qfm_test) %>% as.data.frame()

  # return
  if(return_all) {
    result <- list(
      model_eligibility = model_eligibility,
      wide_model_eligibility = wide_model_eligibility,
      location_groups = list(
        locations = unique(attr(qfm_train, 'row_index')[['location']]),
        qfm_train = qfm_train,
        qfm_test = qfm_test,
        imputed_qfm_train = imputed_qfm_train,
        imputed_qfm_test = imputed_qfm_test,
        qra_fit = qra_fit,
        qra_forecast = qra_forecast
      ),
      weight_transfer = weight_transfer
    )
  } else {
    stop('unsupported option for deprecated parameter return_all')
  }

  return(result)
}


#' Calculate rescaled convex ensemble fit, renormalizing in case of missing models
#'
#' @param forecasts data frame with columns 'model', 'location',
#' 'forecast_week_end_date', 'target', 'quantile', and 'value'
#' @param observed_by_location_target_end_date data frame with columns
#' 'location', 'base_target', 'target_end_date', and 'observed'
#' @param forecast_week_end_date Date object: date of the saturday for the end
#' of the forecast week; week-ahead targets are with respect to this date
#' @param window_size size of window
#'
#' @return tibble or data frame with ensemble fits and results
get_rescaled_ensemble_fits_and_predictions <- function(
  forecasts,
  observed_by_location_target_end_date,
  forecast_week_end_date,
  window_size,
  do_q10_check,
  do_nondecreasing_quantile_check,
  manual_eligibility_adjust,
  return_all=FALSE,
  return_eligibility = TRUE) {
  # for development/debugging
  #forecast_week_end_date <- lubridate::ymd("2020-05-02")
  #window_size <- 2L

  if(window_size == 0) {
    stop("window size must be >= 1 for rescaled convex ensemble!")
  }

  # obtain model eligibility for each model in the current week.
  # - drop forecasts for all weeks from models that are not eligible for any
  #   location in the current week
  # - drop forecasts for the current week for model-location combinations where
  #   the model is ineligible
  forecast_matrix <- covidEnsembles::new_QuantileForecastMatrix_from_df(
    forecast_df = forecasts %>%
      filter(forecast_week_end_date == UQ(forecast_week_end_date)),
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'quantile',
    quantile_value_col = 'value'
  )

  model_eligibility <- covidEnsembles::calc_model_eligibility_for_ensemble(
    qfm = forecast_matrix,
    observed_by_location_target_end_date =
      observed_by_location_target_end_date,
    do_q10_check = do_q10_check,
    do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
    window_size = 0,
    decrease_tol = 0.0
  )

  models_ineligible_all_locations <- model_eligibility %>%
    group_by(model) %>%
    summarize(all_ineligible = all(overall_eligibility != 'eligible')) %>%
    filter(all_ineligible) %>%
    pull(model)

  forecasts <- forecasts %>%
    dplyr::filter(!(model %in% models_ineligible_all_locations)) %>%
    dplyr::left_join(model_eligibility %>%
      dplyr::filter(!(model %in% models_ineligible_all_locations)) %>%
      dplyr::select(location, model, overall_eligibility)) %>%
    dplyr::filter(overall_eligibility == 'eligible' |
      forecast_week_end_date != UQ(forecast_week_end_date)) %>%
    dplyr::select(-overall_eligibility)

  # obtain model eligibility for each model separately for past weeks in the
  # window.
  # - drop forecasts for that week for model-location combinations where
  #   the model is ineligible
  for(w in seq_len(window_size)) {
    forecast_matrix <- covidEnsembles::new_QuantileForecastMatrix_from_df(
      forecast_df = forecasts %>%
        filter(forecast_week_end_date == UQ(forecast_week_end_date) - 7*w),
      model_col = 'model',
      id_cols = c('location', 'forecast_week_end_date', 'target'),
      quantile_name_col = 'quantile',
      quantile_value_col = 'value'
    )

    model_eligibility <- covidEnsembles::calc_model_eligibility_for_ensemble(
      qfm = forecast_matrix,
      observed_by_location_target_end_date =
        observed_by_location_target_end_date %>%
        dplyr::filter(base_target == paste0('wk ahead cum death')),
      do_q10_check = do_q10_check,
      do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
      window_size = 0,
      decrease_tol = 0.0
    )

    forecasts <- forecasts %>%
      dplyr::filter(!(model %in% models_ineligible_all_locations)) %>%
      dplyr::left_join(model_eligibility %>%
                         dplyr::filter(!(model %in% models_ineligible_all_locations)) %>%
                         dplyr::select(location, model, overall_eligibility)) %>%
      dplyr::filter(overall_eligibility == 'eligible' |
                      forecast_week_end_date != UQ(forecast_week_end_date) - 7*w) %>%
      dplyr::select(-overall_eligibility)
  }


  # create training and test sets
  this_week_forecasts_train <-
    forecasts %>%
    filter(target_end_date <= UQ(forecast_week_end_date))

  this_week_forecasts_test <-
    forecasts %>%
    filter(
      forecast_week_end_date == UQ(forecast_week_end_date),
      model %in% this_week_forecasts_train$model
    )

  this_week_forecasts_train <- this_week_forecasts_train %>%
    filter(model %in% this_week_forecasts_test$model)

  qfm_train <- new_QuantileForecastMatrix_from_df(
    forecast_df = this_week_forecasts_train,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'quantile',
    quantile_value_col = 'value',
    drop_missing_id_levels = TRUE
  )

  qfm_test <- new_QuantileForecastMatrix_from_df(
    forecast_df = this_week_forecasts_test %>%
      left_join(
        observed_by_location_target_end_date %>%
          dplyr::filter(base_target == paste0('wk ahead cum death')),
        by = c('location', 'target_end_date')) %>%
      filter(
        forecast_week_end_date == max(forecast_week_end_date),
        !is.na(observed)),
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'quantile',
    quantile_value_col = 'value'
  )

  y_train <- attr(qfm_train, 'row_index') %>%
    dplyr::mutate(
      target_end_date = as.character(
        lubridate::ymd(forecast_week_end_date) + as.numeric(substr(target, 1, 1))*7
      ),
      base_target = substr(target, 3, nchar(target))
    ) %>%
    dplyr::left_join(
      observed_by_location_target_end_date %>%
        dplyr::filter(base_target == paste0('wk ahead cum death')),
      by = c('location', 'target_end_date', 'base_target')) %>%
    dplyr::pull(observed)

  rescaled_convex_qra_fit <- estimate_qra(
    qfm_train = qfm_train,
    y_train = y_train,
    qra_model = 'rescaled_convex_per_model',
    backend = 'optim')[[2]]

  return(
    list(
      rescaled_convex_qra_fit = rescaled_convex_qra_fit,
      qfm_test = qfm_test,
      predicted_test = predict(rescaled_convex_qra_fit, qfm_test)
    )
  )
}

