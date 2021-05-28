
#' load covid forecasts from local file
#'
#' @param model_abbrs Character vector of model abbreviations
#' @param last_timezero The last timezero date of forecasts to retrieve
#' @param timezero_window_size The number of days back to go.  A window size of
#' 0 will retrieve only forecasts submitted on the `last_timezero` date.
#' @param types character vector of types to retrieve, for example
#' c("quantile", "point") to keep both quantile and point forecasts
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death')
#' @param submissions_root path to the data-processed folder of the
#' covid19-forecast-hub repository
#' @param keep_last logical; if TRUE, keep only the last identified file
#' within the submission time frame determined by last_timezero and
#' timezero_window_size. if FALSE, keep all files.
#'
#' @return data frame with columns model, timezero, location, target, quantile,
#' value
#'
#' @export
load_covid_forecasts <- function(
  model_abbrs,
  last_timezero,
  timezero_window_size,
  types = "quantile",
  targets,
  submissions_root,
  keep_last = TRUE) {
  submission_dates <- as.character(last_timezero +
    seq(from = -timezero_window_size, to = 0, by = 1))

  results <- purrr::map_dfr(
    model_abbrs,
    function(model_abbr) {
      # identify submission files within specified dates
      results_paths <- Sys.glob(paste0(submissions_root, model_abbr,
          "/*", submission_dates, "-", model_abbr, ".csv"))

      if (keep_last) {
        results_paths <- tail(results_paths, 1)
      }

      # if no results, return
      if (length(results_paths) == 0) {
        return(NULL)
      }

      purrr::map_dfr(
        results_paths,
        function(results_path) {
          readr::read_csv(results_path,
            col_types = cols(
              forecast_date = col_date(format = "%Y-%m-%d"),
              target = col_character(),
              target_end_date = col_date(format = "%Y-%m-%d"),
              location = col_character(),
              type = col_character(),
              quantile = col_double(),
              value = col_double()
            )) %>%
            dplyr::filter(
              tolower(type) %in% types,
              tolower(target) %in% targets) %>%
            dplyr::transmute(
              model = model_abbr,
              timezero = forecast_date,
              location = location,
              target = tolower(target),
              target_end_date = target_end_date,
              quantile = quantile,
              value = value
            )
        }
      )
    }
  )

  return(results)
}

#' Load forecasts from multiple models over multiple weeks, keep only the last
#' forecast from each model each week, and adjust the targets for daily
#' forecasts to be relative to the Monday on or after submission.
#'
#' @param monday_dates Date vector of Mondays that are submission deadlines
#' @param model_abbrs Character vector of model abbreviations
#' @param timezero_window_size The number of days back to go.  A window size of
#' 0 will retrieve only forecasts submitted on the `last_timezero` date.
#' @param locations character vector of locations; FIPS codes
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death')
#' @param horizon maximum horizon relative to the monday dates that should be
#' retained
#' @param required_quantiles numeric vector of quantiles component models are
#' required to have submitted
#' @param submissions_root path to the data-processed folder of the
#' covid19-forecast-hub repository
#' @param include_null_point_forecasts boolean; if TRUE, quantile forecasts are
#' augmented with NA point forecasts from all models that submitted point
#' forecasts
#' @param keep_last logical; if TRUE, keep only the last identified file
#' within each week. if FALSE, keep all files.
#'
#' @return data frame with columns model, timezero, location, target, quantile,
#' value
#'
#' @export
load_covid_forecasts_relative_horizon_old <- function(
  monday_dates,
  model_abbrs,
  timezero_window_size,
  locations,
  targets,
  horizon,
  required_quantiles,
  submissions_root,
  include_null_point_forecasts = FALSE,
  keep_last = TRUE) {
  # obtain the quantile forecasts for required quantiles,
  # and the filter to last submission from each model for each week
  forecasts <-
    # get forecasts from local files
    # passing monday_dates as last_time_zero argument for load_covid_forecasts
    purrr::map_dfr(
      monday_dates,
      load_covid_forecasts,
      model_abbrs = model_abbrs,
      timezero_window_size = timezero_window_size,
      types = "quantile",
      targets = targets,
      submissions_root = submissions_root,
      keep_last = keep_last
    ) %>%
    # keep only required quantiles and locations
    dplyr::filter(
      format(quantile, digits = 3, nsmall = 3) %in%
        format(required_quantiles, digits = 3, nsmall = 3),
      location %in% locations
    ) %>%
    # put quantiles in columns
    tidyr::pivot_wider(names_from = quantile, values_from = value) %>%
    # create columns for horizon, forecast week end date of the forecast, and
    # target of the forecast
    dplyr::mutate(
      # the forecast_week_end_date variable is now mis-named; it represents the
      # date relative to which the horizons and targets are defined for the
      # purpose of building the ensemble.
      forecast_week_end_date =
        covidEnsembles::calc_forecast_week_end_date(timezero, target, return_type = "date"),
      horizon =
        covidEnsembles::calc_relative_horizon(forecast_week_end_date, target_end_date, target),
      target =
        covidEnsembles::calc_relative_target(forecast_week_end_date, target_end_date, target)
    ) %>%
    # keep only forecasts targeting dates after the forecast_week_end_date
    # and less than the specified horizon relative to the forecast week end date
    dplyr::filter(horizon >= 1, horizon <= UQ(horizon)) %>%
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

  # Add in fake rows for models that submitted point forecasts but not quantile
  # forecasts -- this is done so those models will appear in the model
  # eligibility metadata
  if (include_null_point_forecasts) {
    point_forecasts <-
      # get forecasts from local files
      purrr::map_dfr(
        tail(monday_dates, 1),
        load_covid_forecasts,
        model_abbrs = model_abbrs,
        timezero_window_size = timezero_window_size,
        types = "point",
        targets = targets,
        submissions_root = submissions_root,
        keep_last = keep_last
      ) %>%
      # create columns for horizon, forecast week end date of the forecast, and
      # target of the forecast
      dplyr::mutate(
        # the forecast_week_end_date variable is now mis-named; it represents the
        # date relative to which the horizons and targets are defined for the
        # purpose of building the ensemble.
        forecast_week_end_date =
          covidEnsembles::calc_forecast_week_end_date(timezero, target, return_type = "date"),
        horizon =
          covidEnsembles::calc_relative_horizon(forecast_week_end_date, target_end_date, target),
        target =
          covidEnsembles::calc_relative_target(forecast_week_end_date, target_end_date, target)
      ) %>%
      # keep only forecasts targeting dates after the forecast_week_end_date
      # and less than the specified horizon relative to the forecast week end date
      dplyr::filter(horizon > 0, horizon < UQ(horizon)) %>%
      # keep only required locations and models for which no quantile forecasts
      # were provided
      dplyr::filter(
        location %in% locations,
        !(model %in% forecasts$model)
      )

    forecasts <- dplyr::bind_rows(
      forecasts,
      point_forecasts %>%
        dplyr::mutate(
          quantile = "0.5",
          value = NA_real_
        )
    )
  }

  return(forecasts)
}

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
    "models" = model_abbrs,
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
#' @param forecast_date the forecast date for the analysis, typically a Monday
#' @param forecast_week_end_date date relative to week-ahead or day-ahead
#' targets are defined. For week ahead targets, a Saturday; for day ahead
#' targets, a Monday.
#' @param timezero_window_size The number of days back to go.  A window size of
#' 0 will retrieve only forecasts submitted on the `last_timezero` date.
#' @param window_size size of window
#' @param intercept logical specifying whether an intercept is included
#' @param combine_method character specifying the approach to model
#' combination: "equal", "convex", "positive", "unconstrained", "median",
#' or "convex_median".
#' The first four form a linear combination of quantiles across component
#' models with varying levels of restrictions on the combination coefficients.
#' "median" takes the median across models at each quantile level, and
#' "convex_median" uses a weighted median with convext constraints on weights
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
#' forecasts; either 'mean' for mean imputation or 'none' for no imputation
#' @param backend back end used for optimization.
#' @param project_url zoltar project url
#' @param required_quantiles numeric vector of quantiles component models are
#' required to have submitted
#' @param check_missingness_by_target if TRUE, record missingness for every
#' combination of model, location, forecast week, and target; if FALSE, record
#' missingness only for each model and location
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
  forecast_date,
  forecast_week_end_date,
  timezero_window_size = 1,
  window_size,
  data_as_of_date,
  intercept=FALSE,
  combine_method,
  quantile_groups,
  missingness,
  impute_method,
  backend,
  project_url = 'https://www.zoltardata.com/api/project/44/',
  required_quantiles,
  check_missingness_by_target,
  do_q10_check,
  do_nondecreasing_quantile_check,
  do_baseline_check,
  baseline_tol = 1.2,
  top_models = 0,
  manual_eligibility_adjust,
  return_eligibility = TRUE,
  return_all = TRUE
) {
  stop("build_covid_ensemble_from_zoltar is out of date and should not be used!")

  # Set up Zoltar
  zoltar_connection <- new_connection()
  zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

  # Get observed values ("truth" in Zoltar's parlance)
  observed_by_location_target_end_date <-
    get_observed_by_location_target_end_date(
      as_of = as.character(data_as_of_date),
      targets = targets,
      spatial_resolution = c('national', 'state')
    )

  # Dates specifying mondays when forecasts were submitted that are relevant to
  # this analysis: forecast_date and the previous window_size weeks
  monday_dates <- forecast_date +
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
      horizon = as.integer(substr(target, 1, regexpr(" ", target, fixed = TRUE) - 1)),
      forecast_week_end_date = calc_forecast_week_end_date(timezero)#,
#      target_end_date = calc_target_week_end_date(timezero, horizon)
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
    combine_method=combine_method,
    quantile_groups=quantile_groups,
    missingness=missingness,
    impute_method=impute_method,
    backend=backend,
    check_missingness_by_target = check_missingness_by_target,
    do_q10_check = do_q10_check,
    do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
    do_baseline_check = do_baseline_check,
    baseline_tol = baseline_tol,
    top_models = top_models,
    manual_eligibility_adjust = manual_eligibility_adjust,
    return_eligibility = return_eligibility,
    return_all = return_all)

  # return
  return(c(
    results,
    list(forecasts = forecasts)
  ))
}

