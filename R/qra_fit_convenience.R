
#' Get a list of candidate models with specified model designations
#'
#' @param submissions_root path to the data-processed folder of the
#' covid19-forecast-hub repository
#' @param include_designations character vector of model designations to
#' include: "primary", "secondary", "proposed", and/or "other"
#' @param include_COVIDhub_ensemble logical: if TRUE, whether or not the
#' COVIDhub-ensemble model is included depends on whether it falls within one
#' of the specified \code{include_designations}; if FALSE, it will never be
#' included
#' @param include_COVIDhub_baseline logical: if TRUE, whether or not the
#' COVIDhub-baseline model is included depends on whether it falls within one
#' of the specified \code{include_designations}; if FALSE, it will never be
#' included
#'
#' @return character vector of model abbreviations
#'
#' @export
get_candidate_models <- function(
  submissions_root,
  include_designations = c("primary", "secondary", "proposed"),
  include_COVIDhub_ensemble = FALSE,
  include_COVIDhub_baseline = TRUE) {
  # validate include designations
  include_designations <- match.arg(
    include_designations,
    choices = c("primary", "secondary", "proposed", "other"),
    several.ok = TRUE
  )

  # List of directories within the submissions_root
  model_dirs <- list.dirs(submissions_root)

  # drop first result, which is the data-processed directory itself
  model_dirs <- model_dirs[-1]

  # Data frame with model abbreviation and designation for each model
  model_info <- purrr::map_dfr(
    model_dirs,
    function(model_dir) {
      metadata_path <- Sys.glob(paste0(model_dir, "/metadata*"))
      return(as.data.frame(
        yaml::read_yaml(metadata_path)[c("model_abbr", "team_model_designation")],
        stringsAsFactors = FALSE
      ))
    }
  )

  # filter to keep only requested designations
  candidate_models <- model_info %>%
    dplyr::filter(team_model_designation %in% include_designations) %>%
    dplyr::pull(model_abbr)

  # drop COVIDhub models if requested
  if(!include_COVIDhub_ensemble) {
    candidate_models <- candidate_models[
      candidate_models != "COVIDhub-ensemble"]
  }
  if(!include_COVIDhub_baseline) {
    candidate_models <- candidate_models[
      candidate_models != "COVIDhub-baseline"]
  }

  # return
  return(candidate_models)
}

#' Load multiple models' last forecasts from multiple weeks and 
#' adjust the targets for daily forecasts to be relative to the 
#' Monday on or after submission.
#' 
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are "US" and "ECDC"
#' @param source string specifying where forecasts will be loaded from: either 
#' "local_hub_repo" or "zoltar"
#' @param hub_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository to be used when `source` is `local_hub_repo`
#' @param monday_dates Date vector of Mondays that are submission deadlines
#' @param model_abbrs Character vector of model abbreviations
#' @param timezero_window_size The number of days back to go.  A window size of
#' 0 will retrieve only forecasts submitted on the `last_timezero` date.
#' @param locations character vector of locations; FIPS codes
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death')
#' @param max_horizon maximum horizon relative to forecast_week_end_date and 
#' analogous training reference dates for both training and prediction
#' @param required_quantiles numeric vector of quantiles component models are
#' required to have submitted
#'
#' @return data frame with ensemble forecasts by location
#'
#' @export
load_covid_forecasts_relative_horizon <- function(
  hub = "US",
  source = "local_hub_repo",
  hub_repo_path,
  monday_dates,
  model_abbrs,
  timezero_window_size,
  locations,
  targets,
  max_horizon,
  required_quantiles
) {
  # map monday_dates to `last_forecast_date` of `load_latest_forecasts`
  forecasts <- purrr::map_dfr(
    monday_dates, 
    covidHubUtils::load_latest_forecasts,
    models = model_abbrs,
    forecast_date_window_size = timezero_window_size,
    locations = locations,
    targets = targets,
    types = c("point", "quantile"),
    source = source,
    hub_repo_path = hub_repo_path,
    as_of = NULL,
    hub = hub,
    verbose = FALSE
    )  %>% 
  dplyr::mutate(
    target_unadjusted = paste(horizon, temporal_resolution, "ahead", target_variable),
    reference_date = covidEnsembles::calc_forecast_week_end_date(
      forecast_date, 
      target_unadjusted, 
      return_type = "date"
    ),
    relative_horizon = covidEnsembles::calc_relative_horizon(
      reference_date, 
      target_end_date, 
      target_unadjusted
    )
  ) %>% 
  # keep only forecasts targeting dates after their reference dates
  # and at relative horizons less than or equal to max_horizon
  dplyr::filter(
    relative_horizon <= max_horizon & relative_horizon > 0
  )

  # Add in fake rows for models that submitted point forecasts but not quantile
  # forecasts -- this is done so those models will appear in the model
  # eligibility metadata
  forecasts <- forecasts %>% dplyr::filter(
    quantile %in% required_quantiles | type == "point"
  ) %>% 
  # within each combination of model, forecast_date, location, and target_end_date,
  # if both a quantile and point forecast were provided, drop the point forecast; else, keep both.
  # if the forecaster provided only a point forecast, convert it to a missing median
  # this is done so that the forecast will appear in validation outputs
  dplyr::group_by(model, forecast_date, location, target_end_date) %>% 
  dplyr::filter(min_rank(match(type, c("quantile", "point")))==1) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    quantile = ifelse(type == "point", 0.5, quantile),
    value = ifelse(type == "point", NA_real_, value)
  )

  # Patch code to be eliminated as rest of covidEnsembles becomes
  # better aligned with covidHubUtils
  forecasts <- forecasts %>% dplyr::transmute(
    model = model,
    timezero = forecast_date,
    location = location,
    target_end_date = target_end_date,
    quantile = as.character(quantile),
    value = value,
    location_name = location_name,
    location_name_with_state = full_location_name, 
    # assumes fips_codes$location_name_with_state same as 
    # covidHubUtils::hub_locations other than "US" <-> "United States"
    abbreviation = abbreviation,
    # add in relative horizon info
    # to be renamed and/or eliminated in future development
    forecast_week_end_date = reference_date,
    horizon = relative_horizon,
    target = covidEnsembles::calc_relative_target(
      reference_date, 
      target_end_date, 
      target_unadjusted
    ),
  )

  return(forecasts)
}



#' Read in covid forecasts from local files and fit one ensemble
#' 
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are "US" and "ECDC";
#' passed to covidHubUtils::load_latest_forecasts
#' @param source string specifying where forecasts will be loaded from: either 
#' "local_hub_repo" or "zoltar";
#' passed to covidHubUtils::load_latest_forecasts
#' @param hub_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository to be used when `source` is `local_hub_repo`;
#' passed to covidHubUtils::load_latest_forecasts
#' @param candidate_model_abbreviations_to_include List of model abbreviations
#' for models that may be included in ensemble forecast
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death')
#' @param forecast_date the forecast date for the analysis, typically a Monday
#' @param forecast_week_end_date date relative to week-ahead or day-ahead
#' targets are defined. For week ahead targets, a Saturday; for day ahead
#' targets, a Monday.
#' @param max_horizon maximum horizon relative to forecast_week_end_date and 
#' analogous training reference dates for both training and prediction
#' @param timezero_window_size The number of days back to go.  A window size of
#' 0 will retrieve only forecasts submitted on the `last_timezero` date.
#' @param window_size size of window
#' @param data_as_of_date date for which observations should be current
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
#' @param noncross string specifying approach to handling quantile noncrossing:
#' one of "constrain" or "sort". "constrain" means estimation is done subject
#' to constraints ruling out quantile crossing.  "sort" means no such
#' constraints are imposed during estimation, but the resulting forecasts are
#' sorted.
#' @param missingness character specifying approach to handling missing
#' forecasts: 'by_location_group', 'rescale', or 'impute'
#' @param impute_method character string specifying method for imputing missing
#' forecasts; either 'mean' for mean imputation or 'none' for no imputation
#' @param backend back end used for optimization.
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
build_covid_ensemble <- function(
  hub,
  source,
  hub_repo_path,
  candidate_model_abbreviations_to_include,
  spatial_resolution,
  targets,
  forecast_date,
  forecast_week_end_date,
  max_horizon,
  timezero_window_size = 1,
  window_size,
  data_as_of_date,
  intercept = FALSE,
  combine_method,
  quantile_groups,
  noncross = "constrain",
  missingness,
  impute_method,
  backend,
  required_quantiles,
  check_missingness_by_target,
  do_q10_check,
  do_nondecreasing_quantile_check,
  do_baseline_check,
  do_sd_check,
  sd_check_table_path = NULL,
  sd_check_plot_path = NULL,
  baseline_tol = 1.2,
  top_models = 0,
  manual_eligibility_adjust,
  return_eligibility = TRUE,
  return_all = TRUE,
  partial_save_frequency,
  partial_save_filename
) {
  # Get observed values ("truth" in Zoltar's parlance)
  observed_by_location_target_end_date <-
    get_observed_by_location_target_end_date(
      as_of = as.character(data_as_of_date),
      targets = targets,
      spatial_resolution = spatial_resolution
    )

  # Dates specifying mondays when forecasts were submitted that are relevant to
  # this analysis: forecast_date and the previous window_size weeks
  monday_dates <- forecast_date +
    seq(from = -window_size, to = 0, by = 1) * 7

  forecasts <- load_covid_forecasts_relative_horizon(
    hub = hub,
    source = source,
    hub_repo_path = hub_repo_path,
    monday_dates = monday_dates,
    model_abbrs = candidate_model_abbreviations_to_include,
    timezero_window_size = timezero_window_size,
    locations = unique(observed_by_location_target_end_date$location),
    targets = targets,
    max_horizon = horizon,
    required_quantiles = required_quantiles
  )

  # obtain ensemble fit(s)
  results <- get_ensemble_fit_and_predictions(
    forecasts=forecasts,
    observed_by_location_target_end_date=observed_by_location_target_end_date,
    forecast_week_end_date=forecast_week_end_date,
    window_size=window_size,
    intercept=intercept,
    combine_method=combine_method,
    quantile_groups=quantile_groups,
    noncross = noncross,
    missingness=missingness,
    impute_method=impute_method,
    backend=backend,
    check_missingness_by_target = check_missingness_by_target,
    do_q10_check = do_q10_check,
    do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
    do_baseline_check = do_baseline_check,
    do_sd_check = do_sd_check,
    sd_check_table_path = sd_check_table_path,
    sd_check_plot_path = sd_check_plot_path,
    baseline_tol = baseline_tol,
    top_models = top_models,
    manual_eligibility_adjust = manual_eligibility_adjust,
    return_eligibility = return_eligibility,
    return_all = return_all,
    partial_save_frequency = partial_save_frequency,
    partial_save_filename = partial_save_filename)

  # return
  return(c(
    results,
    list(forecasts = forecasts)
  ))
}

#' Read in covid forecasts from local files and fit one ensemble
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
#' @param data_as_of_date date for which observations should be current
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
#' @param noncross string specifying approach to handling quantile noncrossing:
#' one of "constrain" or "sort". "constrain" means estimation is done subject
#' to constraints ruling out quantile crossing.  "sort" means no such
#' constraints are imposed during estimation, but the resulting forecasts are
#' sorted.
#' @param missingness character specifying approach to handling missing
#' forecasts: 'by_location_group', 'rescale', or 'impute'
#' @param impute_method character string specifying method for imputing missing
#' forecasts; either 'mean' for mean imputation or 'none' for no imputation
#' @param backend back end used for optimization.
#' @param submissions_root path to the data-processed folder of the
#' covid19-forecast-hub repository
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
build_covid_ensemble_from_local_files <- function(
  candidate_model_abbreviations_to_include,
  spatial_resolution,
  targets,
  forecast_date,
  forecast_week_end_date,
  horizon,
  timezero_window_size = 1,
  window_size,
  data_as_of_date,
  intercept=FALSE,
  combine_method,
  quantile_groups,
  noncross = "constrain",
  missingness,
  impute_method,
  backend,
  submissions_root,
  required_quantiles,
  check_missingness_by_target,
  do_q10_check,
  do_nondecreasing_quantile_check,
  do_baseline_check,
  do_sd_check,
  sd_check_table_path = NULL,
  sd_check_plot_path = NULL,
  baseline_tol = 1.2,
  top_models = 0,
  manual_eligibility_adjust,
  return_eligibility = TRUE,
  return_all = TRUE,
  partial_save_frequency,
  partial_save_filename
) {
  # Get observed values ("truth" in Zoltar's parlance)
  observed_by_location_target_end_date <-
    get_observed_by_location_target_end_date(
      as_of = as.character(data_as_of_date),
      targets = targets,
      spatial_resolution = spatial_resolution
    )

  # Dates specifying mondays when forecasts were submitted that are relevant to
  # this analysis: forecast_date and the previous window_size weeks
  monday_dates <- forecast_date +
    seq(from = -window_size, to = 0, by = 1) * 7

  forecasts <- load_covid_forecasts_relative_horizon_old(
    monday_dates = monday_dates,
    model_abbrs = candidate_model_abbreviations_to_include,
    timezero_window_size = timezero_window_size,
    locations = unique(observed_by_location_target_end_date$location),
    targets = targets,
    horizon = horizon,
    required_quantiles = required_quantiles,
    submissions_root =submissions_root,
    include_null_point_forecasts = TRUE
  )

  # obtain ensemble fit(s)
  results <- get_ensemble_fit_and_predictions(
    forecasts=forecasts,
    observed_by_location_target_end_date=observed_by_location_target_end_date,
    forecast_week_end_date=forecast_week_end_date,
    window_size=window_size,
    intercept=intercept,
    combine_method=combine_method,
    quantile_groups=quantile_groups,
    noncross = noncross,
    missingness=missingness,
    impute_method=impute_method,
    backend=backend,
    check_missingness_by_target = check_missingness_by_target,
    do_q10_check = do_q10_check,
    do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
    do_baseline_check = do_baseline_check,
    do_sd_check = do_sd_check,
    sd_check_table_path = sd_check_table_path,
    sd_check_plot_path = sd_check_plot_path,
    baseline_tol = baseline_tol,
    top_models = top_models,
    manual_eligibility_adjust = manual_eligibility_adjust,
    return_eligibility = return_eligibility,
    return_all = return_all,
    partial_save_frequency = partial_save_frequency,
    partial_save_filename = partial_save_filename)

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
#' @param noncross string specifying approach to handling quantile noncrossing:
#' one of "constrain" or "sort". "constrain" means estimation is done subject
#' to constraints ruling out quantile crossing.  "sort" means no such
#' constraints are imposed during estimation, but the resulting forecasts are
#' sorted.
#' @param missingness character specifying approach to handling missing
#' forecasts: 'by_location_group', 'rescale', and 'impute'
#' @param impute_method character string specifying method for imputing missing
#' forecasts; either 'mean' for mean imputation or 'none' for no imputation
#' @param backend back end used for optimization.
#' @param check_missingness_by_target if TRUE, record missingness for every
#' combination of model, location, forecast week, and target; if FALSE, record
#' missingness only for each model and location
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
  combine_method = c('ew', 'convex', 'positive', 'unconstrained', 'median', 'convex_median', 'rel_wis_weighted_median'),
  quantile_groups = NULL,
  noncross = "constrain",
  missingness = c('by_location_group', 'rescale', 'mean_impute'),
  impute_method = 'mean',
  backend = 'quantmod',
  check_missingness_by_target = FALSE,
  do_q10_check,
  do_nondecreasing_quantile_check,
  do_baseline_check,
  do_sd_check,
  sd_check_table_path = NULL,
  sd_check_plot_path = NULL,  
  baseline_tol = 1.2,
  top_models=0,
  manual_eligibility_adjust,
  return_eligibility = TRUE,
  return_all = FALSE,
  partial_save_frequency,
  partial_save_filename) {
  if(missing(forecasts) ||
     missing(forecast_week_end_date) ||
     missing(window_size)) {
    stop("The arguments `forecasts`, `forecast_week_end_date`, and `window_size` must all be provided.")
  }

  combine_method <- match.arg(
    combine_method,
    choices = c("ew", "convex", "positive", "unconstrained", "median", "convex_median", "rel_wis_weighted_median"),
    several.ok = FALSE)

  if(missingness == "by_location_group") {
    results <- get_by_location_group_ensemble_fits_and_predictions(
      forecasts = forecasts,
      observed_by_location_target_end_date =
        observed_by_location_target_end_date,
      forecast_week_end_date = forecast_week_end_date,
      window_size = window_size,
      intercept = intercept,
      combine_method = combine_method,
      quantile_groups = quantile_groups,
      noncross = noncross,
      backend = backend,
      do_q10_check = do_q10_check,
      do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
      do_baseline_check = do_baseline_check,
      do_sd_check = do_sd_check,
      sd_check_table_path = sd_check_table_path,
      sd_check_plot_path = sd_check_plot_path,
      baseline_tol = baseline_tol,
      manual_eligibility_adjust = manual_eligibility_adjust,
      return_eligibility = return_eligibility,
      return_all = return_all)
  } else if(missingness == "impute") {
    results <- get_imputed_ensemble_fits_and_predictions(
      forecasts = forecasts,
      observed_by_location_target_end_date =
        observed_by_location_target_end_date,
      forecast_week_end_date = forecast_week_end_date,
      window_size = window_size,
      intercept = intercept,
      combine_method = combine_method,
      quantile_groups = quantile_groups,
      noncross = noncross,
      impute_method = impute_method,
      backend = backend,
      check_missingness_by_target = check_missingness_by_target,
      do_q10_check = do_q10_check,
      do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
      do_baseline_check = do_baseline_check,
      do_sd_check = do_sd_check,
      sd_check_table_path = sd_check_table_path,
      sd_check_plot_path = sd_check_plot_path, 
      baseline_tol = baseline_tol,
      top_models=top_models,
      manual_eligibility_adjust = manual_eligibility_adjust,
      return_eligibility = return_eligibility,
      return_all = return_all,
      partial_save_frequency = partial_save_frequency,
      partial_save_filename = partial_save_filename)
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
#' @param noncross string specifying approach to handling quantile noncrossing:
#' one of "constrain" or "sort". "constrain" means estimation is done subject
#' to constraints ruling out quantile crossing.  "sort" means no such
#' constraints are imposed during estimation, but the resulting forecasts are
#' sorted.
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
get_by_location_group_ensemble_fits_and_predictions <- function(
  forecasts,
  observed_by_location_target_end_date,
  forecast_week_end_date,
  window_size,
  intercept = FALSE,
  combine_method = c("ew", "convex", "positive", "unconstrained", "median", "convex_median", "rel_wis_weighted_median"),
  quantile_groups = NULL,
  noncross = "constrain",
  backend = "quantmod",
  do_q10_check,
  do_nondecreasing_quantile_check,
  do_baseline_check,
  do_sd_check,
  sd_check_table_path = NULL,
  sd_check_plot_path = NULL,   
  baseline_tol = 1.2,
  manual_eligibility_adjust,
  return_all=FALSE,
  return_eligibility = TRUE) {
  if(missing(forecasts) ||
     missing(forecast_week_end_date) ||
     missing(window_size)) {
    stop("The arguments `forecasts`, `forecast_week_end_date`, and `window_size` must all be provided.")
  }

  combine_method <- match.arg(
    combine_method,
    choices = c("ew", "convex", "positive", "unconstrained", "median", "convex_median"),
    several.ok = TRUE)

  # obtain model eligibility by location
  # since we have not yet filtered by horizon/target, eligibility is based on
  # all four targets 1 - 4 wk ahead cum deaths
  forecast_matrix <- covidEnsembles::new_QuantileForecastMatrix_from_df(
    forecast_df = forecasts,
    model_col = "model",
    id_cols = c("location", "forecast_week_end_date", "target"),
    quantile_name_col = "quantile",
    quantile_value_col = "value"
  )

  forecast_base_targets <- substr(
    forecasts$target,
    regexpr(" ", forecasts$target) + 1,
    nchar(forecasts$target)
  )
  model_eligibility <- covidEnsembles::calc_model_eligibility_for_ensemble(
    qfm = forecast_matrix,
    observed_by_location_target_end_date =
      observed_by_location_target_end_date %>%
        dplyr::filter(base_target %in% forecast_base_targets),
    do_q10_check = do_q10_check,
    do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
    do_baseline_check = do_baseline_check,
    do_sd_check = do_sd_check,
    sd_check_table_path = sd_check_table_path,
    sd_check_plot_path = sd_check_plot_path,
    baseline_tol = baseline_tol,
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
        manual_eligibility_adjust$message[i]
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

      new_QuantileForecastMatrix_from_df(
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

      new_QuantileForecastMatrix_from_df(
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
            lubridate::ymd(forecast_week_end_date) +
              as.numeric(substr(target, 1, regexpr(" ", target, fixed = TRUE) - 1)) *
                ifelse(grepl("day", target), 1, 7)
          ),
          base_target = substr(target, regexpr(" ", target, fixed = TRUE) + 1, nchar(target))
        ) %>%
        dplyr::left_join(
          observed_by_location_target_end_date,
          by = c('location', 'target_end_date', 'base_target')) %>%
        dplyr::pull(observed)
    })

  # fit ensembles and obtain predictions per group
  if(combine_method == 'ew') {
    location_groups$qra_fit <- purrr::map(
      location_groups$qfm_train,
      estimate_qra,
      combine_method = 'ew')
  } else if(combine_method == 'median') {
    location_groups$qra_fit <- purrr::map(
      location_groups$qfm_train,
      new_median_qra_fit)
  } else {
    location_groups[['qra_fit']] <- purrr::pmap(
      location_groups %>% select(qfm_train, y_train, qfm_test),
      function(qfm_train, y_train, qfm_test) {
        estimate_qra(
          qfm_train = qfm_train,
          y_train = y_train,
          qfm_test = qfm_test,
          intercept = intercept,
          combine_method = combine_method,
          quantile_groups = quantile_groups,
          noncross = noncross,
          backend = backend)
      })
  }

  # obtain predictions
  location_groups[['qra_forecast']] <- purrr::pmap(
    location_groups %>% dplyr::select(qra_fit, qfm_test),
    function(qra_fit, qfm_test) {
      predict(qra_fit, qfm_test, sort_quantiles = (noncross == "sort")) %>%
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
#' @param impute_method character string specifying method for imputing missing
#' forecasts; either 'mean' for mean imputation or 'none' for no imputation
#' @param weight_transfer_per_group logical indicating whether to compute weight
#' transfer matrices for every group defined by `weight_transfer_group_factors'
#' @param weight_transfer_group_factors string vector of these factors with only
#' "locations" as default.  Ignored if weight_transfer_per_group is FALSE
#' @param imputed_qfm_only if TRUE, return only imputed QuantileForecastMatrix
#' 
#' @return if `imputed_qfm_only` is TRUE, 'qfm_imputed', the input 
#' QuantileForecastMatrix object with missing values imputed
#' 
#' otherwise a list of two items:
#' 1. 'qfm_imputed'
#' 2. if `weight_transfer_per_group` is FALSE, 'weight_transfer', a square matrix 
#' of dimension equal to the number of unique models in qfm.  Entry [i, j] is the 
#' proportion of imputed observations for model j that are attributable to model i.
#'    if `weight_transfer_per_group` is TRUE, a data 
#' frame having a column for each factor and a list-column of the corresponding 
#' weight transfer matrices whose entries give within-group proportions.
#'
#' @export
impute_missing_per_quantile <- function(
  qfm, 
  impute_method = 'mean',
  weight_transfer_per_group = FALSE,
  weight_transfer_group_factors = 'location',
  imputed_qfm_only = FALSE) {
  
  if (impute_method == 'none') {
    qfm_imputed <- qfm
    weight_transfer <- NULL
  } else if (impute_method == 'mean') {
    col_index <- attr(qfm, 'col_index')
    model_col <- attr(qfm, 'model_col')
    quantile_name_col <- attr(qfm, 'quantile_name_col')
    quantile_levels <- col_index[[quantile_name_col]]
    unique_quantile_levels <- unique(quantile_levels)
    num_models <- length(unique(col_index[[model_col]]))

    X_na <- is.na(qfm)

    missingness_patterns <- X_na %>%
      as.data.frame() %>%
      mutate(row_num = dplyr::row_number()) %>%
      dplyr::group_by(across(seq_len(ncol(.) - 1))) %>%
      dplyr::summarise(row_inds = list(row_num), .groups = "drop")

    qfm_imputed <- qfm
    qfm_imputed[is.na(qfm_imputed)] <- 0.0

    weight_transfer <- matrix(0, nrow = num_models, ncol = num_models)

    if (weight_transfer_per_group) {
      row_groups <- attr(qfm, 'row_index') %>% 
        mutate(row_num = dplyr::row_number()) %>%
        dplyr::group_by(!!!syms(weight_transfer_group_factors)) %>% 
        dplyr::summarise(row_inds_per_group = list(row_num), .groups = "drop") %>% 
        mutate(weight_transfer = list(weight_transfer))
    }

    for(i in seq_len(nrow(missingness_patterns))) {
      row_inds <- missingness_patterns$row_inds[[i]]

      # intialize as identity
      impute_mat <- diag(num_models)

      col_inds <- which(quantile_levels == unique_quantile_levels[1])
      temp <- !is.na(unclass(qfm)[row_inds[1], col_inds])
      temp <- temp / sum(temp)

      # form transfer matrix based on first quantile for first 
      # row (location-date-target) with missingness pattern
      for(j_ind in seq_along(col_inds)) {
        j <- col_inds[j_ind]
        # replace e_i with 'average of non-missing' column
        if(is.na(qfm[row_inds[1], j])) {
          impute_mat[, j_ind] <- temp
        }
      }

      # use this matrix to impute all quantiles in all rows with i'th miss pattern
      for(quantile_level in unique_quantile_levels) {
        col_inds <- which(quantile_levels == quantile_level)
        qfm_imputed[row_inds, col_inds] <-
          qfm_imputed[row_inds, col_inds, drop = FALSE] %*% impute_mat
      }

      weight_transfer <- weight_transfer + length(row_inds) * impute_mat
      if (weight_transfer_per_group) {
        row_groups <- row_groups %>% mutate(
          weight_transfer = purrr::map2(
            weight_transfer, row_inds_per_group,
            ~ .x + length(intersect(row_inds, .y)) * impute_mat 
            )
          )
      }
    }

    weight_transfer <- if (weight_transfer_per_group) {
      row_groups <- row_groups %>% mutate(
        weight_transfer = purrr::map2(
          weight_transfer, row_inds_per_group,
          ~ .x / length(.y)
        )
      )
    } else {
      tibble(weight_transfer = list(weight_transfer / nrow(qfm)))
    }
  } else {
    # impute method is neither 'mean' nor 'none'
    stop("Invalid impute_method in impute_missing_per_quantile: must be either 'mean' or 'none'")
  }

  if (imputed_qfm_only) {
    return(qfm_imputed)
  } else {
    return(list(
      qfm_imputed = qfm_imputed,
      weight_transfer = weight_transfer
      )
    )
  }
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
#' @param noncross string specifying approach to handling quantile noncrossing:
#' one of "constrain" or "sort". "constrain" means estimation is done subject
#' to constraints ruling out quantile crossing.  "sort" means no such
#' constraints are imposed during estimation, but the resulting forecasts are
#' sorted.
#' @param impute_method character string specifying method for imputing missing
#' forecasts; either 'mean' for mean imputation or 'none' for no imputation
#' @param weight_transfer_per_group 
#' @param weight_transfer_group_factors 
#' @param backend back end used for optimization.
#' @param check_missingness_by_target if TRUE, record missingness for every
#' combination of model, location, forecast week, and target; if FALSE, record
#' missingness only for each model and location
#' @param do_q10_check if TRUE, do q10 check
#' @param do_nondecreasing_quantile_check if TRUE, do nondecreasing quantile check
#' @param do_baseline_check if TRUE, do baseline quantile check
#' @param do_sd_check if TRUE, do sd quantile check (for hospitalization forecasts)
#' @param sd_check_table_path where to save hospitalization sd check table results
#' @param sd_check_plot_path where to save hospitalization sd check plot results
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
  combine_method = c('ew', 'median', 'convex', 'positive', 'unconstrained', 'convex_median', 'rel_wis_weighted_median'),
  quantile_groups = NULL,
  noncross = "constrain",
  impute_method = 'mean',
  weight_transfer_per_group = FALSE,
  weight_transfer_group_factors = "location",
  backend = 'quantmod',
  check_missingness_by_target = FALSE,
  do_q10_check,
  do_nondecreasing_quantile_check,
  do_baseline_check,
  do_sd_check,
  sd_check_table_path = NULL,
  sd_check_plot_path = NULL,
  baseline_tol = 1.2,
  top_models=0,
  manual_eligibility_adjust,
  return_all=FALSE,
  return_eligibility = TRUE,
  partial_save_frequency,
  partial_save_filename) {
  if (missing(forecasts) ||
     missing(forecast_week_end_date) ||
     missing(window_size)) {
    stop("The arguments `forecasts`, `forecast_week_end_date`, and `window_size` must all be provided.")
  }

  combine_method <- match.arg(
    combine_method,
    choices = c('ew', 'median', 'convex', 'positive', 'unconstrained', 'convex_median', 'rel_wis_weighted_median'),
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

  # consider refactoring to handle similar to covidHubUtils
  # (this could be unit tested)
  forecast_base_targets <- substr(
    forecasts$target,
    regexpr(' ', forecasts$target) + 1,
    nchar(forecasts$target)
  )
  model_eligibility <- covidEnsembles::calc_model_eligibility_for_ensemble(
    qfm = forecast_matrix,
    observed_by_location_target_end_date =
      observed_by_location_target_end_date %>%
        dplyr::filter(base_target %in% forecast_base_targets),
    missingness_by_target = check_missingness_by_target,
    do_q10_check = do_q10_check,
    do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
    do_baseline_check = do_baseline_check,
    do_sd_check = do_sd_check,
    sd_check_table_path = sd_check_table_path,
    sd_check_plot_path = sd_check_plot_path,
    baseline_tol = baseline_tol,
    window_size = window_size,
    decrease_tol = 0.0
  )

  # insert manual adjustments into model eligibility results
  # this code is different (and better) in get_by_location_... function above
  # can we move this into calc_model_eligibility_for_ensemble?  and unit test it?
  if (length(manual_eligibility_adjust) > 0) {
    for (i in seq_len(nrow(manual_eligibility_adjust))) {
      el_inds <- which(
        model_eligibility$model == manual_eligibility_adjust$model[i] &
          model_eligibility$location == manual_eligibility_adjust$location[i]
      )
      model_eligibility$overall_eligibility[el_inds] <-
        'Visual misalignment of predictive quantiles with JHU reference data.'
    }
  }

  # this code should go in a separate function that is called and needs to be
  # unit tested
  # intention: remove models that fail to submit all forecasts, at some level
  # of granularity specified by check_missingness_by_target
  #  - check_missingness_by_target TRUE: drop a forecast for a combination of
  #    model, location, forecast_date, target (horizon) if not all quantiles
  #    provided
  #  - check_missingness_by_target FALSE: drop a forecast for a combination of
  #    model, location if not all combinations of forecast date, target
  #    (horizon) and quantile are provided
  # keep only models that are eligible for inclusion in at least one location,
  # or one combination of location, forecast week, and target if
  # check_missingness_by_target is TRUE
  if (check_missingness_by_target) {
    # convert model eligibility to wide format logical with human readable names
    wide_model_eligibility <- model_eligibility %>%
      dplyr::transmute(
        model = model,
        location = location,
        forecast_week_end_date = forecast_week_end_date,
        target = target,
        eligibility = (overall_eligibility == "eligible"))

    # keep only model-location-targets that are eligible
    # here this is done by filtering the original forecasts data frame and
    # recreating the QuantileForecastMatrix
    forecasts <- forecasts %>%
      dplyr::left_join(wide_model_eligibility,
        by = c("model", "location", "forecast_week_end_date", "target")) %>%
      dplyr::filter(eligibility) %>%
      dplyr::select(-eligibility)
    
    forecast_matrix <- covidEnsembles::new_QuantileForecastMatrix_from_df(
      forecast_df = forecasts,
      model_col = "model",
      id_cols = c("location", "forecast_week_end_date", "target"),
      quantile_name_col = "quantile",
      quantile_value_col = "value"
    )
  } else {
    # convert model eligibility to wide format logical with human readable names
    wide_model_eligibility <- model_eligibility %>%
      dplyr::transmute(
        model = model,
        location = location,
        eligibility = (overall_eligibility == "eligible")) %>%
      tidyr::pivot_wider(names_from = "model", values_from = "eligibility")

    # keep only models that are eligible
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
  }

  # (in new function)
  # drop rows with no eligible models
  rows_to_keep <- apply(forecast_matrix, 1, function(qfm_row) any(!is.na(qfm_row))) %>%
    which()

  # (in new function)
  if(length(rows_to_keep) != nrow(forecast_matrix)) {
#    dropped_rows <- forecast_matrix[-rows_to_keep, ]
    forecast_matrix <- forecast_matrix[rows_to_keep, ]
  }

  # refactor train/test split into its own unit tested function.
  # get train/test inds
  # train:
  #  - if window_size >= 1, training set comprises only forecasts where
  # target_end_date <= forecast_week_end_date
  #  - else if window_size == 0, just keep horizon 1 for train set
  col_index <- attr(forecast_matrix, 'col_index')
  row_index <- attr(forecast_matrix, 'row_index')
  if (window_size >= 1) {
    # this should call a function that's tested
    target_end_date <- row_index %>%
      dplyr::mutate(
        target_end_date = as.character(
          lubridate::ymd(forecast_week_end_date) +
            as.numeric(substr(target, 1, regexpr(" ", target, fixed = TRUE) - 1)) *
              ifelse(grepl("day", target), 1, 7)
        ),
      ) %>%
      pull(target_end_date)

    train_row_inds <- which(target_end_date <= forecast_week_end_date)
    test_row_inds <- which(row_index[['forecast_week_end_date']] == forecast_week_end_date)

    # training set and test set QuantileForecastMatrix
    qfm_train <- forecast_matrix[train_row_inds, ]
    qfm_test <- forecast_matrix[test_row_inds, ]

    # drop combinations of model and location that don't
    # appear in the training set from the test set
    # essentially, this requires at least one previous submission for a given
    # location to use a model for test set predictions in that location
    train_row_index <- attr(qfm_train, 'row_index')
    test_row_index <- attr(qfm_test, 'row_index')
    all_locations <- unique(row_index$location)
    train_locations <- unique(train_row_index$location)
    locations_to_check <- all_locations[all_locations %in% train_locations]
    for (model in unique(col_index$model)) {
      model_cols <- which(col_index$model == model)

      # identify locations for which this model has no submissions within the training window
      missing_locations <- all_locations[!(all_locations %in% train_locations)]
      missing_locations <- c(
        missing_locations,
        purrr::map(
          locations_to_check,
          function(loc) {
            train_row_inds <- which(train_row_index$location == loc)
            if (all(is.na(qfm_train[train_row_inds, model_cols]))) {
              return(loc)
            } else {
              return(NULL)
            }
          }
        ) %>% unlist()
      )

      # if any missing locations were identified, set the corresponding model
      # forecasts to NA in the test set.
      if (length(missing_locations) > 0) {
        test_row_inds <- which(test_row_index$location %in% missing_locations)
        if (length(test_row_inds) > 0) {
          qfm_test[test_row_inds, model_cols] <- NA_real_
        }
      }
    }

    # drop models that don't appear in the test set from the training set
    # we don't give weight to models that didn't make any predictions this week
    # even though we checked for missingness above, this is necessary here in
    # case we dropped locations in the immediately preceeding check.
    models_to_drop <- NULL
    for (model in unique(col_index$model)) {
      model_cols <- which(col_index$model == model)
      if (all(is.na(qfm_test[, model_cols]))) {
        models_to_drop <- c(models_to_drop, model)
      }
    }
    all_models <- unique(col_index$model)
    models_to_keep <- all_models[!(all_models %in% models_to_drop)]
    cols_to_keep <- which(col_index$model %in% models_to_keep)
    qfm_train <- qfm_train[, cols_to_keep]
    qfm_test <- qfm_test[, cols_to_keep]
  } else {
    train_row_inds <- which(row_index[['forecast_week_end_date']] == forecast_week_end_date)
    test_row_inds <- which(substr(row_index$target, 1, 4) == '1 wk')

    # training set and test set QuantileForecastMatrix
    qfm_train <- forecast_matrix[train_row_inds, ]
    qfm_test <- forecast_matrix[test_row_inds, ]
  }

  # impute missing values

  if (!weight_transfer_per_group) weight_transfer_group_factors <- NULL

  c(imputed_qfm_train, weight_transfer) %<-% impute_missing_per_quantile(
    qfm = qfm_train,
    impute_method = impute_method,
    weight_transfer_per_group = weight_transfer_per_group,
    weight_transfer_group_factors = weight_transfer_group_factors)
  c(imputed_qfm_test, weight_transfer_test) %<-% impute_missing_per_quantile(
    qfm = qfm_test,
    impute_method = impute_method,
    weight_transfer_per_group = weight_transfer_per_group,
    weight_transfer_group_factors = weight_transfer_group_factors)

  # observed responses to date
  y_train <- attr(qfm_train, 'row_index') %>%
    dplyr::mutate(
      target_end_date = as.character(
        lubridate::ymd(forecast_week_end_date) +
          as.numeric(substr(target, 1, regexpr(" ", target, fixed = TRUE) - 1)) *
            ifelse(grepl("day", target), 1, 7)
      ),
      base_target = substr(target, regexpr(" ", target, fixed = TRUE) + 1, nchar(target))
    ) %>%
    dplyr::left_join(
      observed_by_location_target_end_date,
      by = c('location', 'target_end_date', 'base_target')
    ) %>%
    dplyr::pull(observed)
  
  # Subset to training set observations for which a response has been observed
  non_missing_inds <- which(!is.na(y_train))
  y_train <- y_train[non_missing_inds]
  imputed_qfm_train <- imputed_qfm_train[non_missing_inds, ]

  # if requested, subset to models with best individual performance
  if (top_models > 0) {
    rel_wis <- calc_relative_wis(y_train, imputed_qfm_train)
    top_models <- min(top_models, nrow(rel_wis))
    models_to_keep <- rel_wis$model[seq_len(top_models)]
    col_index <- attr(imputed_qfm_train, 'col_index')
    cols_to_keep <- which(col_index$model %in% models_to_keep)
    imputed_qfm_train <- imputed_qfm_train[, cols_to_keep]
    imputed_qfm_test <- imputed_qfm_test[, cols_to_keep]

    # drop rows with no eligible models (maybe none the top models we selected had submissions for some location)
    rows_to_keep <- apply(imputed_qfm_train, 1, function(qfm_row) any(!is.na(qfm_row))) %>%
      which()
    if (length(rows_to_keep) != nrow(imputed_qfm_train)) {
      imputed_qfm_train <- imputed_qfm_train[rows_to_keep, ]
      y_train <- y_train[rows_to_keep]
    }

    rows_to_keep <- apply(imputed_qfm_test, 1, function(qfm_row) any(!is.na(qfm_row))) %>%
      which()
    if (length(rows_to_keep) != nrow(imputed_qfm_test)) {
      imputed_qfm_test <- imputed_qfm_test[rows_to_keep, ]
    }
  }

  # fit ensembles and obtain predictions per group
  if (combine_method == 'ew') {
    # no y_train given - no training is done for equal weights
    qra_fit <- estimate_qra(
      qfm_train = imputed_qfm_train,
      combine_method = 'ew')
  } else if(combine_method == 'median') {
    qra_fit <- new_median_qra_fit(imputed_qfm_train)
  } else {
    qra_fit <- estimate_qra(
      qfm_train = imputed_qfm_train,
      y_train = y_train,
      qfm_test = imputed_qfm_test,
      intercept = intercept,
      combine_method = combine_method,
      quantile_groups = quantile_groups,
      noncross = noncross,
      backend = backend,
      partial_save_frequency = partial_save_frequency,
      partial_save_filename = partial_save_filename)
  }
  
  orig_qra_fit <- qra_fit

  # do weight transfer among models
  # save original weights for retrospective exploration

  # No need to do redistribution of estimated weights for an equally weighted model --
  # we just apply equal weights to all models that made test set predictions
  if (combine_method != 'ew' && impute_method != 'none') {
    if(nrow(qra_fit$coefficients) == nrow(weight_transfer)) {
      # single weight per model
      betas <- weight_transfer %>% 
      dplyr::mutate(
        betas = purrr::map(
          weight_transfer,
          ~ . %*% matrix(qra_fit$coefficients$beta)
        )) %>% 
      dplyr::select(!!!syms(weight_transfer_group_factors), betas)
    } else {
      # weight per quantile; adjust by iterating through quantile levels
      qs <- qra_fit$coefficients[[attr(qfm_train, 'quantile_name_col')]]
      betas <- weight_transfer %>% 
      dplyr::mutate(
        betas = purrr::map(
          weight_transfer,
          function(wt) {
            betas <- matrix(qra_fit$coefficients$beta)
            for (q in unique(qs)) {
              betas[which(qs == q)] <- wt %*% betas[which(qs == q)]
            }
            return(betas)
          }
        )) %>% 
      dplyr::select(!!!syms(weight_transfer_group_factors), betas)
    }
  }

  # obtain predictions
  if (combine_method %in% c('ew', 'median') || !weight_transfer_per_group) {
    qra_forecast <- predict(
      qra_fit,
      imputed_qfm_test,
      sort_quantiles = (noncross == "sort")) %>%
    as.data.frame()
  } else {
    qra_forecast <- weight_transfer_test %>% 
      dplyr::left_join(betas, by = weight_transfer_group_factors) %>% 
      dplyr::mutate(
        imputed_qfm_test_per_group = purrr::map(
          row_inds_per_group,
          ~ imputed_qfm_test[.,])) %>% 
      dplyr::mutate(
        forecasts = purrr::map2(
          betas, imputed_qfm_test_per_group,
          function (betas, imputed_qfm_test_per_group) {
            qra_fit$coefficients$beta <- as.vector(betas)
            return(
              predict(qra_fit, imputed_qfm_test_per_group,
                sort_quantiles = (noncross == "sort")
                ) %>% as.data.frame()
            )
          }
        )
      ) %>% dplyr::select(forecasts) %>% 
      purrr::map_dfr(bind_rows)
    }

  # return
  if(return_all) {
    result <- list(
      model_eligibility = model_eligibility,
      wide_model_eligibility = wide_model_eligibility,
      location_groups = as_tibble(list(
        locations = list(unique(attr(qfm_train, 'row_index')[['location']])),
        qfm_train = list(qfm_train),
        qfm_test = list(qfm_test),
        y_train = list(y_train),
        imputed_qfm_train = list(imputed_qfm_train),
        imputed_qfm_test = list(imputed_qfm_test),
        orig_qra_fit = list(orig_qra_fit),
        qra_fit = list(qra_fit),
        qra_forecast = list(qra_forecast)
      )),
      weight_transfer = weight_transfer
    )
  } else {
    stop('unsupported option for deprecated parameter return_all')
  }

  return(result)
}
