#' Read in covid forecasts from local files and fit one ensemble
#' 
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are "US" and "ECDC"
#' @param source string specifying where forecasts will be loaded from: either 
#' "local_hub_repo" or "zoltar"
#' @param hub_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository to be used when `source` is `local_hub_repo`
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
#' @param required_quantiles numeric vector of quantiles component models are
#' required to have submitted
#' @param check_missingness_by_target if TRUE, record missingness for every
#' combination of model, location, forecast week, and target; if FALSE, record
#' missingness only for each model and location
#' @param do_q10_check if TRUE, do q10 check
#' @param do_nondecreasing_quantile_check if TRUE, do nondecreasing quantile check
#' @param do_baseline_check
#' @param do_sd_check if TRUE, do CDC sd checks for hospitalization forecasts
#' @param sd_check_table_path where to save tables recording sd checks
#' @param sd_check_plot_path where to save plots of sd checks
#' @param baseline_tol
#' @param top_models
#' @param manual_eligibility_adjust
#' @param return_eligibility if TRUE, return model eligibility
#' @param return_all
#' @param partial_save_frequency passed to `qenspy` backend
#' @param partial_save_filename passed to `qenspy` backend
#'
#' @return data frame with ensemble forecasts by location
#'
#' @export
build_covid_ensemble <- function(
  hub = "US",
  source = "local_hub_repo",
  hub_repo_path,
  candidate_model_abbreviations_to_include,
  spatial_resolution,
  targets,
  forecast_date,
  forecast_week_end_date, 
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

  # Dates specifying the Monday from which this ensemble will predict 
  # and if window_size > 0, the Mondays corresponding to training periods
  monday_dates <- forecast_date +
    seq(from = -window_size, to = 0, by = 1) * 7    

  # map these Mondays to `last_forecast_date` of `load_latest_forecasts`
  forecasts <- purrr::map_dfr(
    monday_dates, 
  covidHubUtils::load_latest_forecasts,
    models = candidate_model_abbreviations_to_include,
    forecast_date_window_size = timezero_window_size,
    locations = unique(observed_by_location_target_end_date$location),
    targets = targets,
    types = c("point", "quantile"),
    source = source,
    hub_repo_path = hub_repo_path,
    as_of = NULL,
    hub = hub,
    ) %>% 
  dplyr::filter(quantile %in% required_quantiles)


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