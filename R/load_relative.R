#' Load multiple models' last forecasts from multiple weeks and 
#' adjust the targets for daily forecasts to be relative to the 
#' Monday on or after submission.
#' 
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are "US" and "ECDC"
#' @param source string specifying where forecasts will be loaded from: either 
#' "local_hub_repo" or "zoltar"
#' @param monday_dates Date vector of Mondays that are submission deadlines
#' @param hub_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository to be used when `source` is `local_hub_repo`
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
load_covid_forecasts_relative_horizon_new <- function(
  hub = "US",
  source = "local_hub_repo",
  monday_dates,
  hub_repo_path,
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

