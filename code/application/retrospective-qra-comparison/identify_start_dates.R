library(gurobi)
library(quantgen)
library(covidData)
library(covidEnsembles)
library(tidyverse)
library(zeallot)
library(gridExtra)
library(yaml)

submissions_root <- "~/research/epi/covid/covid19-forecast-hub/data-processed/"

# List of candidate models for inclusion in ensemble
candidate_model_abbreviations_to_include <- get_candidate_models(
  submissions_root = submissions_root,
  include_designations = c("primary", "secondary"),
  include_COVIDhub_ensemble = FALSE,
  include_COVIDhub_baseline = TRUE)

# Drop hospitalizations ensemble from JHU APL
candidate_model_abbreviations_to_include <-
  candidate_model_abbreviations_to_include[
    !(candidate_model_abbreviations_to_include == "JHUAPL-SLPHospEns")
  ]

# candidate forecast dates at which we might want to start.
first_forecast_date <- lubridate::ymd("2020-03-02")
last_forecast_date <- lubridate::floor_date(Sys.Date(), unit = "week") + 1
num_forecast_weeks <-
  as.numeric(last_forecast_date - first_forecast_date) / 7 + 1

forecast_dates <- as.character(
  lubridate::ymd(first_forecast_date) +
    seq(from = 0, length = num_forecast_weeks) * 7)

start_dates <- tidyr::expand_grid(
  response_var = c("inc_case", "inc_death", "cum_death", "inc_hosp"),
  spatial_resolution = c("county", "state", "national"),
  start_date = lubridate::NA_Date_
)

for (response_var in c("inc_case", "inc_death", "cum_death", "inc_hosp")) {
  if (response_var %in% c("inc_death", "cum_death")) {
    required_quantiles <-
      c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
    temporal_resolution <- "wk"
    horizon <- 4L
    targets <- paste0(1:horizon, " wk ahead ", gsub("_", " ", response_var))
    spatial_resolutions <- c("state", "national")
  } else if (response_var == "inc_case") {
    required_quantiles <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
    temporal_resolution <- "wk"
    horizon <- 4L
    targets <- paste0(1:horizon, " wk ahead ", gsub("_", " ", response_var))
    spatial_resolutions <- c("county", "state", "national")
  } else if (response_var == "inc_hosp") {
    required_quantiles <-
      c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
    temporal_resolution <- "day"
    horizon <- 28L
    targets <-
      paste0(1:(horizon + 6), " day ahead ", gsub("_", " ", response_var))
    spatial_resolutions <- c("state", "national")
  }

  for (spatial_resolution in spatial_resolutions) {
    for (forecast_date in forecast_dates) {
      issue_date <- as.character(lubridate::ymd(forecast_date) - 1)
      if (response_var %in% c("inc_death", "cum_death")) {
        issue_date <- max(issue_date,
                          min(covidData::jhu_deaths_data$issue_date))
      } else if (response_var == "inc_case") {
        issue_date <- max(issue_date,
                          min(covidData::jhu_deaths_data$issue_date))
      } else if (response_var == "inc_hosp") {
        issue_date <- max(issue_date,
                          min(covidData::healthdata_hosp_data$issue_date))
      }

      observed_by_location_target_end_date <-
        get_observed_by_location_target_end_date(
          issue_date = issue_date,
          targets = targets,
          spatial_resolution = spatial_resolution
        )

      forecasts <- tryCatch(
          load_covid_forecasts_relative_horizon(
            monday_dates = lubridate::ymd(forecast_date),
            model_abbrs = candidate_model_abbreviations_to_include,
            timezero_window_size = 6,
            locations = unique(observed_by_location_target_end_date$location),
            targets = targets,
            horizon = horizon,
            required_quantiles = required_quantiles,
            submissions_root = submissions_root,
            include_null_point_forecasts = FALSE
          ),
          error = function(e) {
            NULL
          }
        )
      if (is.null(forecasts)) {
        next
      }

      if (length(unique(forecasts$model)) > 1) {
        start_dates$start_date[
          start_dates$response_var == response_var &
          start_dates$spatial_resolution == spatial_resolution] <- forecast_date
        break
      }
    }
  }
}

start_dates %>%
  dplyr::mutate(
    first_forecast_date = start_date + 10 * 7
  )

# Decision: for each target, start at the earliest forecast_date for which
# complete forecasts are available from at least two models for all spatial
# resolutions
# - for inc_case, start with forecast_date 2020-09-14
# - for inc_death and cum_death, start with forecast_date 2020-06-22
# - for inc_hosp, start with forecast_date 2020-11-16 since this is the first
#   date when data were reliably available.
