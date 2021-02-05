# load packages
library(covidData)
library(covidHubUtils)
library(covidEnsembles)
library(tidyverse)
library(gridExtra)
library(knitr)
library(here)

setwd(here())

knitr::opts_chunk$set(echo = FALSE, cache.lazy = FALSE)
options(width = 200)

# load data

# dates for "truth" data used to compute scores and used in plots
jhu_issue_date <- max(covidData::jhu_deaths_data$issue_date)
healthdata_issue_date <- max(covidData::healthdata_hosp_data$issue_date)

# load data
observed_deaths <-
  covidData::load_jhu_data(
    issue_date = jhu_issue_date,
    spatial_resolution = c("state", "national"),
    temporal_resolution = "weekly",
    measure = "deaths") %>%
  tidyr::pivot_longer(
    cols = c("inc", "cum"),
    names_to = "base_target",
    values_to = "observed"
  ) %>%
  dplyr::transmute(
    location = location,
    base_target = paste0("wk ahead ", base_target, " death"),
    target_end_date = as.character(date),
    observed = observed
  )

observed_deaths <- observed_deaths[!duplicated(observed_deaths), ]

observed_cases <-
  covidData::load_jhu_data(
    issue_date = jhu_issue_date,
    spatial_resolution = c("county", "state", "national"),
    temporal_resolution = "weekly",
    measure = "cases") %>%
  tidyr::pivot_longer(
    cols = c("inc", "cum"),
    names_to = "base_target",
    values_to = "observed"
  ) %>%
  dplyr::transmute(
    location = location,
    base_target = paste0("wk ahead ", base_target, " case"),
    target_end_date = as.character(date),
    observed = observed
  )

observed_cases <- observed_cases[!duplicated(observed_cases), ]

observed_hosps <-
  covidData::load_healthdata_data(
    issue_date = healthdata_issue_date,
    spatial_resolution = c("state", "national"),
    temporal_resolution = "daily",
    measure = "hospitalizations") %>%
  tidyr::pivot_longer(
    cols = c("inc", "cum"),
    names_to = "base_target",
    values_to = "observed"
  ) %>%
  dplyr::transmute(
    location = location,
    base_target = paste0("day ahead ", base_target, " hosp"),
    target_end_date = as.character(date),
    observed = observed
  )

observed_hosps <- observed_hosps[!duplicated(observed_hosps), ]

observed <- dplyr::bind_rows(observed_deaths, observed_cases, observed_hosps)

# last target date to evaluate:
#  - most recent Saturday with observed data for weekly targets
#  - most recent day with observed data for daily targets
last_weekly_target_date <- max(observed_deaths$target_end_date)
last_daily_target_date <- max(observed_hosps$target_end_date)

# dates for saturdays included in the analysis:
#  - we consider ensemble forecasts generated 2 days after this saturday date
#  - week ahead targets are defined relative to this saturday date
first_forecast_week_end_date <- lubridate::ymd("2020-05-09")
last_forecast_week_end_date <- lubridate::ymd(last_weekly_target_date) - 7
num_forecast_weeks <- as.integer(last_forecast_week_end_date -
                         first_forecast_week_end_date) / 7 + 1
forecast_week_end_dates <- as.character(
  first_forecast_week_end_date +
    seq(from = 0, length = num_forecast_weeks) * 7
)

# Dates of forecast submission for forecasts included in this analysis:
# 2 days after the saturdays
forecast_dates <- lubridate::ymd(forecast_week_end_dates) + 2


# load forecasts

# targets
all_targets <- c(
  paste0(1:4, " wk ahead cum death"),
  paste0(1:4, " wk ahead inc death"),
  paste0(1:4, " wk ahead inc case"),
  paste0(1:28, " day ahead inc hosp")
)

all_forecasts <- purrr::map_dfr(
  c("national", "state", "state_national", "county"),
  function(spatial_scale) {
    # Path to forecasts to evaluate
    submissions_root <- paste0(
      "code/application/retrospective-qra-comparison/retrospective-forecasts/",
      spatial_scale, "/"
    )

    # models to read in
    model_abbrs <- list.dirs(submissions_root, full.names = FALSE)
    model_abbrs <- model_abbrs[nchar(model_abbrs) > 0]

    if (spatial_scale == "county") {
      model_abbrs <- c(
        "intercept_FALSE-combine_method_convex-missingness_impute-quantile_groups_per_quantile-window_size_3-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE",
        "intercept_FALSE-combine_method_convex-missingness_impute-quantile_groups_per_quantile-window_size_4-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE",
        "intercept_FALSE-combine_method_ew-missingness_by_location_group-quantile_groups_per_model-window_size_0-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE",
        "intercept_FALSE-combine_method_median-missingness_by_location_group-quantile_groups_per_model-window_size_0-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE"
      )
    }

    if (spatial_scale %in% c("national", "state_national")) {
      response_vars <- c("cum_death", "inc_death", "inc_case", "inc_hosp")
    } else if (spatial_scale == "state") {
      response_vars <- c("cum_death", "inc_death", "inc_case", "inc_hosp")
    } else if (spatial_scale == "county") {
      response_vars <- "inc_case"
    }

    spatial_scale_forecasts <- purrr::map_dfr(
      response_vars,
      function(response_var) {
        if (response_var %in% c("inc_death", "cum_death")) {
          required_quantiles <-
            c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
          temporal_resolution <- "wk"
          horizon <- 4L
          targets <-
            paste0(1:horizon, " wk ahead ", gsub("_", " ", response_var))
          all_locations <- unique(observed_deaths$location)
        } else if (response_var == "inc_case") {
          required_quantiles <-
            c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
          temporal_resolution <- "wk"
          horizon <- 4L
          targets <- paste0(
            1:horizon, " wk ahead ", gsub("_", " ", response_var))
          all_locations <- unique(observed_cases$location)
        } else if (response_var == "inc_hosp") {
          required_quantiles <-
            c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
          temporal_resolution <- "day"
          horizon <- 28L
          targets <- paste0(
            1:(horizon + 6), " day ahead ", gsub("_", " ", response_var))
          all_locations <- unique(observed_hosps$location)
        }

        load_covid_forecasts_relative_horizon(
          monday_dates = forecast_dates,
          model_abbrs = model_abbrs,
          timezero_window_size = 6,
          locations = all_locations,
          targets = targets,
          horizon = horizon,
          required_quantiles = required_quantiles,
          submissions_root = submissions_root,
          include_null_point_forecasts = FALSE,
          keep_last = FALSE
        )
      }
    ) %>%
      dplyr::mutate(spatial_scale = spatial_scale)

    return(spatial_scale_forecasts)
  }
)

all_forecasts <- all_forecasts %>%
  dplyr::transmute(
    model = paste0(model, "-estimation_scale_", spatial_scale),
    forecast_date = timezero,
    location = location,
    location_name = location_name,
    geo_type = "state",
    horizon = as.integer(horizon),
    temporal_resolution = ifelse(grepl("wk", target), "wk", "day"),
    target_variable = substr(target, regexpr("ahead", target) + 6, nchar(target)),
    target_end_date = target_end_date,
    type = "quantile",
    quantile = as.numeric(quantile),
    value = value
  )
all_forecasts <- dplyr::bind_rows(
  all_forecasts,
  all_forecasts %>%
    dplyr::filter(quantile == 0.5) %>%
    dplyr::mutate(
      type = "point",
      quantile = NA
    )
)

truth <- observed %>%
  dplyr::left_join(
    covidData::fips_codes, by = "location"
  ) %>%
  dplyr::transmute(
    model = "Observed Data (JHU)",
    target_variable = substr(
      base_target,
      pmax(regexpr("inc", base_target), regexpr("cum", base_target)),
      nchar(base_target)
    ),
    target_end_date = lubridate::ymd(target_end_date),
    location = location,
    value = observed,
    geo_type = "state",
    location_name = location_name,
    abbreviation = abbreviation
  )

all_scores <- purrr::map_dfr(
  unique(all_forecasts$target_variable),
  function(tv) {
    covidHubUtils::score_forecasts(
      forecasts = all_forecasts %>%
        dplyr::filter(target_variable == tv),
      return_format = "wide",
      truth = truth
    )
  }
)

saveRDS(
  all_scores,
  "code/application/retrospective-qra-comparison/analyses/retrospective-scores/retrospective_scores.rds")
