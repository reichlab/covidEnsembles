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

# args <- c("inc_death", "2021-02-01", "national", "FALSE")
# args <- c("inc_death", "2021-02-01", "state", "FALSE")
# args <- c("cum_death", "2021-02-01", "national", "FALSE")
# args <- c("cum_death", "2021-02-01", "state", "FALSE")
# args <- c("inc_hosp", "2021-02-01", "national", "FALSE")
# args <- c("inc_hosp", "2021-02-01", "state", "FALSE")
# args <- c("inc_case", "2021-02-01", "national", "FALSE")
# args <- c("inc_case", "2021-02-01", "state", "FALSE")
# args <- c("inc_case", "2021-02-01", "county", "FALSE")
args <- commandArgs(trailingOnly = TRUE)
response_var <- args[1]
forecast_date <- lubridate::ymd(args[2])
spatial_resolution_arg <- args[3]
include_full_history <- as.logical(args[4])

if (spatial_resolution_arg %in% c("state", "national")) {
  spatial_scales <- c(spatial_resolution_arg, "state_national")
} else {
  spatial_scales <- spatial_resolution_arg
}

# Dates of forecast submission for forecasts included in this analysis
if (response_var %in% c("cum_death", "inc_death")) {
  first_forecast_date <- lubridate::ymd("2020-06-22")
} else if (response_var == "inc_case") {
  first_forecast_date <- lubridate::ymd("2020-09-14")
} else if (response_var == "inc_hosp") {
  first_forecast_date <- lubridate::ymd("2020-11-23")
}
num_forecast_weeks <- as.integer(forecast_date - 7 - first_forecast_date)/7 + 1

forecast_dates <- first_forecast_date +
  seq(from = 0, length = num_forecast_weeks) * 7

all_scores <- calc_retrospective_ensemble_scores(
  submissions_root = "code/application/retrospective-qra-comparison/retrospective-forecasts/",
  forecast_dates = forecast_dates,
  spatial_scales = spatial_scales,
  response_vars = response_var,
  truth_as_of = forecast_date - 1
) %>%
  dplyr::filter(
    !grepl("ensemble_switching", model),
    !grepl("combine_method_positive", model)#,
#    !grepl("check_missingness_by_target_TRUE", model)
  ) %>%
  dplyr::mutate(
    spatial_scale = ifelse(
      location == "US",
      "National",
      ifelse(
        nchar(location) == 2,
        "State",
        "County"
      )
    )
  )

all_model_cases <- purrr::map_dfr(
  unique(all_scores$model),
  function(x) {
    parse_model_case(x) %>% dplyr::mutate(model = x)
  }
)

all_scores <- all_scores %>%
  dplyr::left_join(all_model_cases, by = "model") %>%
  dplyr::mutate(
    target = paste(horizon, temporal_resolution, "ahead", target_variable),
    model_brief = paste(
      combine_method,
      "window",
      window_size,
      quantile_groups,
      estimation_grouping,
      sep = "_"
    )
  )

if (!include_full_history) {
  all_scores <- all_scores %>%
    dplyr::filter(window_size != "full_history")
}


# subset scores to those that are comparable for all models within each
# combination of spatial scale and base target
# only among those models with any forecasts for that combination
all_scores_common_by_target_variable_spatial_scale <-
  purrr::pmap_dfr(
    all_scores %>%
      distinct(target_variable, spatial_scale),
    function(target_variable, spatial_scale) {
      reduced_scores <- all_scores %>%
        dplyr::filter(
          target_variable == UQ(target_variable),
          spatial_scale == UQ(spatial_scale)
        )

      # subset to same forecasts made for each ensemble method
      scores_to_keep <- reduced_scores %>%
        dplyr::select(model, forecast_date, location, target, abs_error) %>%
        tidyr::pivot_wider(
          names_from = "model", values_from = "abs_error"
        )
      all_models <- unique(reduced_scores$model)
      scores_to_keep$keep <-
        apply(scores_to_keep[all_models], 1, function(x) all(!is.na(x)))

      # message(paste0(
      #   "at ", spatial_scale, " for ", target_variable,
      #   ", missing forecasts for models: ",
      #   paste0(
      #     all_models[apply(scores_to_keep[all_models], 2, function(x) any(is.na(x)))]
      #   )
      # ))

      scores_to_keep <- scores_to_keep %>%
        dplyr::select(forecast_date, location, target, keep)

      dplyr::left_join(
        reduced_scores,
        scores_to_keep,
        by = c("forecast_date", "location", "target")
      ) %>%
        dplyr::filter(keep) %>%
        dplyr::select(-keep)
    }
  )

# score summaries
scores_overall <- all_scores_common_by_target_variable_spatial_scale %>%
  dplyr::mutate(
    target_variable = substr(target, regexpr(" ", target) + 1, nchar(target)),
    spatial_scale = ifelse(
      location == "US",
      "National",
      ifelse(
        nchar(location) == 2,
        "State",
        "County"
      )
    )
  ) %>%
  dplyr::group_by(
    model, model_brief, intercept, combine_method, missingness, quantile_groups,
    window_size, check_missingness_by_target, do_standard_checks,
    do_baseline_check, estimation_grouping, target_variable, spatial_scale) %>%
  dplyr::summarize(
    across(starts_with("abs_error"), function(x) round(mean(x), 3)),
    across(starts_with("wis"), function(x) round(mean(x), 3)),
    across(starts_with("coverage"), function(x) round(mean(x), 3))#,
  )

# Extract model with lowest WIS for each combination of
# spatial scale and target variable
best_models <- scores_overall %>%
  dplyr::filter(!(spatial_scale == "County" & window_size == "4")) %>%
  dplyr::group_by(target_variable, spatial_scale) %>%
  dplyr::slice_min(wis) %>%
  dplyr::filter(tolower(spatial_scale) == tolower(spatial_resolution_arg))

# For each combination of spatial scale and target variable,
# copy the forecast file for the selected ensemble from the original model's
# folder to the prospective-selection folder
for (i in seq_len(nrow(best_models))) {
  case_str <- paste0(
    "intercept_", as.character(best_models$intercept[i]),
    "-combine_method_", best_models$combine_method[i],
    "-missingness_", best_models$missingness[i],
    "-quantile_groups_", best_models$quantile_groups[i],
    "-window_size_", best_models$window_size[i],
    "-check_missingness_by_target_", best_models$check_missingness_by_target[i],
    "-do_standard_checks_", best_models$do_standard_checks[i],
    "-do_baseline_check_", best_models$do_baseline_check[i])

  source_forecasts_dir <- file.path(
    "code/application/retrospective-qra-comparison/retrospective-forecasts",
    best_models$estimation_grouping[i],
    case_str)

  source_forecast_filename <- paste0(
    source_forecasts_dir, "/",
    response_var, "-", forecast_date, "-",
    case_str, ".csv")

  target_forecasts_dir <- file.path(
    "code/application/retrospective-qra-comparison/retrospective-forecasts",
    tolower(best_models$spatial_scale[i]),
    paste0("prospective_selection-include_full_history_", include_full_history)
  )

  if (!dir.exists(target_forecasts_dir)) {
    dir.create(target_forecasts_dir)
  }

  target_forecast_filename <- paste0(
    target_forecasts_dir, "/",
    response_var, "-", forecast_date, "-",
    "prospective_selection-include_full_history_",
    include_full_history,
    ".csv"
  )

  forecasts <- readr::read_csv(
    source_forecast_filename,
    col_types = readr::cols(
      forecast_date = readr::col_date(format = ""),
      target = readr::col_character(),
      target_end_date = readr::col_date(format = ""),
      location = readr::col_character(),
      type = readr::col_character(),
      quantile = readr::col_double(),
      value = readr::col_double())
  )

  if (spatial_resolution_arg == "state") {
    forecasts <- forecasts %>%
      dplyr::filter(location != "US")
  } else if (spatial_resolution_arg == "national") {
    forecasts <- forecasts %>%
      dplyr::filter(location == "US")
  }

  readr::write_csv(forecasts, target_forecast_filename)
}


# For each combination of spatial scale and target variable,
# copy the weights file for the selected ensemble from the original model's
# folder to the prospective-selection folder
for (i in seq_len(nrow(best_models))) {
  case_str <- paste0(
    "intercept_", as.character(best_models$intercept[i]),
    "-combine_method_", best_models$combine_method[i],
    "-missingness_", best_models$missingness[i],
    "-quantile_groups_", best_models$quantile_groups[i],
    "-window_size_", best_models$window_size[i],
    "-check_missingness_by_target_", best_models$check_missingness_by_target[i],
    "-do_standard_checks_", best_models$do_standard_checks[i],
    "-do_baseline_check_", best_models$do_baseline_check[i])

  source_weights_dir <- file.path(
    "code/application/retrospective-qra-comparison/retrospective-weights",
    best_models$estimation_grouping[i],
    case_str)

  source_weights_filename <- paste0(
    source_weights_dir, "/",
    response_var, "-", forecast_date, "-",
    case_str, ".csv")

  target_forecasts_dir <- file.path(
    "code/application/retrospective-qra-comparison/retrospective-forecasts",
    tolower(best_models$spatial_scale[i]),
    paste0("prospective_selection-include_full_history_", include_full_history)
  )

  if (!dir.exists(target_forecasts_dir)) {
    dir.create(target_forecasts_dir)
  }

  target_forecast_filename <- paste0(
    target_forecasts_dir, "/",
    response_var, "-", forecast_date, "-",
    "prospective_selection-include_full_history_",
    include_full_history,
    ".csv"
  )

  forecasts <- readr::read_csv(
    source_forecast_filename,
    col_types = readr::cols(
      forecast_date = readr::col_date(format = ""),
      target = readr::col_character(),
      target_end_date = readr::col_date(format = ""),
      location = readr::col_character(),
      type = readr::col_character(),
      quantile = readr::col_double(),
      value = readr::col_double())
  )

  if (spatial_resolution_arg == "state") {
    forecasts <- forecasts %>%
      dplyr::filter(location != "US")
  } else if (spatial_resolution_arg == "national") {
    forecasts <- forecasts %>%
      dplyr::filter(location == "US")
  }

  readr::write_csv(forecasts, target_forecast_filename)
}
