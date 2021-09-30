library(tidyverse)
library(zeallot)
library(covidHubUtils)
library(covidEnsembles)
library(covidData)
library(googledrive)
library(yaml)
library(here)
options(error = recover)
setwd(here())

final_run <- TRUE

# Where to find component model submissions
submissions_root <- '../covid19-forecast-hub/data-processed/'

# Parent dir of this which is the hub clone path used by
# covidHubUtils::load_latest_forecasts for loading locally
hub_repo_path <- '../covid19-forecast-hub/'

# Where to save ensemble forecasts
save_roots <- c('code/application/weekly-ensemble/forecasts/')
for (root in save_roots) {
  if (!file.exists(root)) dir.create(root, recursive = TRUE)
  if (!file.exists(paste0(root,"trained_ensemble-metadata/"))) {
    dir.create(paste0(root,"trained_ensemble-metadata/"), recursive = TRUE)
  }
}

# Where to save plots
plots_root <- 'code/application/weekly-ensemble/plots/COVIDhub-trained_ensemble/'
if (!file.exists(plots_root)) dir.create(plots_root, recursive = TRUE)

# Where to save hospitalization exclusion tables
sd_check_table_path <- 'code/application/weekly-ensemble/exclusion-outputs/tables/'
if (!file.exists(sd_check_table_path)) dir.create(sd_check_table_path, recursive = TRUE)
sd_check_plot_path <- 'code/application/weekly-ensemble/exclusion-outputs/plots/'
if (!file.exists(sd_check_plot_path)) dir.create(sd_check_plot_path, recursive = TRUE)

# List of candidate models for inclusion in ensemble
candidate_model_abbreviations_to_include <- get_candidate_models(
  submissions_root = submissions_root,
  include_designations = c("primary", "secondary"),
  include_COVIDhub_ensemble = FALSE,
  include_COVIDhub_baseline = TRUE)

# Drop other ensemble models
excluded_ensembles <- c("JHUAPL-SLPHospEns", "FDANIHASU-Sweight",
  "COVIDhub-trained_ensemble", "KITmetricslab-select_ensemble")
candidate_model_abbreviations_to_include <-
  candidate_model_abbreviations_to_include[
    !(candidate_model_abbreviations_to_include %in% excluded_ensembles)
  ]


# Figure out what day it is; forecast creation date is set to a Monday,
# even if we are delayed and create it Tuesday morning.
forecast_date <- lubridate::floor_date(Sys.Date(), unit = "week") + 1

tic <- Sys.time()
for (response_var in c("cum_death", "inc_death", "inc_case", "inc_hosp")) {
#for (response_var in c("inc_case", "inc_hosp")) {
#for (response_var in c("cum_death", "inc_death", "inc_case")) {
  # reset model_weights to NULL
  model_weights <- NULL

  print(response_var)
  if (response_var == "cum_death") {
    do_q10_check <- do_nondecreasing_quantile_check <- TRUE
    do_sd_check <- "exclude_none"
    required_quantiles <-
      c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
    spatial_resolutions <- c("state", "national")
    temporal_resolution <- "wk"
    horizon <- 4L
    targets <- paste0(1:horizon, " wk ahead ", gsub("_", " ", response_var))
    forecast_week_end_date <- forecast_date - 2

    window_size <- 12L
    top_models <- 10L
    missingness <- "impute"
    impute_method <- "none"

    # date for which retrieved deaths truth data should be current
    data_as_of_date <- covidData:::available_issue_dates("deaths") %>% max()
  } else if (response_var == 'inc_death') {
    do_q10_check <- do_nondecreasing_quantile_check <- FALSE
    do_sd_check <- "exclude_none"
    required_quantiles <-
      c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
    spatial_resolutions <- c("state", "national")
    temporal_resolution <- "wk"
    horizon <- 4L
    targets <- paste0(1:horizon, " wk ahead ", gsub("_", " ", response_var))
    forecast_week_end_date <- forecast_date - 2

    window_size <- 12L
    top_models <- 10L
    missingness <- "impute"
    impute_method <- "none"

    # date for which retrieved deaths truth data should be current
    # repeated from cum_death block for clarity
    data_as_of_date <- covidData:::available_issue_dates("deaths") %>% max()
  } else if (response_var == "inc_case") {
    do_q10_check <- do_nondecreasing_quantile_check <- FALSE
    do_sd_check <- "exclude_none"
    required_quantiles <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
    spatial_resolutions <- c('county', 'state', 'national')
    temporal_resolution <- "wk"
    horizon <- 4L
    targets <- paste0(1:horizon, " wk ahead ", gsub("_", " ", response_var))
    forecast_week_end_date <- forecast_date - 2

    window_size <- 12L
    top_models <- 10L
    missingness <- "impute"
    impute_method <- "none"

    # date for which retrieved cases truth data should be current
    data_as_of_date <- covidData:::available_issue_dates("cases") %>% max()
  } else if (response_var == "inc_hosp") {
    do_q10_check <- do_nondecreasing_quantile_check <- FALSE
    do_sd_check <- "exclude_none"
    required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
    spatial_resolutions <- c("state", "national")
    temporal_resolution <- "day"
    horizon <- 28L
    targets <- paste0(1:(horizon + 6), " day ahead ", gsub("_", " ", response_var))
    forecast_week_end_date <- forecast_date

    window_size <- 12L
    top_models <- 10L
    missingness <- "impute"
    impute_method <- "none"

    # date for which retrieved hospitalization truth data should be current
    data_as_of_date <- covidData:::available_issue_dates("hospitalizations") %>% max()
  }

  combine_method <- 'rel_wis_weighted_median'
  for (spatial_resolution in spatial_resolutions) {
    results <- build_covid_ensemble(
      hub = "US",
      source = "local_hub_repo",
      hub_repo_path = hub_repo_path,
      candidate_model_abbreviations_to_include =
        candidate_model_abbreviations_to_include,
      spatial_resolution = spatial_resolution,
      targets = targets,
      forecast_date = forecast_date,
      forecast_week_end_date = forecast_week_end_date,
      max_horizon = horizon,
      timezero_window_size = 6,
      window_size = window_size,
      data_as_of_date = data_as_of_date,
      intercept = FALSE,
      combine_method = combine_method,
      quantile_groups = rep(1, 23),
      noncross = "sort",
      missingness = missingness,
      impute_method = impute_method,
      backend = ifelse(
        combine_method == "rel_wis_weighted_median",
        "grid_search",
        "qenspy"),
      required_quantiles = required_quantiles,
      check_missingness_by_target = TRUE,
      do_q10_check = FALSE,
      do_nondecreasing_quantile_check = FALSE,
      do_baseline_check = FALSE,
      do_sd_check = do_sd_check, # implement CDC exclusion requests
      baseline_tol = 1.0,
      top_models = top_models,
      sd_check_table_path = sd_check_table_path,
      sd_check_plot_path = sd_check_plot_path,
      manual_eligibility_adjust = NULL,
      return_eligibility = TRUE,
      return_all = TRUE
    )

    if (missingness == "impute") {
      c(model_eligibility, wide_model_eligibility, location_groups,
        weight_transfer, component_forecasts) %<-% results
    } else {
      c(model_eligibility, wide_model_eligibility, location_groups,
        component_forecasts) %<-% results
    }

    ensemble_predictions <- bind_rows(location_groups[['qra_forecast']])

    # save the results in required format
    formatted_ensemble_predictions <- ensemble_predictions %>%
      left_join(
        fips_codes,# %>% select(location, location_name = location_abbreviation),
        by='location') %>%
      dplyr::transmute(
        forecast_date = UQ(forecast_date),
        target = target,
        target_end_date = as.character(
            lubridate::ymd(forecast_week_end_date) +
              as.numeric(substr(target, 1, regexpr(" ", target, fixed = TRUE) - 1)) *
                ifelse(grepl("day", target), 1, 7)
          ),
        location = location,
        location_name = location_name,
        type = 'quantile',
        quantile = quantile,
        value = ifelse(
          quantile < 0.5,
          floor(value),
          ifelse(
            quantile == 0.5,
            round(value),
            ceiling(value)
          )
        )
      )

    formatted_ensemble_predictions <- bind_rows(
      formatted_ensemble_predictions,
      formatted_ensemble_predictions %>%
        filter(format(quantile, digits=3, nsmall=3) == '0.500') %>%
        mutate(
          type='point',
          quantile=NA_real_
        )
    )

    model_weights <- dplyr::bind_rows(
      model_weights,
      location_groups$qra_fit[[1]]$coefficients %>%
        tidyr::pivot_wider(names_from = "model", values_from = "beta") %>%
        dplyr::full_join(
          data.frame(locations = location_groups$locations[[1]], stringsAsFactors = FALSE),
          by = character()
        )
    )
    model_weights <- dplyr::relocate(model_weights, locations, .after = dplyr::last_col())
    model_weights[is.na(model_weights)] <- 0.0

    if(response_var == 'cum_death' && spatial_resolution == spatial_resolutions[1]) {
      all_formatted_ensemble_predictions <- formatted_ensemble_predictions
    } else {
      all_formatted_ensemble_predictions <- bind_rows(
        all_formatted_ensemble_predictions,
        formatted_ensemble_predictions
      )
    }

    if (!exists("thetas")) {
      thetas <- tibble(
        forecast_date = forecast_date,
        response_var = response_var,
        spatial_resolution = spatial_resolution,
        theta = round(location_groups$qra_fit[[1]]$par,1)
      )
    } else {
      thetas <- thetas %>% dplyr::add_row(
        forecast_date = forecast_date,  
        response_var = response_var,
        spatial_resolution = spatial_resolution,
        theta = round(location_groups$qra_fit[[1]]$par,1)
      )
    }

    for(root in save_roots) {
      if(final_run) {
        save_dir <- paste0(root, 'data-processed/COVIDhub-trained_ensemble/')
        if (!file.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
        write_csv(all_formatted_ensemble_predictions %>% select(-location_name),
                  paste0(save_dir,
                        formatted_ensemble_predictions$forecast_date[1],
                        '-COVIDhub-trained_ensemble.csv')
        )

        # not saving metadata for now
        save_dir <- paste0(root, "trained_ensemble-metadata/")
        if (!file.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
        write_csv(model_weights,
          paste0(save_dir,
            formatted_ensemble_predictions$forecast_date[1],
            '-',
            response_var,
            '-model-weights.csv'))
      }
    }
  }
}
toc <- Sys.time()
print(toc-tic)

save_dir <- paste0(root, "trained_ensemble-metadata/")
if (!file.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

write_csv(thetas, paste0(save_dir, 'thetas.csv'), append = TRUE)

# make plots of ensemble submission
plot_forecasts_single_model(
  submissions_root = paste0(root, "data-processed/"),
  plots_root = plots_root,
  forecast_date = forecast_date,
  model_abbrs = "COVIDhub-trained_ensemble",
  target_variables = c("cases", "deaths", "hospitalizations")
)
