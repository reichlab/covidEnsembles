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

# Set of targets for which we'll use the trained ensemble; we will just copy
# forecasts and metadata for these targets from the COVIDhub-trained_ensemble
trained_response_vars <- c("inc_death", "cum_death")

# Where to find component model submissions
submissions_root <- '../covid19-forecast-hub/data-processed/'

# Parent dir of this which is the hub clone path used by
# covidHubUtils::load_latest_forecasts for loading locally
hub_repo_path <- '../covid19-forecast-hub/'

# Where to save ensemble forecasts
save_roots <- c('code/application/weekly-ensemble/forecasts/')
for (root in save_roots) {
  if (!file.exists(root)) dir.create(root, recursive = TRUE)
  if (!file.exists(paste0(root,"ensemble-metadata/"))) {
    dir.create(paste0(root,"ensemble-metadata/"), recursive = TRUE)
  }
}

# Where to save plots
plots_root <- 'code/application/weekly-ensemble/plots/COVIDhub-ensemble/'
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

# Drop hospitalizations ensemble from JHU APL and ensemble from FDANIHASU
candidate_model_abbreviations_to_include <-
  candidate_model_abbreviations_to_include[
    !(candidate_model_abbreviations_to_include %in% c("JHUAPL-SLPHospEns", "FDANIHASU-Sweight", "COVIDhub-trained_ensemble", "KITmetricslab-select_ensemble"))
  ]


# Figure out what day it is; forecast creation date is set to a Monday,
# even if we are delayed and create it Tuesday morning.
forecast_date <- lubridate::floor_date(Sys.Date(), unit = "week") + 1

for (response_var in c("cum_death", "inc_death", "inc_case", "inc_hosp")) {
  if (response_var == "cum_death") {
    do_q10_check <- do_nondecreasing_quantile_check <- TRUE
    do_sd_check <- "exclude_none"
    required_quantiles <-
      c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
    spatial_resolution <- c("state", "national")
    temporal_resolution <- "wk"
    horizon <- 4L
    targets <- paste0(1:horizon, " wk ahead ", gsub("_", " ", response_var))
    forecast_week_end_date <- forecast_date - 2

    # date for which retrieved deaths truth data should be current
    data_as_of_date <- covidData::available_issue_dates("deaths") %>% max()

    # adjustments based on plots
    if (forecast_date == "2020-06-08") {
      manual_eligibility_adjust <- c(
        "Auquan-SEIR", "CAN-SEIR_CAN", "CU-select", "UA-EpiCovDA",
        "SWC-TerminusCM"
      )
    } else if (forecast_date == "2020-06-15") {
      manual_eligibility_adjust <- "Auquan-SEIR"
    } else if (forecast_date == "2020-06-29") {
      manual_eligibility_adjust <- data.frame(
        model = c("epiforecasts-ensemble1", "NotreDame-mobility"),
        location = "34",
        message = "Visual misalignment of predictive quantiles with JHU reference data."
      )
    } else if (forecast_date == "2020-07-06") {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c("COVIDhub-baseline", "CU-select", "RobertWalraven-ESG",
          "USACE-ERDC_SEIR", "MITCovAlliance-SIR"),
        location = fips_codes$location,
        message = "Visual misalignment of predictive quantiles with JHU reference data."
      )
    } else if (forecast_date == "2020-07-13") {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c("COVIDhub-baseline", "RobertWalraven-ESG", "USACE-ERDC_SEIR",
          "MITCovAlliance-SIR"),
        location = fips_codes$location,
        message = "Visual misalignment of predictive quantiles with JHU reference data."
      )
    } else if (forecast_date == "2020-07-20") {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c(
          "COVIDhub-baseline", "MITCovAlliance-SIR", "RobertWalraven-ESG",
          "USACE-ERDC_SEIR"),
        location = fips_codes$location,
        message = "Visual misalignment of predictive quantiles with JHU reference data."
      )
    } else {
      manual_eligibility_adjust <- NULL
    }
  } else if (response_var == 'inc_death') {
    do_q10_check <- do_nondecreasing_quantile_check <- FALSE
    do_sd_check <- "exclude_none"
    required_quantiles <-
      c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
    spatial_resolution <- c("state", "national")
    temporal_resolution <- "wk"
    horizon <- 4L
    targets <- paste0(1:horizon, " wk ahead ", gsub("_", " ", response_var))
    forecast_week_end_date <- forecast_date - 2

    # date for which retrieved deaths truth data should be current
    # repeated from cum_death block for clarity
    data_as_of_date <- covidData::available_issue_dates("deaths") %>% max()

    # adjustments based on plots
    if (forecast_date == "2020-06-08") {
      manual_eligibility_adjust <- c(
        "CAN-SEIR_CAN", "SWC-TerminusCM", "USACE-ERDC_SEIR", "IHME-CurveFit"
      )
    } else if (forecast_date == "2020-06-15") {
      manual_eligibility_adjust <- c(
        "USACE-ERDC_SEIR", "LANL-GrowthRate"
      )
    } else if (forecast_date == "2020-06-29") {
      manual_eligibility_adjust <- bind_rows(
        data.frame(
          model = c("epiforecasts-ensemble1", "NotreDame-mobility"),
          location = "34",
          message = "Visual misalignment of predictive quantiles with JHU reference data.",
          stringsAsFactors = FALSE
        ),
        data.frame(
          model = "CU-select",
          location = fips_codes$location,
          message = "Visual misalignment of predictive quantiles with JHU reference data.",
          stringsAsFactors = FALSE
        )
      )
    } else if (forecast_date == "2020-07-06") {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c("COVIDhub-baseline", "CU-select", "RobertWalraven-ESG",
          "USACE-ERDC_SEIR", "MITCovAlliance-SIR"),
        location = fips_codes$location,
        message = "Visual misalignment of predictive quantiles with JHU reference data."
      )
    } else if (forecast_date == "2020-07-13") {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c("COVIDhub-baseline", "RobertWalraven-ESG", "USACE-ERDC_SEIR",
          "MITCovAlliance-SIR"),
        location = fips_codes$location,
        message = "Visual misalignment of predictive quantiles with JHU reference data."
      )
    } else if (forecast_date == "2020-07-20") {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c(
          "COVIDhub-baseline", "MITCovAlliance-SIR", "MOBS-GLEAM_COVID",
          "RobertWalraven-ESG", "USACE-ERDC_SEIR"),
        location = fips_codes$location,
        message = "Visual misalignment of predictive quantiles with JHU reference data."
      )
    } else {
      manual_eligibility_adjust <- NULL
    }
  } else if (response_var == "inc_case") {
    do_q10_check <- do_nondecreasing_quantile_check <- FALSE
    do_sd_check <- "exclude_none"
    required_quantiles <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
    spatial_resolution <- c('county', 'state', 'national')
    temporal_resolution <- "wk"
    horizon <- 1L
    targets <- paste0(1:horizon, " wk ahead ", gsub("_", " ", response_var))
    forecast_week_end_date <- forecast_date - 2

    # date for which retrieved cases truth data should be current
    data_as_of_date <- covidData::available_issue_dates("cases") %>% max()

    if (forecast_date == "2020-07-13") {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c("CU-select", "IowaStateLW-STEM", "JHU_IDD-CovidSP",
                  "LANL-GrowthRate", "RobertWalraven-ESG", "USACE-ERDC_SEIR",
                  "Covid19Sim-Simulator", "MIT_CovidAnalytics-DELPHI",
                  "CDDEP-SEIR"),
        location = covidData::fips_codes$location,
        message = "Visual misalignment of predictive quantiles with JHU reference data."
      )
    } else if (forecast_date == "2020-07-20") {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c(
          "CDDEP-SEIR_MCMC", "Covid19Sim-Simulator", "CovidAnalytics-DELPHI",
          "CU-select", "IHME-CurveFit", "IowaStateLW-STEM", "JHU_IDD-CovidSP",
          "MITCovAlliance-SIR", "RobertWalraven-ESG", "USACE-ERDC_SEIR",
          "UVA-Ensemble"),
        location = covidData::fips_codes$location,
        message = "Visual misalignment of predictive quantiles with JHU reference data."
      )
    } else {
      manual_eligibility_adjust <- NULL
    }
  } else if (response_var == "inc_hosp") {
    do_q10_check <- do_nondecreasing_quantile_check <- FALSE
    do_sd_check <- "exclude_none"
    required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
    spatial_resolution <- c("state", "national")
    temporal_resolution <- "day"
    horizon <- 14L
    targets <- paste0(1:(horizon + 6), " day ahead ", gsub("_", " ", response_var))
    forecast_week_end_date <- forecast_date

    # date for which retrieved hospitalization truth data should be current
    data_as_of_date <- covidData::available_issue_dates("hospitalizations") %>% max()

    if (forecast_date == "2020-12-07") {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c("Google_Harvard-CPF"),
        location = covidData::fips_codes$location,
        message = "Mean daily point forecast for first seven days less than mean reported hospitalizations over past two weeks minus four standard deviations."
      )
    } else if (forecast_date == "2020-12-14") {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c("Google_Harvard-CPF", "IHME-CurveFit", "UCLA-SuEIR"),
        location = covidData::fips_codes$location,
        message = "Mean daily point forecast for first seven days less than mean reported hospitalizations over past two weeks minus four standard deviations."
      )
    } else if (forecast_date == "2020-12-21") {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c("Google_Harvard-CPF", "IHME-CurveFit", "UCLA-SuEIR"),
        location = covidData::fips_codes$location,
        message = "Mean daily point forecast for first seven days less than mean reported hospitalizations over past two weeks minus four standard deviations."
      )
    } else if (forecast_date == "2020-12-28") {
      manual_eligibility_adjust <- readr::read_csv(
        "code/application/weekly-ensemble/exclusion-inputs/Hosp_Models_Locations with Thresholds Below SD_2020-12-29.csv"
      ) %>%
        dplyr::mutate(
          location_name =
            ifelse(location_name == "National", "US", location_name),
          message = "Mean daily point forecast for first seven days less than mean reported hospitalizations over past two weeks minus four standard deviations."
        ) %>%
        dplyr::left_join(covidData::fips_codes, by = "location_name") %>%
        dplyr::select(model, location, message)
    } else if (forecast_date == "2021-01-04") {
      manual_eligibility_adjust <- readr::read_csv(
        "code/application/weekly-ensemble/exclusion-inputs/Hosp_Models_Locations with Thresholds Below SD_2021-01-05.csv"
      ) %>%
        dplyr::mutate(
          location_name =
            ifelse(location_name == "National", "US", location_name),
          message = "Mean daily point forecast for first seven days less than mean reported hospitalizations over past two weeks minus four standard deviations."
        ) %>%
        dplyr::left_join(covidData::fips_codes, by = "location_name") %>%
        dplyr::select(model, location, message)
    } else if (forecast_date == "2021-01-11") {
      manual_eligibility_adjust <- readr::read_csv(
        "code/application/weekly-ensemble/exclusion-inputs/Hosp_Models_Locations with Thresholds Below SD_updated_2021-01-12.csv"
      ) %>%
        dplyr::mutate(
          location_name =
            ifelse(location_name == "National", "US", location_name),
          message = "Mean daily point forecast for first seven days less than mean reported hospitalizations over past two weeks minus four standard deviations."
        ) %>%
        dplyr::left_join(covidData::fips_codes, by = "location_name") %>%
        dplyr::select(model, location, message)
    } else if (forecast_date == "2021-01-18") {
      manual_eligibility_adjust <- readr::read_csv(
        "code/application/weekly-ensemble/exclusion-inputs/Hosp_Models_Locations with Thresholds Below SD_updated_2021-01-19.csv"
      ) %>%
        dplyr::mutate(
          location_name =
            ifelse(location_name == "National", "US", location_name),
          message = "Mean daily point forecast for first seven days less than mean reported hospitalizations over past two weeks minus four standard deviations."
        ) %>%
        dplyr::left_join(covidData::fips_codes, by = "location_name") %>%
        dplyr::select(model, location, message)        
    } else if (forecast_date == "2021-01-25") {
      manual_eligibility_adjust <- readr::read_csv(
        "code/application/weekly-ensemble/exclusion-inputs/Hosp_Models_Locations with Thresholds Below SD_updated_2021-01-26.csv"
      ) %>%
        dplyr::mutate(
          location_name =
            ifelse(location_name == "National", "US", location_name),
          message = "Mean daily median forecast for first seven days less than mean reported hospitalizations over past two weeks minus four standard deviations."
        ) %>%
        dplyr::left_join(covidData::fips_codes, by = "location_name") %>%
        dplyr::select(model, location, message)
    } else {
      manual_eligibility_adjust <- NULL
    }
  }

  if (response_var %in% trained_response_vars) {
    # get predictions from COVIDhub-trained_ensemble outputs
    trained_save_dir <- paste0(root, 'data-processed/COVIDhub-trained_ensemble/')
    trained_predictions <- read_csv(
      paste0(trained_save_dir,
             forecast_date,
             '-COVIDhub-trained_ensemble.csv'),
      col_types = cols(
        forecast_date = col_date(format = ""),
        target = col_character(),
        target_end_date = col_character(),
        location = col_character(),
        type = col_character(),
        quantile = col_double(),
        value = col_double()
      )
    )
    formatted_ensemble_predictions <- trained_predictions %>%
      dplyr::filter(grepl(gsub("_", " ", response_var), target)) %>%
      dplyr::mutate(location_name = NA_character_)
  } else {
    # get predictions and metadata from a new fit
    c(model_eligibility, wide_model_eligibility, location_groups, component_forecasts) %<-%
      build_covid_ensemble(
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
        window_size = 0,
        data_as_of_date = data_as_of_date,
        intercept = FALSE,
        combine_method = 'median',
        quantile_groups = rep(1, 23),
        missingness = 'by_location_group',
        backend = NA,
        required_quantiles = required_quantiles,
        do_q10_check = do_q10_check,
        do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
        do_baseline_check = FALSE,
        do_sd_check = do_sd_check, # implement CDC exclusion requests
        sd_check_table_path = sd_check_table_path,
        sd_check_plot_path = sd_check_plot_path,
        manual_eligibility_adjust = manual_eligibility_adjust,
        return_eligibility = TRUE,
        return_all = TRUE
      )

    # subset ensemble forecasts to only locations where more than 1
    # component model contributed.
    model_counts <- apply(
      location_groups %>% select_if(is.logical),
      1,
      sum)
    location_groups <- location_groups[model_counts > 1, ]
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

    # reformat model weights and eligibility for output
    model_weights <- location_groups %>%
      dplyr::select(-qfm_train, -qfm_test, -y_train, -qra_fit, -qra_forecast) %>%
      tidyr::unnest(locations) %>%
      dplyr::mutate_if(is.logical, as.numeric)
  }

  # add predictions for response_var to all_formatted_ensemble_predictions
  if (response_var == 'cum_death') {
    all_formatted_ensemble_predictions <- formatted_ensemble_predictions
  } else {
    all_formatted_ensemble_predictions <- bind_rows(
      all_formatted_ensemble_predictions,
      formatted_ensemble_predictions
    )
  }

  for(root in save_roots) {
    if(final_run) {
      save_dir <- paste0(root, 'data-processed/COVIDhub-ensemble/')
      if (!file.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
      write_csv(all_formatted_ensemble_predictions %>% select(-location_name),
                paste0(save_dir,
                       formatted_ensemble_predictions$forecast_date[1],
                       '-COVIDhub-ensemble.csv')
      )

      save_dir <- paste0(root, "ensemble-metadata/")
      if (!file.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

      eligibility_path <- paste0(save_dir,
        formatted_ensemble_predictions$forecast_date[1],
        '-',
        response_var,
        '-model-eligibility.csv')
      weights_path <- paste0(save_dir,
        formatted_ensemble_predictions$forecast_date[1],
        '-',
        response_var,
        '-model-weights.csv')

      if (response_var %in% trained_response_vars) {
        # copy metadata and weights files from trained ensemble outputs
        trained_save_dir <- paste0(root, "trained_ensemble-metadata/")
        trained_eligibility_path <- paste0(trained_save_dir,
          forecast_date,
          '-',
          response_var,
          '-model-eligibility.csv')
        trained_weights_path <- paste0(trained_save_dir,
          forecast_date,
          '-',
          response_var,
          '-model-weights.csv')
        file.copy(from = trained_eligibility_path, to = eligibility_path)
        file.copy(from = trained_weights_path, to = weights_path)
      } else {
        # save 
        write_csv(model_eligibility, eligibility_path)
        write_csv(model_weights, weights_path)
      }
    }
  }
}

# Check that all models that had any submission for each target are in the
# eligibility metadata file
for (response_var in c("inc_death", "cum_death", "inc_case", "inc_hosp")) {
  if (response_var == "inc_hosp") {
    targets <- paste0(1:14, " day ahead inc hosp")
  } else if(response_var == "inc_case") {
    targets <- paste0(1, ' wk ahead inc case')
  } else {
    targets <- paste0(1:4, ' wk ahead ', gsub("_", " ", response_var))
  }
  all_forecasts <- covidHubUtils::load_latest_forecasts(
    models = candidate_model_abbreviations_to_include,
    last_forecast_date = forecast_date,
    forecast_date_window_size = 6,
    targets = targets,
    hub_repo_path = "../covid19-forecast-hub"
  )
  all_models <- unique(all_forecasts$model)

  save_dir <- paste0(root, "ensemble-metadata/")
  eligibility <- read_csv(paste0(save_dir,
    forecast_date,
    '-',
    response_var,
    '-model-eligibility.csv'))

  locations <- unique(eligibility$location)

  val_result <- identical(
    sort(paste0(eligibility$location, eligibility$model)),
    tidyr::expand_grid(
      model = all_models,
      location = locations
    ) %>%
    dplyr::mutate(lm = paste0(location, model)) %>%
    dplyr::pull(lm) %>%
    sort()
  )
  message(paste0("CHECK THAT ALL MODELS ARE IN ELIGIBILITY FILE: ", response_var))
  message(val_result)
}

# make plots of ensemble submission
plot_forecasts_single_model(
  submissions_root = paste0(root, "data-processed/"),
  plots_root = plots_root,
  forecast_date = forecast_date,
  model_abbrs = "COVIDhub-ensemble",
  target_variables = c("cases", "deaths", "hospitalizations")
)
