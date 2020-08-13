library(tidyverse)
library(zeallot)
library(covidEnsembles)
library(covidData)
library(googledrive)
library(yaml)
library(here)
options(error=recover)
setwd(here())

final_run <- TRUE

# Where to find component model submissions
submissions_root <- '../covid19-forecast-hub/data-processed/'

# Where to save ensemble forecasts
save_roots <- c('code/application/weekly-ensemble/forecasts/')
for (root in save_roots) {
  if (!file.exists(root)) dir.create(root, recursive = TRUE)
}

# Where to save plots
plots_root <- 'code/application/weekly-ensemble/plots/COVIDhub-ensemble/'
if (!file.exists(plots_root)) dir.create(plots_root, recursive = TRUE)

# List of candidate models for inclusion in ensemble
candidate_model_abbreviations_to_include <- get_candidate_models(
  submissions_root = submission_root,
  include_designations = c("primary", "secondary"),
  include_COVIDhub_ensemble = FALSE,
  include_COVIDhub_baseline = TRUE)

# Figure out what day it is; forecast creation date is set to a Monday,
# even if we are delayed and create it Tuesday morning.
forecast_week_end_date <- lubridate::floor_date(Sys.Date(), unit = "week") - 1
forecast_date <- forecast_week_end_date + 2

for(response_var in c('cum_death', 'inc_death', 'inc_case')) {
  if(response_var == 'cum_death') {
    do_q10_check <- do_nondecreasing_quantile_check <- TRUE
    required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
    spatial_resolution <- c('state', 'national')
    horizon <- 4L

    # adjustments based on plots
    if(forecast_date == '2020-06-08') {
      manual_eligibility_adjust <- c(
        "Auquan-SEIR", "CAN-SEIR_CAN", "CU-select", "UA-EpiCovDA", "SWC-TerminusCM"
      )
    } else if(forecast_date == '2020-06-15') {
      manual_eligibility_adjust <- "Auquan-SEIR"
    } else if(forecast_date == '2020-06-29') {
      manual_eligibility_adjust <- data.frame(
        model = c("epiforecasts-ensemble1", "NotreDame-mobility"),
        location = "34"
      )
    } else if(forecast_date == "2020-07-06") {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c("COVIDhub-baseline", "CU-select", "RobertWalraven-ESG", "USACE-ERDC_SEIR",
                  "MITCovAlliance-SIR"),
        location = fips_codes$location
      )
    } else if(forecast_date == '2020-07-13') {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c("COVIDhub-baseline", "RobertWalraven-ESG", "USACE-ERDC_SEIR", "MITCovAlliance-SIR"),
        location = fips_codes$location
      )
    } else if(forecast_date == '2020-07-20') {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c(
          "COVIDhub-baseline", "MITCovAlliance-SIR", "RobertWalraven-ESG", "USACE-ERDC_SEIR"),
        location = fips_codes$location
      )
    } else {
      manual_eligibility_adjust <- NULL
    }
  } else if(response_var == 'inc_death') {
    do_q10_check <- do_nondecreasing_quantile_check <- FALSE
    required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
    spatial_resolution <- c('state', 'national')
    horizon <- 4L

    # adjustments based on plots
    if(forecast_date == '2020-06-08') {
      manual_eligibility_adjust <- c(
        "CAN-SEIR_CAN", "SWC-TerminusCM", "USACE-ERDC_SEIR", "IHME-CurveFit"
      )
    } else if(forecast_date == '2020-06-15') {
      manual_eligibility_adjust <- c(
        "USACE-ERDC_SEIR", "LANL-GrowthRate"
      )
    } else if(forecast_date == '2020-06-29') {
      manual_eligibility_adjust <- bind_rows(
        data.frame(
          model = c("epiforecasts-ensemble1", "NotreDame-mobility"),
          location = "34",
          stringsAsFactors = FALSE
        ),
        data.frame(
          model = "CU-select",
          location = fips_codes$location,
          stringsAsFactors = FALSE
        )
      )
    } else if(forecast_date == "2020-07-06") {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c("COVIDhub-baseline", "CU-select", "RobertWalraven-ESG", "USACE-ERDC_SEIR",
                  "MITCovAlliance-SIR"),
        location = fips_codes$location
      )
    } else if(forecast_date == '2020-07-13') {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c("COVIDhub-baseline", "RobertWalraven-ESG", "USACE-ERDC_SEIR", "MITCovAlliance-SIR"),
        location = fips_codes$location
      )
    } else if(forecast_date == '2020-07-20') {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c(
          "COVIDhub-baseline", "MITCovAlliance-SIR", "MOBS-GLEAM_COVID",
          "RobertWalraven-ESG", "USACE-ERDC_SEIR"),
        location = fips_codes$location
      )
    } else {
      manual_eligibility_adjust <- NULL
    }
  } else if(response_var == 'inc_case') {
    do_q10_check <- do_nondecreasing_quantile_check <- FALSE
    required_quantiles <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
    spatial_resolution <- c('county', 'state', 'national')
    horizon <- 4L

    if(forecast_date == '2020-07-13') {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c("CU-select", "IowaStateLW-STEM", "JHU_IDD-CovidSP",
                  "LANL-GrowthRate", "RobertWalraven-ESG", "USACE-ERDC_SEIR",
                  "Covid19Sim-Simulator", "MIT_CovidAnalytics-DELPHI",
                  "CDDEP-SEIR"),
        location = covidData::fips_codes$location
      )
    } else if(forecast_date == '2020-07-20') {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c(
          "CDDEP-SEIR_MCMC", "Covid19Sim-Simulator", "CovidAnalytics-DELPHI",
          "CU-select", "IHME-CurveFit", "IowaStateLW-STEM", "JHU_IDD-CovidSP",
          "MITCovAlliance-SIR", "RobertWalraven-ESG", "USACE-ERDC_SEIR",
          "UVA-Ensemble"),
        location = covidData::fips_codes$location
      )
    } else {
      manual_eligibility_adjust <- NULL
    }
  }

  c(model_eligibility, wide_model_eligibility, location_groups, component_forecasts) %<-%
    build_covid_ensemble_from_local_files(
      candidate_model_abbreviations_to_include = candidate_model_abbreviations_to_include,
      spatial_resolution = spatial_resolution,
      targets = paste0(1:horizon, ' wk ahead ', gsub('_', ' ', response_var)),
      forecast_week_end_date = forecast_date - 2,
      timezero_window_size = 7,
      window_size = 0,
      intercept = FALSE,
      constraint = 'median',
      quantile_groups = rep(1, 23),
      missingness = 'by_location_group',
      backend = NA,
      submissions_root = submissions_root,
      required_quantiles = required_quantiles,
      do_q10_check = do_q10_check,
      do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
      do_baseline_check = FALSE,
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
      target_end_date = calc_target_week_end_date(
        forecast_week_end_date,
        as.integer(substr(target, 1, 1))),
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
  # model_weights <- purrr::pmap_dfr(
  #   location_groups %>% select(locations, qra_fit),
  #   function(locations, qra_fit) {
  #     temp <- qra_fit$coefficients %>%
  #       tidyr::pivot_wider(names_from = 'model', values_from = 'beta')
  #
  #     return(purrr::map_dfr(
  #       locations,
  #       function(location) {
  #         temp %>%
  #           mutate(location = location)
  #       }
  #     ))
  #   }
  # )
  # model_weights <- bind_cols(
  #   model_weights %>%
  #     select(location) %>%
  #     left_join(fips_codes, by = 'location'),
  #   model_weights %>% select(-location)
  # ) %>%
  #   arrange(location)
  # model_weights[is.na(model_weights)] <- 0.0

  if(response_var == 'cum_death') {
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
      write_csv(model_eligibility,
        paste0(save_dir,
          formatted_ensemble_predictions$forecast_date[1],
          '-',
          response_var,
          '-model-eligibility.csv'))

      write_csv(model_weights,
        paste0(save_dir,
          formatted_ensemble_predictions$forecast_date[1],
          '-',
          response_var,
          '-model-weights.csv'))
    }
  }
}


# make plots of ensemble submission
submissions_root <- paste0(root, 'data-processed/')

submission_dates <- forecast_date + seq(from = -6, to = 0)

# List of candidate models for inclusion in ensemble
candidate_model_abbreviations_to_include <- 'COVIDhub-ensemble'


for(model_abbr in candidate_model_abbreviations_to_include) {
  results_path <- paste0(submissions_root, model_abbr, '/',
                         submission_dates, '-', model_abbr, '.csv')
  results_path <- results_path[file.exists(results_path)]
  results_path <- tail(results_path, 1)

  if(length(results_path) == 0) {
    # no forecasts for this week
    next
  }

  model_forecast_date <- strsplit(results_path, split = '/')[[1]] %>%
    tail(1) %>%
    substr(1, 10)

  results <- readr::read_csv(
    results_path,
    col_types = cols(
      forecast_date = col_date(format = ""),
      target = col_character(),
      target_end_date = col_date(format = ""),
      location = col_character(),
      type = col_character(),
      quantile = col_double(),
      value = col_double()
    ))

  one_week_target_date <- results %>%
    dplyr::filter(grepl('^1 wk', target)) %>%
    dplyr::pull(target_end_date) %>%
    tail(1)
  if(!(one_week_target_date == (forecast_week_end_date + 7))) {
    # forecast file targets wrong week
    next
  }

  for(measure in c('deaths', 'cases')) {
    plot_path <- paste0(plots_root, model_abbr, '-', model_forecast_date, '-', measure, '.pdf')
    if(!file.exists(plot_path)) {
      if(measure == 'deaths') {
        data <- covidData::load_jhu_data(
          issue_date = as.character(forecast_week_end_date + 1),
          spatial_resolution = c('state', 'national'),
          temporal_resolution = 'weekly',
          measure = measure)
        horizon <- 4L
        types <- c('inc', 'cum')
        required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
      } else if(measure == 'cases') {
        data <- covidData::load_jhu_data(
          issue_date = as.character(forecast_week_end_date + 1),
          spatial_resolution = c('county', 'state', 'national'),
          temporal_resolution = 'weekly',
          measure = measure)
        horizon <- 4L
        types <- 'inc'
        required_quantiles <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
      }

      location_batches <- results %>%
        dplyr::filter(grepl(substr(measure, 1, nchar(measure) - 1), target)) %>%
        dplyr::distinct(location) %>%
        dplyr::arrange(nchar(location), location) %>%
        dplyr::mutate(
          location = factor(location, levels = location),
          batch = rep(seq_len(ceiling(nrow(.)/30)), each = 30)[seq_len(nrow(.))]
        )

      made_plots <- FALSE
      pdf(plot_path, width=24, height=14)

      for(batch_val in unique(location_batches$batch)) {
        print(batch_val)
        batch_locations <- location_batches$location[location_batches$batch == batch_val]
        plottable_predictions <- results %>%
          dplyr::filter(
            location %in% batch_locations,
            grepl(substr(measure, 1, nchar(measure) - 1), target)) %>%
          dplyr::mutate(
            endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
            alpha = ifelse(
              endpoint_type == 'lower',
              format(2*quantile, digits=3, nsmall=3),
              format(2*(1-quantile), digits=3, nsmall=3))
          ) %>%
          dplyr::filter(alpha != "1.000") %>%
          dplyr::select(-quantile) %>%
          tidyr::pivot_wider(names_from='endpoint_type', values_from='value')

        for(type in types) {
          type_intervals <- plottable_predictions %>%
            dplyr::filter(location %in% batch_locations) %>%
            filter(alpha != "1.000", grepl(UQ(type), target))

          if(nrow(type_intervals) > 0) {
            made_plots <- TRUE
            p <- ggplot() +
              geom_line(data=data %>%
                          dplyr::mutate(date = lubridate::ymd(date)) %>%
                          dplyr::filter(location %in% batch_locations),
                        mapping = aes_string(x = "date", y = type, group = "location")) +
              geom_point(data=data %>%
                           dplyr::mutate(date = lubridate::ymd(date)) %>%
                           dplyr::filter(location %in% batch_locations),
                         mapping = aes_string(x = "date", y = type, group = "location")) +
              geom_ribbon(
                data = type_intervals,
                mapping = aes(x = target_end_date,
                              ymin=lower, ymax=upper,
                              fill=alpha)) +
              geom_line(
                data = results %>% dplyr::filter(location %in% batch_locations) %>%
                  filter(quantile == 0.5,
                         grepl(UQ(type), target),
                         grepl(substr(measure, 1, nchar(measure) - 1), target)),
                mapping = aes(x = target_end_date, y = value)) +
              geom_point(
                data = results %>% dplyr::filter(location %in% batch_locations) %>%
                  filter(quantile == 0.5,
                         grepl(UQ(type), target),
                         grepl(substr(measure, 1, nchar(measure) - 1), target)),
                mapping = aes(x = target_end_date, y = value)) +
              facet_wrap(~location, ncol=6, scales = 'free_y') +
              ggtitle(paste(type, measure, as.character(forecast_week_end_date))) +
              theme_bw()
            print(p)
          }
        }
      }
      dev.off()
      if(!made_plots) {
        unlink(plot_path)
      }
    }
  }
}


# Upload to google drive
gdrive_plot_folders <- googledrive::drive_ls(
  path = googledrive::as_id("1lvEs1dHYANygB2EE-bHl1MIZyJbgMLr-"))
if(as.character(forecast_date) %in% gdrive_plot_folders$name) {
  gdrive_plots_root <- gdrive_plot_folders %>%
    dplyr::filter(name == as.character(forecast_date))
  existing_uploaded <- googledrive::drive_ls(path = gdrive_plots_root)$name
} else {
  gdrive_plots_root <- googledrive::drive_mkdir(
    name = as.character(forecast_date),
    path = googledrive::as_id("1lvEs1dHYANygB2EE-bHl1MIZyJbgMLr-"))
  existing_uploaded <- NULL
}

current_wd <- getwd()
setwd(plots_root)

plot_paths <- paste0('COVIDhub-ensemble-', model_forecast_date, '-',
  c("deaths", "cases"), '.pdf')
for(local_file in plot_paths) {
  if(!(local_file %in% existing_uploaded)) {
    googledrive::drive_put(
      media = local_file,
      path = gdrive_plots_root)
  }
}

setwd(current_wd)
