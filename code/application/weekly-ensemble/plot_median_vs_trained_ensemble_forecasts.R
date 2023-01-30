library(tidyverse)
library(zeallot)
library(covidData)
library(here)
setwd(here())

# Location of main covid19-forecast-hub repo where component model submissions
# can be found
submissions_root <- "code/application/weekly-ensemble/forecasts/data-processed/"

# Where we want to save the plots
plots_root <- "code/application/weekly-ensemble/plots/"

# Figure out what day it is.
# forecast_week_end_date is a saturday relative to which week-ahead targets are
# defined. forecast_date is the monday of forecast submission
forecast_date <- lubridate::floor_date(Sys.Date(), unit = "week", week_start = 1)

day_plots_root <- paste0(plots_root, forecast_date, '/')
if(!file.exists(day_plots_root)) {
  dir.create(day_plots_root)
}

submission_dates <- forecast_date + seq(from = -6, to = 0)

# List of models to plot
# model_abbrs <- list.dirs(submissions_root, full.names = FALSE)
# model_abbrs <- model_abbrs[nchar(model_abbrs) > 0]

# Get 2 week out of plot
model_abbrs <-  c("COVIDhub-4_week_ensemble", "COVIDhub-trained_ensemble")

all_forecasts <- purrr::map_dfr(
  model_abbrs,
  function(model_abbr) {
    # Find a submission file for this model abbreviation
    results_path <- Sys.glob(paste0(submissions_root, model_abbr, "/*",
      submission_dates, "-", model_abbr, ".csv"))

    if (length(results_path) == 0) {
      # no forecasts for this week
      return(NULL)
    }

    results <- purrr::map_dfr(
      results_path,
      readr::read_csv,
      col_types = cols(
        forecast_date = col_date(format = ""),
        target = col_character(),
        target_end_date = col_date(format = ""),
        location = col_character(),
        type = col_character(),
        quantile = col_double(),
        value = col_double()
      )) %>%
      dplyr::left_join(fips_codes, by = 'location') %>%
      dplyr::mutate(model = model_abbr)

    # one_week_target_date <- results %>%
    #   dplyr::filter(grepl('^1 wk', target)) %>%
    #   dplyr::pull(target_end_date) %>%
    #   tail(1)
    # if(!(one_week_target_date == (forecast_week_end_date + 7))) {
    #   # forecast file targets wrong week
    #   return(NULL)
    # }

    return(results)
  })


# removing 'cases' 23-01-30
for(measure in c('deaths', 'hospitalizations')) {
#for(measure in c('hospitalizations')) {
  if (measure == "deaths") {
    target_variable_short <- "death"
  } else if (measure == "cases") {
    target_variable_short <- "case"
  } else if (measure == "hospitalizations") {
    target_variable_short <- "hosp"
  }

#  for(measure in 'deaths') {
#  for(measure in 'cases') {
    plot_path <- paste0(day_plots_root, 'forecast_comparison-', forecast_date, '-', measure, '.pdf')
    if (!file.exists(plot_path)) {
      if (measure == 'deaths') {
        data <- dplyr::bind_rows(
          covidData::load_jhu_data(
            issue_date = as.character(forecast_date - 1),
            spatial_resolution = c('state', 'national'),
            temporal_resolution = 'weekly',
            measure = measure) %>%
            dplyr::left_join(fips_codes, by = 'location') %>%
            dplyr::mutate(issue_date = as.character(forecast_date - 1)),
          covidData::load_jhu_data(
            issue_date = as.character(covidData::available_issue_dates("deaths") %>% max()),
            spatial_resolution = c('state', 'national'),
            temporal_resolution = 'weekly',
            measure = measure) %>%
            dplyr::left_join(fips_codes, by = 'location') %>%
            dplyr::mutate(issue_date = as.character(covidData::available_issue_dates("deaths") %>% max()))
        )

        # maximum horizon to plot
        horizon <- 4L
        types <- c('inc', 'cum')
        required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
        num_obs_per_week <- 1
      } else if (measure == 'cases') {
        data <- dplyr::bind_rows(
          covidData::load_jhu_data(
            issue_date = as.character(forecast_date - 1),
            spatial_resolution = c('county', 'state', 'national'),
            temporal_resolution = 'weekly',
            measure = measure) %>%
            dplyr::left_join(fips_codes, by = 'location') %>%
            dplyr::mutate(issue_date = as.character(forecast_date - 1)),
          covidData::load_jhu_data(
            issue_date = as.character(covidData::available_issue_dates("cases") %>% max()),
            spatial_resolution = c('county', 'state', 'national'),
            temporal_resolution = 'weekly',
            measure = measure) %>%
            dplyr::left_join(fips_codes, by = 'location') %>%
            mutate(issue_date = as.character(covidData::available_issue_dates("cases") %>% max())),
        )

        # maximum horizon to plot
        horizon <- 8L
        types <- 'inc'
        required_quantiles <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
        num_obs_per_week <- 1
      } else if (measure == 'hospitalizations') {
        data <- covidData::load_data(
            as_of = as.character(covidData::available_issue_dates("hospitalizations") %>% max()),
            spatial_resolution = c('state', 'national'),
            temporal_resolution = 'daily',
            measure = measure) %>%
          dplyr::left_join(fips_codes, by = 'location') %>%
          dplyr::mutate(issue_date = as.character(covidData::available_issue_dates("hospitalizations") %>% max()))

        horizon <- 28L
        types <- c('inc')
        required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
        num_obs_per_week <- 7
      }

      results <- all_forecasts

      location_batches <- results %>%
        dplyr::filter(grepl(target_variable_short, target)) %>%
        dplyr::distinct(location, location_name_with_state) %>%
        dplyr::arrange(nchar(location), 
            sub(pattern = ".*,\\s", "", location_name_with_state),
            location_name_with_state) %>%
        dplyr::mutate(
          location_name = factor(location_name_with_state, levels = location_name_with_state),
          batch = rep(seq_len(ceiling(nrow(.)/30)), each = 30)[seq_len(nrow(.))]
        )

      data <- data %>% group_by(location) %>% top_n(12 * num_obs_per_week, wt = date)

      made_plots <- FALSE
      pdf(plot_path, width=24, height=14)

      for(batch_val in unique(location_batches$batch)) {
        print(batch_val)
        batch_locations <- location_batches$location_name_with_state[location_batches$batch == batch_val]
        plottable_predictions <- results %>%
          dplyr::filter(
            location_name_with_state %in% batch_locations,
            grepl(target_variable_short, target)) %>%
          dplyr::mutate(
            endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
            alpha = ifelse(
              endpoint_type == 'lower',
              format(2*quantile, digits=3, nsmall=3),
              format(2*(1-quantile), digits=3, nsmall=3))
          ) %>%
          dplyr::filter(alpha == "0.050") %>%
#          dplyr::filter(alpha != "1.000") %>%
          dplyr::select(-quantile) %>%
          tidyr::pivot_wider(names_from='endpoint_type', values_from='value') # %>%
          # dplyr::mutate(
          #   model = dplyr::case_when(
          #     model == "ensemble_switching-criteria_min_wis-comparison_window_2" ~ "switching",
          #     model == "intercept_FALSE-combine_method_convex-missingness_impute-quantile_groups_3_groups-window_size_4-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "convex",
          #     model == "intercept_FALSE-combine_method_median-missingness_by_location_group-quantile_groups_per_model-window_size_0-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "median"
          #   )
          # )

        for(type in types) {
          type_intervals <- plottable_predictions %>%
            dplyr::filter(location_name_with_state %in% batch_locations) %>%
            filter(alpha != "1.000", grepl(UQ(type), target))

          if (nrow(type_intervals) > 0) {
            made_plots <- TRUE
            p <- ggplot() +
              geom_ribbon(
                data = type_intervals,
                mapping = aes(
                  x = target_end_date,
                  ymin=lower, ymax=upper,
                  fill=model), alpha = 0.4) +
              geom_line(
                data = results %>% dplyr::filter(location_name_with_state %in% batch_locations) %>%
                  filter(quantile == 0.5,
                         grepl(UQ(type), target),
                         grepl(target_variable_short, target)),
                mapping = aes(x = target_end_date, y = value, color = model)) +
              geom_point(
                data = results %>% dplyr::filter(location_name_with_state %in% batch_locations) %>%
                  filter(quantile == 0.5,
                         grepl(UQ(type), target),
                         grepl(target_variable_short, target)),
                mapping = aes(x = target_end_date, y = value, color = model, shape = model)) +
              geom_line(data=data %>%
                          dplyr::mutate(
                            date = lubridate::ymd(date),
                            group_factor = paste0(issue_date, location_name_with_state)
                          ) %>%
                          dplyr::filter(location_name_with_state %in% batch_locations, issue_date == max(issue_date)),
                        mapping = aes_string(x = "date", y = type, group = "group_factor")) +
              geom_point(data=data %>%
                          dplyr::mutate(
                            date = lubridate::ymd(date),
                            group_factor = paste0(issue_date, location_name_with_state)
                          ) %>%
                          dplyr::filter(location_name_with_state %in% batch_locations, issue_date == max(issue_date)),
                         mapping = aes_string(x = "date", y = type, group = "group_factor")) +
              facet_wrap(~location_name_with_state, ncol=6, scales = 'free_y') +
#              scale_color_manual("Issue Date", values = c("black", "red")) +
#              scale_linetype_discrete("Issue Date") +
#              scale_shape_discrete("Issue Date") +
#              scale_fill_viridis_d("Interval alpha", begin = 0.2, end = 0.8) + 
              ggtitle(paste(type, measure, as.character(forecast_date))) +
              theme_bw()
            print(p)
          }
        }
      }
      dev.off()
      if (!made_plots) {
        unlink(plot_path)
      }
    }
  }
