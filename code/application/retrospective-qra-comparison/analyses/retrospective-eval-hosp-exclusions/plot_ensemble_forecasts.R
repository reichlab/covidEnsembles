library(tidyverse)
library(zeallot)
library(covidData)
library(covidEnsembles)

# Location of main covid19-forecast-hub repo where component model submissions
# can be found
submissions_root <- paste0(
  "~/research/epi/covid/covidEnsembles/code/application/",
  "retrospective-qra-comparison/retrospective-forecasts/")

# Where we want to save the plots
plots_root <- paste0(
  "~/research/epi/covid/covidEnsembles/code/application/",
  "retrospective-qra-comparison/analyses/retrospective-eval-hosp-exclusions/")

# List of models to plot
model_abbrs <- c(
  "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-do_sd_check_exclude_none-drop_anomalies_FALSE-horizon_group_all",
  "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-do_sd_check_exclude_above-drop_anomalies_FALSE-horizon_group_all",
  "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-do_sd_check_exclude_below-drop_anomalies_FALSE-horizon_group_all",
  "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-do_sd_check_exclude_both-drop_anomalies_FALSE-horizon_group_all"
)

first_forecast_date <- lubridate::ymd("2020-11-23")
last_forecast_date <- lubridate::ymd("2021-07-05")
num_forecast_weeks <-
  as.numeric(last_forecast_date - first_forecast_date) / 7 + 1

all_locations <- covidData::fips_codes %>%
  dplyr::filter(nchar(location) == 2) %>%
  dplyr::pull(location)

spatial_scale <- "state_national"

models_to_keep <- model_abbrs

#for (measure in c('deaths', 'cases')) {
for(measure in 'hosps') {
#for(measure in 'cases') {
  # load forecasts
  all_forecasts <- load_retrospective_ensemble_forecasts(
    submissions_root = submissions_root,
    forecast_dates = lubridate::ymd(first_forecast_date) +
      seq(from = 0, length = num_forecast_weeks) * 7,
    all_locations,
    spatial_scales = spatial_scale,
    response_vars = paste0("inc_", substr(measure, 1, nchar(measure) - 1))
  )

  all_forecasts <- all_forecasts %>%
    dplyr::mutate(
      model_brief = dplyr::case_when(
        model == "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-do_sd_check_exclude_both-drop_anomalies_FALSE-horizon_group_all-estimation_scale_state_national" ~ "exclude_both",
        model == "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-do_sd_check_exclude_above-drop_anomalies_FALSE-horizon_group_all-estimation_scale_state_national" ~ "exclude_above",
        model == "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-do_sd_check_exclude_below-drop_anomalies_FALSE-horizon_group_all-estimation_scale_state_national" ~ "exclude_below",
        model == "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-do_sd_check_exclude_none-drop_anomalies_FALSE-horizon_group_all-estimation_scale_state_national" ~ "exclude_none"
      )
    )

    plot_path <- paste0(plots_root, 'forecast_comparison-', measure, '.pdf')
    if(!file.exists(plot_path)) {
      if(measure == 'deaths') {
        data <- covidData::load_jhu_data(
          issue_date = as.character(tail(covidData::jhu_deaths_data$issue_date, 1)),
          spatial_resolution = c('state', 'national'),
          temporal_resolution = 'weekly',
          measure = measure) %>%
          dplyr::left_join(fips_codes, by = 'location')

        # maximum horizon to plot
        horizon <- 4L
        types <- c('inc')
        required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
      } else if(measure == 'cases') {
        data <- covidData::load_jhu_data(
          issue_date = as.character(tail(covidData::jhu_deaths_data$issue_date, 1)),
          spatial_resolution = c('state', 'national'),
          temporal_resolution = 'weekly',
          measure = measure) %>%
          dplyr::left_join(fips_codes, by = 'location')

        # maximum horizon to plot
        horizon <- 4L
        types <- 'inc'
        required_quantiles <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
      } else if (measure == "hosps") {
        data <- covidData::load_data(
          spatial_resolution = c('state', 'national'),
          temporal_resolution = 'daily',
          measure = "hospitalizations") %>%
          dplyr::left_join(fips_codes, by = 'location')

        # maximum horizon to plot
        horizon <- 28L
        types <- 'inc'
        required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
      }

      results <- all_forecasts# %>%
#        dplyr::filter(target_end_date <= UQ(forecast_week_end_date) + 7 * horizon)

      location_batches <- results %>%
        dplyr::filter(grepl(substr(measure, 1, nchar(measure) - 1), target_variable)) %>%
        dplyr::distinct(location, location_name) %>%
        dplyr::arrange(nchar(location), location_name) %>%
        dplyr::mutate(
          location_name = factor(location_name, levels = location_name),
          batch = rep(seq_len(ceiling(nrow(.)/1)), each = 1)[seq_len(nrow(.))]
        )

#      data <- data %>% group_by(location) %>% top_n(12, wt = date)

      made_plots <- FALSE
      pdf(plot_path, width=24, height=14)

      n_groups <- 4
      forecast_date_groups <- data.frame(
        forecast_date = unique(results$forecast_date)
      ) %>%
        dplyr::mutate(
          forecast_date_group = paste0(
            "Forecast Date Group ",
            rep(1:n_groups, times = ceiling(nrow(.) / n_groups) )[seq_len(nrow(.))])
        )
      results <- results %>%
        dplyr::left_join(forecast_date_groups, by = "forecast_date")

      for(batch_val in unique(location_batches$batch)) {

        print(batch_val)
        batch_locations <- location_batches$location_name[location_batches$batch == batch_val]
        plottable_predictions <- results %>%
          dplyr::filter(
            location_name %in% batch_locations,
            grepl(substr(measure, 1, nchar(measure) - 1), target_variable)) %>%
          dplyr::mutate(
            endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
            alpha = ifelse(
              endpoint_type == 'lower',
              format(2*quantile, digits=3, nsmall=3),
              format(2*(1-quantile), digits=3, nsmall=3))
          ) %>%
#          dplyr::filter(alpha %in% c("0.050", "0.2")) %>%
          dplyr::filter(alpha != "1.000") %>%
          dplyr::select(-quantile) %>%
          tidyr::pivot_wider(names_from='endpoint_type', values_from='value')

        for(type in types) {
          type_intervals <- plottable_predictions %>%
            dplyr::filter(location_name %in% batch_locations) %>%
            filter(alpha != "1.000", grepl(UQ(type), target_variable))

          if(nrow(type_intervals) > 0) {
            made_plots <- TRUE
            p <- ggplot() +
              geom_ribbon(
                data = type_intervals,
                mapping = aes(
                  x = target_end_date,
                  ymin=lower, ymax=upper,
#                  fill=model_brief,
                  fill = alpha,
                  group = paste0(model_brief, forecast_date, alpha, sep = "_"))) + #,
                  #fill = "orange",
                  #alpha = 0.7) +
              # geom_line(
              #   data = results %>% dplyr::filter(location_name %in% batch_locations) %>%
              #     filter(quantile == 0.5,
              #            grepl(UQ(type), target_variable),
              #            grepl(substr(measure, 1, nchar(measure) - 1), target_variable)),
              #   mapping = aes(x = target_end_date, y = value,
              #    #color = model_brief,
              #     group = paste0(model_brief, forecast_date, sep = "_")),
              #   color = "blue") +
              # geom_point(
              #   data = results %>% dplyr::filter(location_name %in% batch_locations) %>%
              #     filter(quantile == 0.5,
              #            grepl(UQ(type), target_variable),
              #            grepl(substr(measure, 1, nchar(measure) - 1), target_variable)),
              #   mapping = aes(x = target_end_date, y = value),
              #   color = "blue") +
              geom_line(data=data %>%
                          dplyr::mutate(
                            date = lubridate::ymd(date)
                          ) %>%
                          dplyr::filter(location_name %in% batch_locations),
                        mapping = aes_string(x = "date", y = type)) +
              geom_point(data=data %>%
                          dplyr::mutate(
                            date = lubridate::ymd(date)
                          ) %>%
                          dplyr::filter(location_name %in% batch_locations),
                         mapping = aes_string(x = "date", y = type)) +
              facet_grid(forecast_date_group ~ model_brief) +
#              scale_y_log10() +
              scale_fill_viridis_d() +
#              scale_color_manual("Issue Date", values = c("black", "red")) +
#              scale_linetype_discrete("Issue Date") +
#              scale_shape_discrete("Issue Date") +
#              scale_fill_viridis_d("Interval alpha", begin = 0.2, end = 0.8) + 
              ggtitle(as.character(batch_locations)) +
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
