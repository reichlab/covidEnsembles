library(tidyverse)
library(zeallot)
library(covidData)

# Location of main covid19-forecast-hub repo where component model submissions
# can be found
submissions_root <- paste0(
  "~/research/epi/covid/covidEnsembles/code/application/",
  "retrospective-qra-comparison/retrospective-forecasts-state/")

# Where we want to save the plots
plots_root <- paste0(
  "~/research/epi/covid/covidEnsembles/code/application/",
  "retrospective-qra-comparison/retrospective-plots/")

# Figure out what day it is.
# forecast_week_end_date is a saturday relative to which week-ahead targets are
# defined. forecast_date is the monday of forecast submission
#forecast_week_end_date <- lubridate::ymd("2020-10-03")
forecast_week_end_date <- lubridate::ymd("2020-11-14") + 0
forecast_date <- forecast_week_end_date + 2

# List of models to plot
model_abbrs <- list.dirs(submissions_root, full.names = FALSE)
model_abbrs <- model_abbrs[nchar(model_abbrs) > 0]

# model_abbrs <- c(
#   "intercept_FALSE-combine_method_ew-missingness_by_location_group-quantile_groups_per_model-window_size_0-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE",
#   "intercept_FALSE-combine_method_median-missingness_by_location_group-quantile_groups_per_model-window_size_0-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE",
#   "intercept_FALSE-combine_method_convex-missingness_impute-quantile_groups_3_groups-window_size_4-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE",
#   "intercept_TRUE-combine_method_positive-missingness_impute-quantile_groups_3_groups-window_size_4-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE"
# )

#model_abbrs <- "intercept_FALSE-combine_method_ew-missingness_by_location_group-quantile_groups_per_model-window_size_0-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE"
model_abbrs <- "intercept_FALSE-combine_method_median-missingness_by_location_group-quantile_groups_per_model-window_size_0-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE"

for (model_abbr in model_abbrs) {
  plot_forecasts_single_model(
    submissions_root = submissions_root,
    plots_root = plots_root,
    forecast_date = forecast_date,
    model_abbrs = model_abbr,
    target_variables = c("cases", "deaths", "hospitalizations")
  )
}
#   # Find a submission file for this model abbreviation
#   results_path <- Sys.glob(paste0(submissions_root, model_abbr, "/*",
#     submission_dates, "-", model_abbr, ".csv"))

#   if (length(results_path) == 0) {
#     # no forecasts for this week
#     next
#   }

#   results <- purrr::map_dfr(
#     results_path,
#     readr::read_csv,
#     col_types = cols(
#       forecast_date = col_date(format = ""),
#       target = col_character(),
#       target_end_date = col_date(format = ""),
#       location = col_character(),
#       type = col_character(),
#       quantile = col_double(),
#       value = col_double()
#     )) %>%
#     dplyr::left_join(fips_codes, by = 'location')

#   # one_week_target_date <- results %>%
#   #   dplyr::filter(grepl('^1 wk', target)) %>%
#   #   dplyr::pull(target_end_date) %>%
#   #   tail(1)
#   # if(!(one_week_target_date == (forecast_week_end_date + 7))) {
#   #   # forecast file targets wrong week
#   #   next
#   # }

# #  for(measure in c('deaths', 'cases', 'hospitalizations')) {
# #  for(measure in 'deaths') {
# #  for(measure in 'cases') {
#   for(measure in 'hospitalizations') {
#     plot_path <- paste0(day_plots_root, model_abbr, '-', forecast_date, '-', measure, '.pdf')
#     if(!file.exists(plot_path)) {
#       if(measure == 'deaths') {
#         data <- dplyr::bind_rows(
#           covidData::load_jhu_data(
#             issue_date = as.character(forecast_week_end_date + 1),
#             spatial_resolution = c('state', 'national'),
#             temporal_resolution = 'weekly',
#             measure = measure) %>%
#             dplyr::left_join(fips_codes, by = 'location') %>%
#             dplyr::mutate(issue_date = as.character(forecast_week_end_date + 1)),
#           covidData::load_jhu_data(
#             issue_date = as.character(tail(covidData::jhu_deaths_data$issue_date, 1)),
#             spatial_resolution = c('state', 'national'),
#             temporal_resolution = 'weekly',
#             measure = measure) %>%
#             dplyr::left_join(fips_codes, by = 'location') %>%
#             dplyr::mutate(issue_date = as.character(tail(covidData::jhu_deaths_data$issue_date, 1)))
#         )

#         # maximum horizon to plot
#         horizon <- 4L
#         types <- c('inc', 'cum')
#         required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
#         target_measure <- "death"
#         days_per_target_unit <- 7L
#       } else if(measure == 'cases') {
#         data <- dplyr::bind_rows(
#           covidData::load_jhu_data(
#             issue_date = as.character(forecast_week_end_date + 1),
#             spatial_resolution = c('county', 'state', 'national'),
#             temporal_resolution = 'weekly',
#             measure = measure) %>%
#             dplyr::left_join(fips_codes, by = 'location') %>%
#             dplyr::mutate(issue_date = as.character(forecast_week_end_date + 1)),
#           covidData::load_jhu_data(
#             issue_date = as.character(tail(covidData::jhu_cases_data$issue_date, 1)),
#             spatial_resolution = c('county', 'state', 'national'),
#             temporal_resolution = 'weekly',
#             measure = measure) %>%
#             dplyr::left_join(fips_codes, by = 'location') %>%
#             mutate(issue_date = as.character(tail(covidData::jhu_cases_data$issue_date, 1))),
#         )

#         # maximum horizon to plot
#         horizon <- 8L
#         types <- 'inc'
#         required_quantiles <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
#         target_measure <- "case"
#         days_per_target_unit <- 7L
#       } else if(measure == 'hospitalizations') {
#         data <- dplyr::bind_rows(
#           covidData::load_healthdata_data(
#             issue_date = as.character(forecast_week_end_date + 1),
#             spatial_resolution = c('county', 'state', 'national'),
#             temporal_resolution = 'daily',
#             measure = measure) %>%
#             dplyr::left_join(fips_codes, by = 'location') %>%
#             dplyr::mutate(issue_date = as.character(forecast_week_end_date + 1)),
#           covidData::load_healthdata_data(
#             issue_date = as.character(tail(covidData::healthdata_hosp_data$issue_date, 1)),
#             spatial_resolution = c('county', 'state', 'national'),
#             temporal_resolution = 'daily',
#             measure = measure) %>%
#             dplyr::left_join(fips_codes, by = 'location') %>%
#             mutate(issue_date = as.character(tail(covidData::healthdata_hosp_data$issue_date, 1))),
#         )

#         # maximum horizon to plot
#         horizon <- 28L
#         types <- 'inc'
#         required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
#         target_measure <- "hosp"
#         days_per_target_unit <- 1L
#         forecast_week_end_date <- forecast_week_end_date + 2L
#       }

#       data <- data %>%
#         dplyr::filter(date >= forecast_week_end_date - 3 * horizon)

#       results <- results %>%
#         dplyr::filter(target_end_date <= UQ(forecast_week_end_date) + days_per_target_unit * horizon)

#       location_batches <- results %>%
#         dplyr::filter(grepl(target_measure, target)) %>%
#         dplyr::distinct(location, location_name) %>%
#         dplyr::arrange(nchar(location), location_name) %>%
#         dplyr::mutate(
#           location_name = factor(location_name, levels = location_name),
#           batch = rep(seq_len(ceiling(nrow(.)/30)), each = 30)[seq_len(nrow(.))]
#         )

#       made_plots <- FALSE
#       pdf(plot_path, width=24, height=14)

#       for(batch_val in unique(location_batches$batch)) {
#         print(batch_val)
#         batch_locations <- location_batches$location_name[location_batches$batch == batch_val]
#         plottable_predictions <- results %>%
#           dplyr::filter(
#             location_name %in% batch_locations,
#             grepl(target_measure, target)) %>%
#           dplyr::mutate(
#             endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
#             alpha = ifelse(
#               endpoint_type == 'lower',
#               format(2*quantile, digits=3, nsmall=3),
#               format(2*(1-quantile), digits=3, nsmall=3))
#           ) %>%
#           dplyr::filter(alpha != "1.000") %>%
#           dplyr::select(-quantile) %>%
#           tidyr::pivot_wider(names_from='endpoint_type', values_from='value')

#         for(type in types) {
#           type_intervals <- plottable_predictions %>%
#             dplyr::filter(location_name %in% batch_locations) %>%
#             filter(alpha != "1.000", grepl(UQ(type), target))

#           if(nrow(type_intervals) > 0) {
#             made_plots <- TRUE
#             p <- ggplot() +
#               geom_ribbon(
#                 data = type_intervals,
#                 mapping = aes(
#                   x = target_end_date,
#                   ymin=lower, ymax=upper,
#                   fill=alpha)) +
#               geom_line(
#                 data = results %>% dplyr::filter(location_name %in% batch_locations) %>%
#                   filter(quantile == 0.5,
#                          grepl(UQ(type), target),
#                          grepl(target_measure, target)),
#                 mapping = aes(x = target_end_date, y = value),
#                 color = "green") +
#               geom_point(
#                 data = results %>% dplyr::filter(location_name %in% batch_locations) %>%
#                   filter(quantile == 0.5,
#                          grepl(UQ(type), target),
#                          grepl(target_measure, target)),
#                 mapping = aes(x = target_end_date, y = value),
#                 color = "green") +
#               geom_line(data=data %>%
#                           dplyr::mutate(
#                             date = lubridate::ymd(date),
#                             group_factor = paste0(issue_date, location_name)
#                           ) %>%
#                           dplyr::filter(location_name %in% batch_locations),
#                         mapping = aes_string(x = "date", y = type, color = "issue_date", linetype = "issue_date", group = "group_factor")) +
#               geom_point(data=data %>%
#                           dplyr::mutate(
#                             date = lubridate::ymd(date),
#                             group_factor = paste0(issue_date, location_name)
#                           ) %>%
#                           dplyr::filter(location_name %in% batch_locations),
#                          mapping = aes_string(x = "date", y = type, color = "issue_date", shape = "issue_date", group = "group_factor")) +
#               facet_wrap(~location_name, ncol=6, scales = 'free_y') +
#               scale_color_manual("Issue Date", values = c("black", "red")) +
#               scale_linetype_discrete("Issue Date") +
#               scale_shape_discrete("Issue Date") +
#               scale_fill_viridis_d("Interval alpha", begin = 0.2, end = 0.8) +
#               ggtitle(paste(type, measure, as.character(forecast_week_end_date))) +
#               theme_bw()
#             print(p)
#           }
#         }
#       }
#       dev.off()
#       if (!made_plots) {
#         unlink(plot_path)
#       }
#     }
#   }
# }
