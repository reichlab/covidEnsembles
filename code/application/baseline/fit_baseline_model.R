library(tidyverse)
library(covidEnsembles)
library(covidModels)

forecast_week_end_dates <- as.character(
  lubridate::ymd('2020-05-09') + seq(from = -5, length = 12)*7)

for(forecast_week_end_date in forecast_week_end_dates) {
  data <- covidEnsembles::historical_truths(
    issue_date = as.character(lubridate::ymd(forecast_week_end_date)+1),
    spatial_resolution = c('state', 'national'),
    temporal_resolution = 'weekly'
  )# %>%
    #dplyr::filter(!(location %in% c('56', '60', '78'))) # American Samoa has 0 cases

  results <- purrr::map_dfr(
    unique(data$location),
    function(location) {
      location_data <- data %>%
        dplyr::filter(location == UQ(location))

      if(all(location_data$cum_deaths == 0)) {
        start_date <- lubridate::ymd(min(location_data$date)) + 7
      } else {
        start_date <- lubridate::ymd(min(location_data$date[location_data$cum_deaths > 0])) - 7
      }
      location_data <- location_data %>%
        dplyr::filter(date >= as.character(start_date))

      baseline_fit <- covidModels::fit_quantile_baseline(location_data$inc_deaths)

      predict(
        baseline_fit,
        inc_data = location_data$inc_deaths,
        cum_data = location_data$cum_deaths,
        quantiles = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
        horizon = 4,
        num_samples = 100000
      ) %>%
        dplyr::mutate(location = location) %>%
        dplyr::select(location, target, quantile, value)
    })

  saveRDS(
    results,
    file = paste0('code/application/baseline/baseline-fits/',
                  forecast_week_end_date, '-baseline.rds')
  )
}


# assemble predictions into an object in the package
baseline_pred_files <- Sys.glob('code/application/baseline/baseline-fits/*.rds')

baseline_forecasts <- dplyr::tibble(
  forecast_week_end_date = purrr::map_chr(
    baseline_pred_files,
    function(file) {
      substr(tail(strsplit(file, '/')[[1]], 1), 1, 10)
    }),
  forecasts = purrr::map(
    baseline_pred_files,
    function(file) {
      forecast_week_end_date <- substr(tail(strsplit(file, '/')[[1]], 1), 1, 10)
      forecast_df <- readRDS(file) %>%
        dplyr::mutate(
          model = 'baseline',
          forecast_week_end_date = forecast_week_end_date)
    }
  )
)

save(baseline_forecasts, file = "data/baseline_forecasts.rdata")


baseline_scores <- purrr::map_dfr(
  baseline_pred_files,
  function(file) {
    forecast_week_end_date <- substr(file, 15, 24)
    forecast_df <- readRDS(file) %>%
      dplyr::mutate(
        model = 'baseline',
        forecast_week_end_date = forecast_week_end_date)
    qfm <- covidEnsembles::new_QuantileForecastMatrix_from_df(
      forecast_df = forecast_df,
      model_col = 'model',
      id_cols = c('location', 'forecast_week_end_date', 'target'),
      quantile_name_col = 'quantile',
      quantile_value_col = 'value',
      drop_missing_id_levels = TRUE
    )

    get_all_wis_components(
      qfm = qfm,
      observed_by_location_target_end_date = observed_by_location_target_end_date)
  }
)


pdf('code/application/baseline/baseline-fits/plots.pdf',
    width=24, height=60)
for(file in baseline_pred_files) {
  forecast_week_end_date <- substr(tail(strsplit(file, '/')[[1]], 1), 1, 10)
  forecast_df <- readRDS(file) %>%
    dplyr::mutate(
      model = 'baseline',
      forecast_week_end_date = forecast_week_end_date)

  data <- covidEnsembles::historical_truths(
    issue_date = as.character(lubridate::ymd(forecast_week_end_date)+1),
    spatial_resolution = c('state', 'national'),
    temporal_resolution = 'weekly'
  )

  plottable_predictions <- forecast_df %>%
    #  filter(quantile != 0.5) %>%
    mutate(
      endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
      alpha = ifelse(
        endpoint_type == 'lower',
        format(2*quantile, digits=3, nsmall=3),
        format(2*(1-quantile), digits=3, nsmall=3))
    ) %>%
    select(-quantile) %>%
    tidyr::pivot_wider(names_from='endpoint_type', values_from='value')

  locations <- unique(data$location)
#  locations <- c('05', '10')

  p <- ggplot() +
    geom_line(data=data %>% dplyr::filter(location %in% locations),
              mapping = aes(x = lubridate::ymd(date), y = inc_deaths, group = location_name)) +
    geom_point(data=data %>% dplyr::filter(location %in% locations),
              mapping = aes(x = lubridate::ymd(date), y = inc_deaths, group = location_name)) +
    geom_ribbon(
      data = plottable_predictions %>% dplyr::filter(location %in% locations) %>%
        filter(alpha != "1.000", grepl('inc', target)) %>%
        mutate(
          horizon = as.integer(substr(target, 1, 1)),
          target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon),
      mapping = aes(x = target_end_date,
                    ymin=lower, ymax=upper,
                    fill=alpha)) +
    geom_line(
      data = forecast_df %>% dplyr::filter(location %in% locations) %>%
        filter(quantile == 0.5, grepl('inc', target)) %>%
        mutate(
          horizon = as.integer(substr(target, 1, 1)),
          target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon),
      mapping = aes(x = target_end_date, y = value)) +
    geom_point(
      data = forecast_df %>% dplyr::filter(location %in% locations) %>%
        filter(quantile == 0.5, grepl('inc', target)) %>%
        mutate(
          horizon = as.integer(substr(target, 1, 1)),
          target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon),
      mapping = aes(x = target_end_date, y = value)) +
    facet_wrap(~location, ncol=4, scales = 'free_y') +
    ggtitle(paste0('inc deaths', forecast_week_end_date)) +
    theme_bw()
  print(p)


  p <- ggplot() +
    geom_line(data=data, mapping = aes(x = lubridate::ymd(date), y = cum_deaths, group = location_name)) +
    geom_point(data=data, mapping = aes(x = lubridate::ymd(date), y = cum_deaths, group = location_name)) +
    geom_ribbon(
      data = plottable_predictions %>%
        filter(alpha != "1.000", grepl('cum', target)) %>%
        mutate(
          horizon = as.integer(substr(target, 1, 1)),
          target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon),
      mapping = aes(x = target_end_date,
                    ymin=lower, ymax=upper,
                    fill=alpha)) +
    geom_line(
      data = forecast_df %>%
        filter(quantile == 0.5, grepl('cum', target)) %>%
        mutate(
          horizon = as.integer(substr(target, 1, 1)),
          target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon),
      mapping = aes(x = target_end_date, y = value)) +
    geom_point(
      data = forecast_df %>%
        filter(quantile == 0.5, grepl('cum', target)) %>%
        mutate(
          horizon = as.integer(substr(target, 1, 1)),
          target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon),
      mapping = aes(x = target_end_date, y = value)) +
    facet_wrap(~location, ncol=4, scales = 'free_y') +
    ggtitle(paste0('cum deaths', forecast_week_end_date)) +
    theme_bw()
  print(p)
}
dev.off()


