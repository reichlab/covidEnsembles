library(tidyverse)
library(zeallot)
library(covidEnsembles)
library(covidData)
library(covidHubUtils)
options(error = recover)

# Where to find component model submissions
hub_repo_path <- '../covid19-forecast-hub'

submissions_root <- '../covid19-forecast-hub/data-processed/'

required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

model_designations <- covidHubUtils::get_model_designations(
  source = "local_hub_repo",
  hub_repo_path = hub_repo_path)
candidate_model_abbreviations_to_include <- model_designations %>%
  dplyr::filter(designation %in% c("primary", "secondary")) %>%
  dplyr::pull(model)

forecast_week_end_date <- lubridate::floor_date(Sys.Date(), unit = "week") - 1
forecast_date <- forecast_week_end_date + 2

all_forecasts <- covidHubUtils::load_forecasts(
  models = candidate_model_abbreviations_to_include,
  last_forecast_date = forecast_date,
  forecast_date_window_size = 6,
  targets = c(paste0(1:4, ' wk ahead inc death'), paste0(1:4, ' wk ahead cum death')),
  hub_repo_path = hub_repo_path
)

us_forecasts <- all_forecasts %>%
  dplyr::filter(location == 'US')

View(us_forecasts %>%
  dplyr::filter(inc_cum == "cum", !is.na(value)) %>%
  dplyr::count(model))

last_cum <- covidData::load_jhu_data(
  issue_date = as.character(forecast_week_end_date + 1),
  spatial_resolution = 'national',
  temporal_resolution = 'weekly',
  measure = 'deaths') %>%
  tail(1) %>%
  pull(cum)


state_data <- covidData::load_jhu_data(
  issue_date = as.character(forecast_week_end_date + 1),
  spatial_resolution = 'state',
  temporal_resolution = 'weekly',
  measure = 'deaths')

implied <- us_forecasts %>%
  dplyr::filter(
    quantile == '0.5',
    inc_cum == "cum") %>%
  dplyr::group_by(model) %>%
  dplyr::arrange(model, horizon) %>%
  dplyr::mutate(
    implied_median_inc =
      ifelse(
        is.na(dplyr::lag(value, 1)),
        value - last_cum,
        value - dplyr::lag(value, 1))
  ) %>%
  dplyr::select(model, horizon, implied_median_inc)

actual <- us_forecasts %>%
  dplyr::filter(
    quantile == '0.5',
    inc_cum == "inc") %>%
  dplyr::transmute(
    model = model,
    horizon = horizon,
    actual_median_inc = value
  )

implied_and_actual <- implied %>%
  dplyr::left_join(actual, by = c('model', 'horizon'))

ggplot(
  data = implied_and_actual %>%
    dplyr::filter(!is.na(implied_median_inc), !is.na(actual_median_inc)),
  mapping = aes(x = actual_median_inc,
                y = implied_median_inc,
                color = model,
                shape = factor(horizon))) +
  geom_point() +
  geom_line(mapping = aes(group = model)) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()

implied_and_actual %>%
  dplyr::mutate(
    observed_cum = last_cum,
    diff = implied_median_inc - actual_median_inc,
    diff = ifelse(abs(diff) < sqrt(.Machine$double.eps), 0.0, diff)
  ) %>%
  dplyr::left_join(
    us_forecasts %>%
      dplyr::filter(
        quantile == "0.5",
        inc_cum == "cum"
      ) %>%
      dplyr::select(
        model, horizon, value
      ),
    by = c("model", "horizon")
  ) %>%
  dplyr::arrange(desc(abs(diff))) %>%
  dplyr::filter(horizon == 1) %>%
  dplyr::select(
    model,
    observed_cum,
    one_week_ahead_cum_forecast = value,
    implied_median_inc,
    actual_median_inc,
    diff
  ) %>%
#  slice(8)
#  print(n = 17)
  print(n = nrow(.))
