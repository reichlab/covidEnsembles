library(tidyverse)
library(zeallot)
library(covidEnsembles)
library(covidData)
library(covidHubUtils)
options(error = recover)

# Where to find component model submissions
hub_repo_path <- '../../../../covid19-forecast-hub/'

# where to save inc vs cum comparison
inc_vs_cum_path <- 'inc-vs-cum'
if (!dir.exists(inc_vs_cum_path)) {
  dir.create(inc_vs_cum_path)
}

# which models to examine
model_designations <- covidHubUtils::get_model_designations(
  source = "local_hub_repo",
  hub_repo_path = hub_repo_path)
candidate_model_abbreviations_to_include <- model_designations %>%
  dplyr::filter(designation %in% c("primary", "secondary")) %>%
  dplyr::pull(model)

# which date we're looking at for submissions
forecast_week_end_date <- lubridate::floor_date(Sys.Date(), unit = "week") - 1
forecast_date <- forecast_week_end_date + 2

# get the forecasts
all_forecasts <- covidHubUtils::load_forecasts(
  models = candidate_model_abbreviations_to_include,
  last_forecast_date = forecast_date,
  forecast_date_window_size = 6,
  targets = c(paste0(1:4, ' wk ahead inc death'), paste0(1:4, ' wk ahead cum death')),
  hub_repo_path = hub_repo_path
)

# subset to US
us_forecasts <- all_forecasts %>%
  dplyr::filter(location == 'US')

# For curiosity's sake, how many submissions per model?
#View(us_forecasts %>%
#  dplyr::filter(inc_cum == "cum", !is.na(value)) %>%
#  dplyr::count(model))

# what was the last observed value of cumulative deaths?
# used later for calculating "implied incident deaths forecast" based on
# cumulative deaths forecast
last_cum <- covidData::load_jhu_data(
  issue_date = as.character(forecast_week_end_date + 1),
  spatial_resolution = 'national',
  temporal_resolution = 'weekly',
  measure = 'deaths') %>%
  tail(1) %>%
  pull(cum)

# implied incident deaths predictive median
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

# actual incident deaths predictive median
actual <- us_forecasts %>%
  dplyr::filter(
    quantile == '0.5',
    inc_cum == "inc") %>%
  dplyr::transmute(
    model = model,
    horizon = horizon,
    actual_median_inc = value
  )

# put implied and actual together
implied_and_actual <- implied %>%
  dplyr::left_join(actual, by = c('model', 'horizon'))

# make a plot
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

# make a table
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
  readr::write_csv(
    path = file.path(
      inc_vs_cum_path,
      paste0("inc_vs_cum_", forecast_date, ".csv")
    )
  )
#  print(n = 17)
#  print(n = nrow(.))
