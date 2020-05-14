library(zoltr)
library(tidyverse)
library(covidEnsembles)
options(warn=2, error=recover)

zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

all_truths <- truth(zoltar_connection, 'https://www.zoltardata.com/api/project/44/') %>%
  transmute(
    timezero = timezero,
    unit = unit,
    target = target,
    observed = value
  )

# keep only quantile forecasts for requested quantiles
# that were in the last submission from a given model for each forecast week
# end date
required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

all_forecasts_and_observed <- readRDS('./data/all_covid19_forecasts.rds') %>%
  dplyr::filter(
    model_name != 'ensemble',
    class == 'quantile',
    quantile %in% required_quantiles) %>%
  mutate(
    value = as.numeric(value)
  ) %>%
  dplyr::select(unit, timezero, target, model_id, model_name, quantile, value) %>%
  tidyr::pivot_wider(names_from = quantile, values_from = value) %>%
  dplyr::left_join(all_truths, by = c('timezero', 'unit', 'target')) %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 1)),
    forecast_week_end_date = calc_forecast_week_end_date(timezero),
    target_end_date = calc_target_week_end_date(timezero, horizon)
  ) %>%
  dplyr::group_by(
    unit, target, forecast_week_end_date, model_id
  ) %>%
  dplyr::top_n(1, timezero) %>%
  tidyr::pivot_longer(
    cols = all_of(as.character(required_quantiles)),
    names_to = 'quantile',
    values_to = 'value') %>%
  ungroup()

## Consider adding a filter to the above like
## lubridate::ymd(forecast_week_end_date) - lubridate::ymd(timezero) <= 3,
## possibly or forecast_week_end_date < max(forecast_week_end_date)

observed_by_unit_target_end_date <- all_forecasts_and_observed %>%
  dplyr::distinct(unit, target_end_date, observed)

forecast_matrix <- new_QuantileForecastMatrix_from_df(
  forecast_df = all_forecasts_and_observed,
  model_col = 'model_id',
  id_cols = c('unit', 'forecast_week_end_date', 'target'),
  quantile_name_col = 'quantile',
  quantile_value_col = 'value'
)

model_eligibility <- calc_model_eligibility_for_ensemble(
  qfm = forecast_matrix,
  observed_by_unit_target_end_date = observed_by_unit_target_end_date,
  lookback_length = 0,
  model_id_name = 'model_id'
)




  all_forecasts %>%
  group_by(unit, forecast_week_end_date, model_id, model_name) %>%
  summarize(
    any_missing = any(is.na(.[, as.character(required_quantiles)]))
  )


all_end_dates <- sort(unique(all_forecasts$forecast_week_end_date))
for(end_date in all_end_dates[5:length(all_end_dates)]) {
  # subset to this end date
  forecasts_this_date <- all_forecasts %>%
    filter(forecast_week_end_date == end_date)

  # for each location, keep only models with forecasts for all quantiles and
  # all four horizons
  all_present <- expand.grid(
    unit = unique(forecasts_this_date$unit),
    target = unique(forecasts_this_date$target),
    model_name = unique(forecasts_this_date$model_name),
    stringsAsFactors = FALSE
  )
  all_present$all_present <- purrr::pmap_lgl(
    all_present,
    function(unit, target, model_name) {
      forecasts_unit_target_model <- forecasts_this_date %>%
        filter(unit == UQ(unit), target == UQ(target), model_name == UQ(model_name)) %>%
        select(as.character(required_quantiles))
      if(nrow(forecasts_unit_target_model) == 0) {
        return(FALSE)
      } else {
        return(all(!is.na(forecasts_unit_target_model)))
      }
    })
}
