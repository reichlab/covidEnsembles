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

## from https://github.com/reichlab/covid19-forecast-hub/blob/master/code/ensemble-scripts/make_ewq_ensemble_script.R#L12
## models to exclude due to multiple models per team
## TODO: Maybe we should change this to a list of included models?
models_to_exclude <- c(
  ## Our ensemble
  'ensemble',
  ## CU
  "nointerv", "60-contact", "70-contact", "80-contact",
  "80-contact1x10p", "80-contact1x5p", "80-contactw10p", "80-contactw5p",
  ## Imperial
  "Ensemble1", "Ensemble2",
  ## IowaStateLW
  "Spatiotemporal Epidemic Modeling Daily Recover 15%",
  ## JHU_IDD
  "CovidScenarioPipeline-0.1", "CovidScenarioPipeline-0.2-HighEffectDistancing", "CovidScenarioPipeline-0.2-ModEffectDistancing",
  ## UChicago
  "CovidIL_40", "CovidIL_60", "CovidIL_80")

# keep only quantile forecasts for requested quantiles
# that were in the last submission from a given model for each forecast week
# end date
required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

all_forecasts_and_observed <- readRDS('./data/all_covid19_forecasts.rds') %>%
  dplyr::filter(
    !(model_name %in% models_to_exclude),
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

## TODO:
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

# convert model eligibility to wide format with human readable names,
# excluding excluded models
wide_model_eligibility <- model_eligibility %>%
  left_join(
    all_forecasts_and_observed %>% distinct(model_id, model_name),
    by = 'model_id'
  ) %>%
  mutate(eligibility = (eligibility == 'eligible')) %>%
  select(-model_id) %>%
  pivot_wider(names_from='model_name', values_from='eligibility') %>%
  left_join(
    read_csv("data-raw/state_fips_codes.csv") %>%
      bind_rows(
        data.frame(
          state='US',
          state_code='US',
          state_name='United States',
          stringsAsFactors=FALSE)) %>%
      select(state = state, unit = state_code),
    by = c("unit")) %>%
  arrange(unit) %>%
  select(state, 2:(ncol(.)-1))

# group states by which models are included per state
state_groups <- wide_model_eligibility %>%
  group_by_if(is.logical) %>%
  summarize(states = list(state))

#
