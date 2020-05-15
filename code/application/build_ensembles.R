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

fips_codes <- read_csv("data-raw/state_fips_codes.csv") %>%
  bind_rows(
    data.frame(
      state='US',
      state_code='US',
      state_name='United States',
      stringsAsFactors=FALSE)) %>%
  select(state = state, unit = state_code)

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
  ungroup() %>%
  left_join(fips_codes, by = c("unit"))

## TODO:
## Consider adding a filter to the above like
## lubridate::ymd(forecast_week_end_date) - lubridate::ymd(timezero) <= 3,
## possibly or forecast_week_end_date < max(forecast_week_end_date)

# compute model eligibility
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
  lookback_length = 0
)

# convert model eligibility to wide format logical with human readable names
wide_model_eligibility <- model_eligibility %>%
  left_join(
    all_forecasts_and_observed %>% distinct(model_id, model_name),
    by = 'model_id'
  ) %>%
  mutate(eligibility = (eligibility == 'eligible')) %>%
  select(-model_id) %>%
  pivot_wider(names_from='model_name', values_from='eligibility') %>%
  left_join(fips_codes, by = c("unit")) %>%
  arrange(unit) %>%
  select(state, 2:(ncol(.)-1))

# group states by which models are included per state
state_groups <- wide_model_eligibility %>%
  group_by_if(is.logical) %>%
  summarize(states = list(state)) %>%
  ungroup()

# fit equally weighted ensemble per group
fit_one_group <- function(i) {
  model_inds <- state_groups[i, -ncol(state_groups)] %>%
    as.matrix() %>%
    which()
  models <- colnames(state_groups)[model_inds]

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df = all_forecasts_and_observed %>%
      filter(model_name %in% models),
    model_col = 'model_id',
    id_cols = c('unit', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'quantile',
    quantile_value_col = 'value'
  )

  ew_qra_fit <- estimate_qra(qfm_train = forecast_matrix, method = 'ew')
  return(ew_qra_fit)
}

state_groups$ew_qra_fits <- purrr::map(
  seq_len(nrow(state_groups)),
  fit_one_group
)


# obtain predictions from ensemble
predict_one_group <- function(i) {
  model_inds <- state_groups[i, seq_len(ncol(state_groups)-2)] %>%
    as.matrix() %>%
    which()
  models <- colnames(state_groups)[model_inds]
  states <- state_groups$states[[i]]
  ew_qra_fit <- state_groups$ew_qra_fits[[i]]

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df = all_forecasts_and_observed %>%
      filter(
        model_name %in% models,
        state %in% states,
        forecast_week_end_date == max(forecast_week_end_date)),
    model_col = 'model_id',
    id_cols = c('unit', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'quantile',
    quantile_value_col = 'value'
  )

  return(
    ew_qra_fit %>%
      predict(forecast_matrix) %>%
      as.data.frame()
  )
}

ensemble_predictions <- purrr::map_dfr(
  seq_len(nrow(state_groups)),
  predict_one_group
)


# save the results in required format
fips_codes <- read_csv("data-raw/state_fips_codes.csv") %>%
  bind_rows(
    data.frame(
      state='US',
      state_code='US',
      state_name='United States',
      stringsAsFactors=FALSE)) %>%
  select(location_name = state_name, unit = state_code)

formatted_ensemble_predictions <- ensemble_predictions %>%
  left_join(fips_codes, by='unit') %>%
  dplyr::transmute(
    forecast_date = forecast_week_end_date, # a convenient lie during development
    target = target,
    target_end_date = calc_target_week_end_date(
      forecast_week_end_date,
      as.integer(substr(target, 1, 1))),
    location = unit,
    location_name = location_name,
    type = 'quantile',
    quantile = quantile,
    value = value
  )

write_csv(formatted_ensemble_predictions,
  paste0('code/application/forecasts/COVIDhub-qra_ew/',
         formatted_ensemble_predictions$forecast_date[1],
         '-COVIDhub-qra_ew.csv')
)



#system('alias chrome="/Applications/Google\\ \\Chrome.app/Contents/MacOS/Google\\ \\Chrome"')
#system('chrome --allow-file-access-from-files ./code/application/plot_ensemble_forecasts.html')


# visualize
observed <- all_truths %>%
  filter(target == '1 wk ahead cum death') %>%
  mutate(time = calc_target_week_end_date(
    timezero,
    as.integer(substr(target, 1, 1))) %>%
    lubridate::ymd()) %>%
  left_join(fips_codes, by='unit')

plottable_ensemble_predictions <- formatted_ensemble_predictions %>%
  filter(quantile != 0.5) %>%
  mutate(
    endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
    alpha = ifelse(
      endpoint_type == 'lower',
      format(2*quantile, digits=3, nsmall=3),
      format(2*(1-quantile), digits=3, nsmall=3))
  ) %>%
  select(-quantile) %>%
  tidyr::pivot_wider(names_from='endpoint_type', values_from='value')

pdf('code/application/prediction_plots.pdf', width=24, height=60)
ggplot() +
  geom_line(data=observed, mapping = aes(x = time, y = observed)) +
  geom_ribbon(
    data = plottable_ensemble_predictions,
    mapping = aes(x = lubridate::ymd(target_end_date),
                  ymin=lower, ymax=upper,
                  fill=alpha)) +
  geom_line(
    data = formatted_ensemble_predictions %>%
      filter(quantile == 0.5),
    mapping = aes(x = lubridate::ymd(target_end_date), y = value)) +
  geom_point(
    data = formatted_ensemble_predictions %>%
      filter(quantile == 0.5),
    mapping = aes(x = lubridate::ymd(target_end_date), y = value)) +
  facet_wrap(~location_name, ncol=4, scales = 'free_y') +
  theme_bw()
dev.off()

