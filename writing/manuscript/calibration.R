library(covidData)
library(covidEnsembles)
library(tidyverse)
library(gridExtra)
library(zoltr)

# date for "truth" data used to compute scores and used in plots
issue_date <- '2020-07-26'

last_target_date <- lubridate::ymd(issue_date) %>%
  lubridate::floor_date(unit = 'week') %>%
  `-`(1) %>%
  as.character()

# dates for saturdays included in the analysis:
#  - we consider ensemble forecasts generated 2 days after this saturday date
#  - week ahead targets are defined relative to this saturday date
first_forecast_week_end_date <- lubridate::ymd('2020-04-25')
last_forecast_week_end_date <- lubridate::ymd(last_target_date) - 7
num_forecast_weeks <- (last_forecast_week_end_date -
                         first_forecast_week_end_date)/7 + 1
forecast_week_end_dates <- as.character(
  first_forecast_week_end_date +
    seq(from = 0, length = num_forecast_weeks)*7
)

# Load observed values:
observed_by_location_target_end_date <-
  covidData::load_jhu_data(
    issue_date = issue_date,
    spatial_resolution = c('state', 'national'),
    temporal_resolution = 'weekly',
    measure = 'deaths') %>%
  tidyr::pivot_longer(
    cols = c('inc', 'cum'),
    names_to = 'base_target',
    values_to = 'observed'
  ) %>%
  dplyr::transmute(
    location = location,
    base_target = paste0('wk ahead ', base_target, ' death'),
    target_end_date = as.character(date),
    observed = observed
  )


# Load model forecasts
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))
project_url <- 'https://www.zoltardata.com/api/project/44/'

required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

monday_dates <- lubridate::ymd(forecast_week_end_dates) + 2

all_forecasts <-
  purrr::map_dfr(
    lubridate::ymd(monday_dates),
    covidEnsembles:::do_zoltar_query,
    model_abbrs = "COVIDhub-ensemble",
    timezero_window_size = 1,
    targets = c(paste0(1:4, ' wk ahead inc death'), paste0(1:4, ' wk ahead cum death')),
    zoltar_connection = zoltar_connection,
    project_url = project_url,
    verbose = TRUE
  ) %>%
  dplyr::select(model, timezero, location=unit, target, quantile, value) %>%
  dplyr::filter(
    format(quantile, digits=3, nsmall=3) %in%
      format(required_quantiles, digits=3, nsmall=3)) %>%
  tidyr::pivot_wider(names_from = quantile, values_from = value) %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 1)),
    forecast_week_end_date = calc_forecast_week_end_date(timezero),
    target_end_date = calc_target_week_end_date(timezero, horizon)
  ) %>%
  dplyr::group_by(
    location, target, forecast_week_end_date, model
  ) %>%
  dplyr::top_n(1, timezero) %>%
  tidyr::pivot_longer(
    cols = all_of(as.character(required_quantiles)),
    names_to = 'quantile',
    values_to = 'value') %>%
  ungroup() %>%
  left_join(fips_codes, by = 'location')

# Calculate scores for forecasts
get_scores <- function(
  qfm,
  observed_by_location_target_end_date) {

  # incidence over four-week period, used for calculating percentage error of
  # forecasts of cumulative deaths with respect to 4 week incidence (numbers in text)
  four_week_inc <- observed_by_location_target_end_date %>%
    dplyr::filter(base_target == 'wk ahead cum death') %>%
    dplyr::group_by(location) %>%
    dplyr::arrange(location, target_end_date) %>%
    dplyr::mutate(
      four_week_inc = observed - dplyr::lag(observed, n = 4L)
    )


  row_index <- attr(qfm, 'row_index')
  col_index <- attr(qfm, 'col_index')

  y_test <- row_index %>%
    dplyr::mutate(
      base_target = substr(target, 3, nchar(target)),
      target_end_date = as.character(lubridate::ymd(forecast_week_end_date) +
        as.numeric(substr(target, 1, 1)) * 7)
    ) %>%
    dplyr::left_join(
      observed_by_location_target_end_date,
      by = c('location', 'target_end_date', 'base_target')
    ) %>%
    pull(observed)

  row_index$pit <- NA_real_
  for(i in seq_len(nrow(qfm))) {
    if(!is.na(y_test[i])) {
      qfm_row <- unclass(qfm)[i, ]
      if(y_test[i] < qfm_row[1]) {
        row_index$pit[i] <- as.numeric(col_index$quantile[1])
      } else if(y_test[i] > tail(qfm_row, 1)) {
        row_index$pit[i] <- as.numeric(tail(col_index$quantile, 1))
      } else if(any(qfm_row == y_test[i])) {
        row_index$pit[i] <- col_index$quantile[qfm_row == y_test[i]] %>%
          as.numeric() %>%
          median()
      } else {
        start_ind <- max(which(unclass(qfm)[i, ] < y_test[i]))
        row_index$pit[i] <- approx(
          x = qfm_row[start_ind:(start_ind+1)],
          y = as.numeric(col_index$quantile[start_ind:(start_ind+1)]),
          xout = y_test[i],
          method = "linear"
        )$y
      }
    }
  }

  for(i in seq_len((nrow(col_index) - 1)/2)) {
    coverage_name <- paste0('coverage_', format(1 - as.numeric(col_index$quantile[i]) * 2,
                                           nsmall=2, digits=2))
    wis_name <- paste0('wis_', format(1 - as.numeric(col_index$quantile[i]) * 2,
                                      nsmall=2, digits=2))

    pred_quantiles <- qfm[, c(i, nrow(col_index) + 1 - i)]
    row_index[[coverage_name]] <- (unclass(pred_quantiles)[, 1] <= y_test) &
      (unclass(pred_quantiles)[, 2] >= y_test)
    row_index[[wis_name]] <- covidEnsembles::wis(y_test, pred_quantiles)
  }

  i <- i + 1
  row_index[['wis_1']] <- abs(y_test - unclass(qfm[, i]))

  row_index[['wis']] <- covidEnsembles::wis(y_test, qfm)
  row_index$wis_reduced <- (row_index$wis_1 + row_index$wis_0.95 + row_index$wis_0.80 + row_index$wis_0.50)/4


  for(i in seq_len(nrow(col_index))) {
    coverage_name <- paste0('one_sided_coverage_', col_index$quantile[i])

    pred_quantiles <- qfm[, i]
    row_index[[coverage_name]] <- (y_test <= unclass(pred_quantiles)[, 1])
    row_index[[wis_name]] <- covidEnsembles::wis(y_test, pred_quantiles)
  }

  row_index$observed_inc <- row_index %>%
    dplyr::mutate(
      base_target = "wk ahead inc death",
      target_end_date = as.character(lubridate::ymd(forecast_week_end_date) +
                                       as.numeric(substr(target, 1, 1)) * 7)
    ) %>%
    dplyr::left_join(
      observed_by_location_target_end_date,
      by = c('location', 'target_end_date', 'base_target')
    ) %>%
    pull(observed)

  row_index$four_week_inc <- row_index %>%
    dplyr::mutate(
      target_end_date = as.character(lubridate::ymd(forecast_week_end_date) +
                                       as.numeric(substr(target, 1, 1)) * 7)
    ) %>%
    dplyr::left_join(
      four_week_inc %>% dplyr::select(location, target_end_date, four_week_inc),
      by = c('location', 'target_end_date')
    ) %>%
    pull(four_week_inc)

  row_index <- row_index %>%
    dplyr::mutate(
      one_week_mape = ifelse(
        substr(target, 1, 1) == '1',
        wis_1/observed_inc,
        NA_real_
      ),
      four_week_mape = ifelse(
        substr(target, 1, 1) == '4',
        wis_1/four_week_inc,
        NA_real_
      )
    )

  row_index$observed <- y_test

  return(row_index[!is.na(y_test), ])
}

qfm <- covidEnsembles::new_QuantileForecastMatrix_from_df(
  forecast_df = all_forecasts,
  model_col = 'model',
  id_cols = c('location', 'forecast_week_end_date', 'target'),
  quantile_name_col = 'quantile',
  quantile_value_col = 'value',
  drop_missing_id_levels = TRUE
)

all_scores <- get_scores(
  qfm = qfm,
  observed_by_location_target_end_date = observed_by_location_target_end_date)

qfm_rounded <- covidEnsembles::new_QuantileForecastMatrix_from_df(
  forecast_df = all_forecasts %>%
    dplyr::mutate(
      value = ifelse(
        quantile < '0.5',
        floor(value),
        ifelse(
          quantile == '0.5',
          round(value),
          ceiling(value)
        )
      )
    ),
  model_col = 'model',
  id_cols = c('location', 'forecast_week_end_date', 'target'),
  quantile_name_col = 'quantile',
  quantile_value_col = 'value',
  drop_missing_id_levels = TRUE
)

all_scores_rounded <- get_scores(
  qfm = qfm_rounded,
  observed_by_location_target_end_date = observed_by_location_target_end_date)


# Determine the number of models conributing to each ensemble forecast.
# Used to subset to forecasts that combined at least one model when calculating
# mean scores below
num_models_by_fwed_and_location <- NULL
for(fwed in unique(all_scores$forecast_week_end_date)) {
  if(fwed == '2020-04-25') {
    # based on https://github.com/reichlab/covid19-forecast-hub/blob/2fda1aaa7b806fafe0b1bfe1a96e7310fdc2f572/data-raw/COVIDhub-ensemble/COVIDhub-ensemble-information.csv
    num_models_by_fwed_and_location <- data.frame(
      location = unique(all_scores$location),
      forecast_week_end_date = fwed,
      num_models = 6
    )
    num_models_by_fwed_and_location$num_models[num_models_by_fwed_and_location$location == 'US'] <- 3
  } else if(fwed == '2020-05-02') {
    # based on https://github.com/reichlab/covid19-forecast-hub/blob/4a7edb6a0daefdeb73e35f1ebd09e3bff5c44045/data-raw/COVIDhub-ensemble/state-weight-information.csv
    # and https://github.com/reichlab/covid19-forecast-hub/blob/master/ensemble-metadata/COVIDhub-ensemble-information.csv
    num_models_by_fwed_and_location <- dplyr::bind_rows(
      num_models_by_fwed_and_location,
      readr::read_csv('https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/4a7edb6a0daefdeb73e35f1ebd09e3bff5c44045/data-raw/COVIDhub-ensemble/state-weight-information.csv') %>%
        dplyr::transmute(
          location = fips,
          forecast_week_end_date = fwed,
          num_models = n_models),
      data.frame(
        location = 'US',
        forecast_week_end_date = fwed,
        num_models = 7,
        stringsAsFactors = FALSE
      )
    )
  } else if(fwed == '2020-05-09') {
    # based on https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/ensemble-metadata/2020-05-11-state-weight-information.csv
    # and https://github.com/reichlab/covid19-forecast-hub/blob/master/ensemble-metadata/2020-05-11-COVIDhub-ensemble-information.csv
    num_models_by_fwed_and_location <- dplyr::bind_rows(
      num_models_by_fwed_and_location,
      readr::read_csv('https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/ensemble-metadata/2020-05-11-state-weight-information.csv') %>%
        dplyr::transmute(
          location = fips,
          forecast_week_end_date = fwed,
          num_models = n_models),
      data.frame(
        location = 'US',
        forecast_week_end_date = fwed,
        num_models = 7,
        stringsAsFactors = FALSE
      )
    )
  } else if(fwed == '2020-05-16') {
    temp <- readr::read_csv('https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/ensemble-metadata/2020-05-19-model-weights.csv')
    num_models_by_fwed_and_location <- dplyr::bind_rows(
      num_models_by_fwed_and_location,
      temp %>% dplyr::transmute(
        location = location,
        forecast_week_end_date = fwed,
        num_models = temp %>% select(!starts_with('location')) %>%
          apply(1, function(weights_row) {sum(as.numeric(weights_row) > 0)}))
    )
  } else {
    temp <- readr::read_csv(
      paste0('https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/ensemble-metadata/',
        as.character(lubridate::ymd(fwed) + 2),
        '-cum_death-model-weights.csv'))
    if('abbreviation' %in% names(temp)) {
      temp <- temp %>% select(-abbreviation)
    }
    num_models_by_fwed_and_location <- dplyr::bind_rows(
      num_models_by_fwed_and_location,
      temp %>% dplyr::transmute(
        location = location,
        forecast_week_end_date = fwed,
        num_models = temp %>% select(!starts_with('location')) %>%
          apply(1, function(weights_row) {sum(as.numeric(weights_row) > 0)}))
    )
  }
}

all_scores <- all_scores %>%
  dplyr::left_join(
    num_models_by_fwed_and_location,
    by = c('location', 'forecast_week_end_date'))

all_scores_rounded <- all_scores_rounded %>%
  dplyr::left_join(
    num_models_by_fwed_and_location,
    by = c('location', 'forecast_week_end_date'))


# Numbers in the main text

## total forecasts and total in locations where at least 2 models contributed
all_scores %>%
  dplyr::filter(
    grepl('cum', target)
  ) %>%
  nrow()

all_scores %>%
  dplyr::filter(
    grepl('cum', target),
    num_models > 1) %>%
  nrow()


## Number of models each week
num_models_by_fwed_and_location %>%
  dplyr::group_by(forecast_week_end_date) %>%
  dplyr::summarize(max(num_models))

## MAE values for US only -- Results section paragraph 2
all_scores %>%
  dplyr::filter(num_models > 1, location == 'US') %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 2)),
    target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon,
    base_target = ifelse(
      grepl('cum', target),
      'Cumulative',
      'Incident'
    )
  ) %>%
  dplyr::filter(target_end_date >= '2020-05-23', target_end_date <= '2020-07-11') %>%
  dplyr::group_by(horizon, base_target) %>%
  summarize(
    mae = mean(wis_1),
    wis = mean(wis)
  ) %>%
  dplyr::filter(base_target == 'Cumulative') %>%
  tidyr::pivot_longer(cols = c('mae', 'wis'), names_to = 'measure', values_to = 'value') %>%
  tidyr::pivot_wider(names_from = 'horizon', values_from = 'value') %>%
  as.data.frame()


all_scores %>%
  dplyr::filter(num_models > 1, location == 'US') %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 2)),
    target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon,
    base_target = ifelse(
      grepl('cum', target),
      'Cumulative',
      'Incident'
    )
  ) %>%
  dplyr::filter(target_end_date >= '2020-05-23', target_end_date <= '2020-07-11',
                base_target == 'Cumulative', horizon == 1) %>%
  summarize(
    mape_1 = mean(one_week_mape, na.rm = TRUE),
    mape_4 = mean(four_week_mape, na.rm = TRUE)
  )


## Proportion of observations above predicted 99th percentile
prop_leq_99 <- all_scores %>%
  dplyr::filter(num_models > 1) %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 2)),
    target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon,
    base_target = ifelse(
      grepl('cum', target),
      'Cumulative',
      'Incident'
    )
  ) %>%
  dplyr::filter(target_end_date >= '2020-05-23', target_end_date <= '2020-07-11') %>%
  dplyr::group_by(base_target) %>%
  dplyr::summarize_at(vars(starts_with('one_sided_coverage_')), mean) %>%
  tidyr::pivot_longer(
    cols = colnames(.)[!(colnames(.) %in% c('horizon', 'base_target'))],
    names_to = 'nominal_coverage',
    names_prefix = 'one_sided_coverage_',
    values_to = 'empirical_coverage') %>%
  dplyr::mutate(nominal_coverage = as.numeric(nominal_coverage)) %>%
  dplyr::filter(nominal_coverage == 0.99)
1 - prop_leq_99 %>%
  dplyr::filter(base_target == 'Cumulative') %>%
  dplyr::pull(empirical_coverage) %>%
  head(1)




# Supplemental Figure 2: models included by location
models_used_by_location <- NULL
for(fwed in unique(all_scores$forecast_week_end_date)) {
  if(fwed == '2020-04-25') {
    # based on https://github.com/reichlab/covid19-forecast-hub/blob/2fda1aaa7b806fafe0b1bfe1a96e7310fdc2f572/data-raw/COVIDhub-ensemble/COVIDhub-ensemble-information.csv
    models_used_by_location <- dplyr::bind_rows(
      readr::read_csv(
        'https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/2fda1aaa7b806fafe0b1bfe1a96e7310fdc2f572/data-processed/CU-70contact/2020-04-26-CU-70contact.csv',
        col_types = cols(
          forecast_date = col_date(format = ""),
          target = col_character(),
          target_end_date = col_date(format = ""),
          location = col_character(),
          type = col_character(),
          quantile = col_double(),
          value = col_double()
        )) %>%
        dplyr::distinct(location) %>%
        dplyr::mutate(model = 'CU-select'),
      readr::read_csv(
        'https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/2fda1aaa7b806fafe0b1bfe1a96e7310fdc2f572/data-processed/MIT_CovidAnalytics-DELPHI/2020-04-27-MIT_CovidAnalytics-DELPHI.csv',
        col_types = cols(
          forecast_date = col_date(format = ""),
          target = col_character(),
          target_end_date = col_date(format = ""),
          location = col_character(),
          type = col_character(),
          quantile = col_double(),
          value = col_double()
        )) %>%
        dplyr::distinct(location) %>%
        dplyr::mutate(model = 'MIT_CovidAnalytics-DELPHI'),
      readr::read_csv(
        'https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/2fda1aaa7b806fafe0b1bfe1a96e7310fdc2f572/data-processed/LANL-GrowthRate/2020-04-26-LANL-GrowthRate.csv',
        col_types = cols(
          forecast_date = col_date(format = ""),
          target = col_character(),
          target_end_date = col_date(format = ""),
          location = col_character(),
          type = col_character(),
          quantile = col_double(),
          value = col_double()
        )) %>%
        dplyr::distinct(location) %>%
        dplyr::mutate(model = 'LANL-GrowthRate'),
      readr::read_csv(
        'https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/2fda1aaa7b806fafe0b1bfe1a96e7310fdc2f572/data-processed/UMass-MechBayes/2020-04-26-UMass-MechBayes.csv',
        col_types = cols(
          forecast_date = col_date(format = ""),
          target = col_character(),
          target_end_date = col_date(format = ""),
          location = col_character(),
          type = col_character(),
          quantile = col_double(),
          value = col_double()
        )) %>%
        dplyr::distinct(location) %>%
        dplyr::mutate(model = 'UMass-MechBayes'),
      readr::read_csv(
        'https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/2fda1aaa7b806fafe0b1bfe1a96e7310fdc2f572/data-processed/UT-Mobility/2020-04-27-UT-Mobility.csv',
        col_types = cols(
          forecast_date = col_date(format = ""),
          target = col_character(),
          target_end_date = col_date(format = ""),
          location = col_character(),
          type = col_character(),
          quantile = col_double(),
          value = col_double()
        )) %>%
        dplyr::distinct(location) %>%
        dplyr::mutate(model = 'UT-Mobility'),
      readr::read_csv(
        'https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/2fda1aaa7b806fafe0b1bfe1a96e7310fdc2f572/data-processed/YYG-ParamSearch/2020-04-27-YYG-ParamSearch.csv',
        col_types = cols(
          forecast_date = col_date(format = ""),
          target = col_character(),
          target_end_date = col_date(format = ""),
          location = col_character(),
          type = col_character(),
          quantile = col_double(),
          value = col_double()
        )) %>%
        dplyr::distinct(location) %>%
        dplyr::mutate(model = 'YYG-ParamSearch')
    )
  } else if(fwed == '2020-05-02') {
    # based on https://github.com/reichlab/covid19-forecast-hub/blob/4a7edb6a0daefdeb73e35f1ebd09e3bff5c44045/data-raw/COVIDhub-ensemble/state-weight-information.csv
    # and https://github.com/reichlab/covid19-forecast-hub/blob/master/ensemble-metadata/COVIDhub-ensemble-information.csv
    models_used_by_location <- dplyr::bind_rows(
      models_used_by_location,
      readr::read_csv('https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/4a7edb6a0daefdeb73e35f1ebd09e3bff5c44045/data-raw/COVIDhub-ensemble/state-weight-information.csv') %>%
        dplyr::select(-state, -sum, -n_models) %>%
        tidyr::pivot_longer(
          cols = colnames(.)[colnames(.) != 'fips'],
          names_to = 'model',
          values_to = 'weight'
        ) %>%
        dplyr::filter(weight > 0) %>%
        dplyr::transmute(
          location = fips,
          model = model
        ),
      readr::read_csv('https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/ensemble-metadata/COVIDhub-ensemble-information.csv') %>%
        dplyr::transmute(location = 'US', model = model_name)
    )
  } else if(fwed == '2020-05-09') {
    # based on https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/ensemble-metadata/2020-05-11-state-weight-information.csv
    # and https://github.com/reichlab/covid19-forecast-hub/blob/master/ensemble-metadata/2020-05-11-COVIDhub-ensemble-information.csv
    models_used_by_location <- dplyr::bind_rows(
      models_used_by_location,
      readr::read_csv('https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/ensemble-metadata/2020-05-11-state-weight-information.csv') %>%
        dplyr::select(-state, -sum, -n_models) %>%
        tidyr::pivot_longer(
          cols = colnames(.)[colnames(.) != 'fips'],
          names_to = 'model',
          values_to = 'weight'
        ) %>%
        dplyr::filter(weight > 0) %>%
        dplyr::transmute(
          location = fips,
          model = model
        ),
      readr::read_csv('https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/ensemble-metadata/2020-05-11-COVIDhub-ensemble-information.csv') %>%
        dplyr::transmute(location = 'US', model = model_name)
    )
  } else if(fwed == '2020-05-16') {
    models_used_by_location <- dplyr::bind_rows(
      models_used_by_location,
      readr::read_csv('https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/ensemble-metadata/2020-05-19-model-weights.csv') %>%
        dplyr::select(-location_name) %>%
        tidyr::pivot_longer(
          cols = colnames(.)[colnames(.) != 'location'],
          names_to = 'model',
          values_to = 'weight'
        ) %>%
        dplyr::filter(weight > 0) %>%
        dplyr::transmute(
          location = location,
          model = model
        )
    )
  } else {
    temp <- readr::read_csv(
      paste0('https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/ensemble-metadata/',
             as.character(lubridate::ymd(fwed) + 2),
             '-cum_death-model-weights.csv'))
    if('location_name' %in% colnames(temp)) {
      temp <- temp %>% select(-location_name)
    }
    if('location_abbreviation' %in% colnames(temp)) {
      temp <- temp %>% select(-location_abbreviation)
    }
    models_used_by_location <- dplyr::bind_rows(
      models_used_by_location,
      temp %>%
        tidyr::pivot_longer(
          cols = colnames(.)[colnames(.) != 'location'],
          names_to = 'model',
          values_to = 'weight'
        ) %>%
        dplyr::filter(weight > 0) %>%
        dplyr::transmute(
          location = location,
          model = model
        )
    )
  }
}

# re-map names of models that changed or lost the front-end team name
models_used_by_location$model[
  models_used_by_location$model %in% c('X80contact_1x', 'select', 'CU-80contact_1x')
] <- 'CU-select'

models_used_by_location$model[
  models_used_by_location$model == 'GrowthRate'
] <- 'LANL-GrowthRate'

models_used_by_location$model[
  models_used_by_location$model == 'DELPHI'
] <- 'MIT_CovidAnalytics-DELPHI'

models_used_by_location$model[
  models_used_by_location$model %in% c('GLEAM_COVID', 'MOBS_NEU-GLEAM_COVID')
] <- 'MOBS-GLEAM_COVID'

models_used_by_location$model[
  models_used_by_location$model == 'SuEIR'
] <- 'UCLA-SuEIR'

models_used_by_location$model[
  models_used_by_location$model == 'MechBayes'
] <- 'UMass-MechBayes'

models_used_by_location$model[
  models_used_by_location$model == 'Mobility'
] <- 'UT-Mobility'

models_used_by_location$model[
  models_used_by_location$model == 'ParamSearch'
] <- 'YYG-ParamSearch'

models_used_by_location$model[
  models_used_by_location$model == 'DeepCOVID'
] <- 'GT-DeepCOVID'

models_used_by_location$model[
  models_used_by_location$model == 'IowaStateLW-STEM10'
] <- 'IowaStateLW-STEM'

models_used_by_location$model[
  models_used_by_location$model %in% c('CovidIL_100', 'UChicago-CovidIL_10_increase')
] <- 'UChicago-CovidIL'

model_use_counts <- models_used_by_location %>%
  dplyr::count(model, location)

total_count_order <- model_use_counts %>%
  dplyr::group_by(model) %>%
  dplyr::summarize(n = sum(n)) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::pull(model)

png('writing/manuscript/model_inclusion_counts.png', width = 10, height = 10, units = "in", res = 300)
ggplot(
  data = model_use_counts %>%
    dplyr::filter(location != '60') %>%
    dplyr::mutate(model = factor(model, levels = total_count_order)) %>%
    dplyr::left_join(covidData::fips_codes, by = 'location')) +
  geom_raster(
    mapping = aes(x = model, y = abbreviation, fill = n)
  ) +
  scale_fill_viridis_c(
    "Number of\nWeeks Included",
    begin = 0.2, end = 0.8,
    breaks = c(1, 3, 5, 7, 9, 11)) +
  scale_y_discrete("Location") +
  scale_x_discrete("Model") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major = element_blank()
  )
dev.off()




# Per-location MAE labels, which will be used in Figure 1
mae_labels <- all_scores %>%
  dplyr::left_join(
    covidData::fips_codes %>%
      dplyr::select(location, location_abbreviation = abbreviation),
    by = 'location'
  ) %>%
  dplyr::filter(num_models > 1) %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 2)),
    target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon
  ) %>%
  dplyr::filter(
    target_end_date >= '2020-05-23', target_end_date <= '2020-07-11',
    grepl('cum', target)) %>%
  dplyr::group_by(location, location_abbreviation, horizon) %>%
  dplyr::summarize(mae = mean(wis_1)) %>%
  tidyr::pivot_wider(names_from = 'horizon', values_from = 'mae') %>%
  dplyr::mutate(
    label_text = paste0(
      'Horizon 1 MAE: ', format(round(`1`, 1), nsmall = 1),
      '\nHorizon 4 MAE: ', format(round(`4`, 1), nsmall = 1)
    ),
    long_label_text = paste0(
#      'Per-Horizon MAE',
#      'Mean Absolute Error',
      '          MAE',
      '\nHorizon 1: ', format(round(`1`, 1), nsmall = 1),
      '\nHorizon 2: ', format(round(`2`, 1), nsmall = 1),
      '\nHorizon 3: ', format(round(`3`, 1), nsmall = 1),
      '\nHorizon 4: ', format(round(`4`, 1), nsmall = 1)
    )
  ) %>%
  dplyr::ungroup()



# Figure 1: plots of forecasts for CA, FL, TX, and US
# (the four locations with largest reported incidence on July 11)
locations_to_plot <- observed_by_location_target_end_date %>%
  filter(
    target_end_date == '2020-07-25',
    base_target == 'wk ahead inc death') %>%
  arrange(desc(observed)) %>%
  pull(location) %>%
  head(4)

location_abbreviations_to_plot <- purrr::map_chr(
  locations_to_plot,
  function(loc) {
    covidData::fips_codes$abbreviation[
      covidData::fips_codes$location == loc]
  }
)
forecast_week_end_dates_to_plot <- c('2020-05-16', '2020-06-27')

observed <- observed_by_location_target_end_date %>%
  dplyr::filter(
    location %in% locations_to_plot,
    base_target == 'wk ahead cum death'
  ) %>%
  dplyr::mutate(target_end_date = lubridate::ymd(target_end_date)) %>%
  dplyr::left_join(fips_codes, by='location')

plottable_ensemble_predictions <- all_forecasts %>%
  dplyr::filter(
    location %in% locations_to_plot,
    forecast_week_end_date %in% forecast_week_end_dates_to_plot,
    grepl('cum', target)
  ) %>%
  dplyr::mutate(
    endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
    quantile = as.numeric(quantile),
    alpha = ifelse(
      endpoint_type == 'lower',
      format(2*quantile, digits=3, nsmall=3),
      format(2*(1-quantile), digits=3, nsmall=3)),
    interval_level = ifelse(
      endpoint_type == 'lower',
      paste0(format((1 - 2*quantile)*100, digits=2, nsmall=0), '%'),
      paste0(format((1 - 2*(1-quantile))*100, digits=2, nsmall=0), '%')
    )
  ) %>%
  dplyr::filter(alpha != "1.000") %>%
  dplyr::select(-quantile) %>%
  tidyr::pivot_wider(names_from='endpoint_type', values_from='value') %>%
  dplyr::filter(interval_level %in% c('  0%', ' 50%', ' 80%', ' 95%')) %>%
  dplyr::mutate(
    interval_level = factor(interval_level, levels = c(' 95%', ' 80%', ' 50%', '  0%'))
  ) %>%
  dplyr::mutate(
    location_abbreviation = factor(
      location_abbreviation,
      levels = location_abbreviations_to_plot),
    week_and_level = factor(
      paste0(lubridate::ymd(forecast_week_end_date)+2, ':', interval_level),
      levels = paste0(rep(lubridate::ymd(forecast_week_end_dates_to_plot) + 2, each=3), c(': 95%', ': 80%', ': 50%'))
    )
  )

for(fwed in forecast_week_end_dates_to_plot) {
  fwed <- lubridate::ymd(fwed)
  interp_endpoint_dates <- c(fwed, fwed + 7)
  interp_date <- fwed + 2
  start_intervals <- purrr::map_dfr(
    locations_to_plot,
    function(location) {
      interp_endpoint_obs <- observed %>%
        dplyr::filter(
          location == UQ(location),
          target_end_date %in% interp_endpoint_dates,
          base_target == 'wk ahead cum death'
        ) %>%
        dplyr::arrange(target_end_date) %>%
        dplyr::pull(observed)
      interp_obs <- approx(
        x = interp_endpoint_dates,
        y = interp_endpoint_obs,
        xout = interp_date
      )
      tidyr::expand_grid(
        forecast_week_end_date = as.character(fwed),
        target_end_date = as.character(interp_date),
        location = location,
        location_abbreviation = plottable_ensemble_predictions %>%
          dplyr::filter(location == UQ(location)) %>%
          head(1) %>%
          dplyr::pull(location_abbreviation),
        lower = interp_obs$y,
        upper = interp_obs$y,
        week_and_level = factor(
          paste0(as.character(fwed+2), c(': 95%', ': 80%', ': 50%')),
          levels = paste0(rep(lubridate::ymd(forecast_week_end_dates_to_plot) + 2, each=3), c(': 95%', ': 80%', ': 50%'))
        )
      )
    })
  plottable_ensemble_predictions <- dplyr::bind_rows(
    plottable_ensemble_predictions,
    start_intervals
  )
}

plottable_ensemble_predictions <- plottable_ensemble_predictions %>%
  dplyr::mutate(
    forecast_week_end_date = as.character(lubridate::ymd(forecast_week_end_date) + 2)
  )

points_to_plot <- dplyr::bind_rows(
  observed %>%
    dplyr::filter(
      location %in% locations_to_plot,
      target_end_date > lubridate::ymd('2020-03-15'),
      base_target == 'wk ahead cum death'
    ) %>%
    dplyr::mutate(
      point_type = 'Observed',
      location_abbreviation = factor(
        location_abbreviation,
        levels = location_abbreviations_to_plot)
    ),
  all_forecasts %>%
    dplyr::filter(
      location %in% locations_to_plot,
      forecast_week_end_date %in% forecast_week_end_dates_to_plot,
      grepl('cum', target),
      quantile == 0.5) %>%
    dplyr::mutate(
      forecast_week_end_date = as.character(lubridate::ymd(forecast_week_end_date) + 2),
      location_abbreviation = factor(
        location_abbreviation,
        levels = location_abbreviations_to_plot),
      target_end_date = lubridate::ymd(target_end_date),
      observed = value,
      point_type = 'Point Prediction'
    )
)


png('writing/manuscript/forecast_plots.png', width = 10, height = 7, units = "in", res = 300)
p_forecasts <- ggplot() +
  geom_vline(
    data = data.frame(
      x = lubridate::ymd(forecast_week_end_dates_to_plot) + 2),
    mapping = aes(xintercept = x, color = factor(x)),
    size = 1,
    linetype = 2,
    show.legend = FALSE
  ) +
  geom_ribbon(
    data = plottable_ensemble_predictions,
    mapping = aes(x = lubridate::ymd(target_end_date),
                  ymin=lower, ymax=upper,
                  fill=week_and_level)) +
  geom_line(
    data=observed %>%
      dplyr::filter(
        location %in% locations_to_plot,
        target_end_date > lubridate::ymd('2020-03-15'),
        base_target == 'wk ahead cum death'
      ) %>%
      dplyr::mutate(
        location_abbreviation = factor(
          location_abbreviation,
          levels = location_abbreviations_to_plot)
      ),
    mapping = aes(x = target_end_date, y = observed)) +
  geom_line(
    data = all_forecasts %>%
      dplyr::filter(
        location %in% locations_to_plot,
        forecast_week_end_date %in% forecast_week_end_dates_to_plot,
        grepl('cum', target),
        quantile == 0.5) %>%
      dplyr::mutate(
        forecast_week_end_date = as.character(lubridate::ymd(forecast_week_end_date) + 2),
        location_abbreviation = factor(
          location_abbreviation,
          levels = location_abbreviations_to_plot)
      ),
    mapping = aes(
      x = lubridate::ymd(target_end_date),
      y = value,
      color = factor(forecast_week_end_date),
      group = factor(forecast_week_end_date)),
    size = 1) +
  geom_point(
    data= points_to_plot,
    mapping = aes(
      x = target_end_date, y = observed,
      shape = point_type, size = point_type),
  ) +
  geom_label(
    mae_labels %>%
      dplyr::filter(location %in% locations_to_plot) %>%
      dplyr::mutate(
        location_abbreviation = factor(
          location_abbreviation,
          levels = location_abbreviations_to_plot)
      ),
    mapping = aes(label = long_label_text),
    x = -Inf,
    y = Inf,
    hjust = -0.05,
    vjust = 1.05
  ) +
  scale_color_manual(
    "Date Forecast Made",
    values = c('#045a8d', '#800026')
  ) +
  scale_fill_manual(
    "Interval Level",
    #        values = c('#a6bddb', '#3690c0', '#0570b0')
    values = c('#bdc9e1', '#74a9cf', '#2b8cbe', '#fed976', '#fd8d3c', '#e31a1c')
  ) +
  scale_shape_manual(
    'Observed Data or\nPoint Prediction',
    values = c('Observed' = 16, 'Point Prediction' = 88)
  ) +
  scale_size_manual(
    'Observed Data or\nPoint Prediction',
    values = c('Observed' = 2, 'Point Prediction' = 4)
  ) +
  #      scale_linetype_discrete("Observation or\nPrediction Date") +
  #      scale_shape_discrete("Observation or\nPrediction Date") +
  xlab('Date') +
  ylab('Cumulative Deaths') +
  facet_wrap( ~ location_abbreviation, scales = "free_y") +
  theme_bw(base_size = 13)
print(p_forecasts)
dev.off()


# Supplemental Figure 1: plots of forecasts for all locations
all_locations_to_plot <- unique(all_forecasts$location_abbreviation)
all_locations_to_plot <- c(
  'US',
  sort(all_locations_to_plot[!(all_locations_to_plot %in% c('US', 'AS'))]) # drop AS
)

png('writing/manuscript/forecast_plots_all_locations1.png', width = 10, height = 11, units = "in", res = 300)
location_abbreviations_to_plot <- head(all_locations_to_plot, 28)
to_search <- covidData::fips_codes %>% dplyr::filter(!is.na(abbreviation))
locations_to_plot <- purrr::map_chr(
  location_abbreviations_to_plot,
  function(abbr) {
    to_search$location[to_search$abbreviation == abbr]
  }
)

observed <- observed_by_location_target_end_date %>%
  dplyr::filter(
    location %in% locations_to_plot,
    base_target == 'wk ahead cum death'
  ) %>%
  dplyr::mutate(target_end_date = lubridate::ymd(target_end_date)) %>%
  dplyr::left_join(fips_codes, by='location')

plottable_ensemble_predictions <- all_forecasts %>%
  dplyr::filter(
    location %in% locations_to_plot,
    forecast_week_end_date %in% forecast_week_end_dates_to_plot,
    grepl('cum', target)
  ) %>%
  dplyr::mutate(
    endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
    quantile = as.numeric(quantile),
    alpha = ifelse(
      endpoint_type == 'lower',
      format(2*quantile, digits=3, nsmall=3),
      format(2*(1-quantile), digits=3, nsmall=3)),
    interval_level = ifelse(
      endpoint_type == 'lower',
      paste0(format((1 - 2*quantile)*100, digits=2, nsmall=0), '%'),
      paste0(format((1 - 2*(1-quantile))*100, digits=2, nsmall=0), '%')
    )
  ) %>%
  dplyr::filter(alpha != "1.000") %>%
  dplyr::select(-quantile) %>%
  tidyr::pivot_wider(names_from='endpoint_type', values_from='value') %>%
  dplyr::filter(interval_level %in% c('  0%', ' 50%', ' 80%', ' 95%')) %>%
  dplyr::mutate(
    interval_level = factor(interval_level, levels = c(' 95%', ' 80%', ' 50%', '  0%'))
  ) %>%
  dplyr::mutate(
    location_abbreviation = factor(
      location_abbreviation,
      levels = location_abbreviations_to_plot),
    week_and_level = factor(
      paste0(lubridate::ymd(forecast_week_end_date)+2, ':', interval_level),
      levels = paste0(rep(lubridate::ymd(forecast_week_end_dates_to_plot) + 2, each=3), c(': 95%', ': 80%', ': 50%'))
    )
  )

for(fwed in forecast_week_end_dates_to_plot) {
  fwed <- lubridate::ymd(fwed)
  interp_endpoint_dates <- c(fwed, fwed + 7)
  interp_date <- fwed + 2
  start_intervals <- purrr::map_dfr(
    locations_to_plot,
    function(location) {
      interp_endpoint_obs <- observed %>%
        dplyr::filter(
          location == UQ(location),
          target_end_date %in% interp_endpoint_dates,
          base_target == 'wk ahead cum death'
        ) %>%
        dplyr::arrange(target_end_date) %>%
        dplyr::pull(observed)
      interp_obs <- approx(
        x = interp_endpoint_dates,
        y = interp_endpoint_obs,
        xout = interp_date
      )
      tidyr::expand_grid(
        forecast_week_end_date = as.character(fwed),
        target_end_date = as.character(interp_date),
        location = location,
        location_abbreviation = plottable_ensemble_predictions %>%
          dplyr::filter(location == UQ(location)) %>%
          head(1) %>%
          dplyr::pull(location_abbreviation),
        lower = interp_obs$y,
        upper = interp_obs$y,
        week_and_level = factor(
          paste0(as.character(fwed+2), c(': 95%', ': 80%', ': 50%')),
          levels = paste0(rep(lubridate::ymd(forecast_week_end_dates_to_plot) + 2, each=3), c(': 95%', ': 80%', ': 50%'))
        )
      )
    })
  plottable_ensemble_predictions <- dplyr::bind_rows(
    plottable_ensemble_predictions,
    start_intervals
  )
}

plottable_ensemble_predictions <- plottable_ensemble_predictions %>%
  dplyr::mutate(
    forecast_week_end_date = as.character(lubridate::ymd(forecast_week_end_date) + 2)
  )

points_to_plot <- dplyr::bind_rows(
  observed %>%
    dplyr::filter(
      location %in% locations_to_plot,
      target_end_date > lubridate::ymd('2020-03-15'),
      base_target == 'wk ahead cum death'
    ) %>%
    dplyr::mutate(
      point_type = 'Observed',
      location_abbreviation = factor(
        location_abbreviation,
        levels = location_abbreviations_to_plot)
    ),
  all_forecasts %>%
    dplyr::filter(
      location %in% locations_to_plot,
      forecast_week_end_date %in% forecast_week_end_dates_to_plot,
      grepl('cum', target),
      quantile == 0.5) %>%
    dplyr::mutate(
      forecast_week_end_date = as.character(lubridate::ymd(forecast_week_end_date) + 2),
      location_abbreviation = factor(
        location_abbreviation,
        levels = location_abbreviations_to_plot),
      target_end_date = lubridate::ymd(target_end_date),
      observed = value,
      point_type = 'Point Prediction'
    )
)

p_forecasts <- ggplot() +
  geom_vline(
    data = data.frame(
      x = lubridate::ymd(forecast_week_end_dates_to_plot) + 2),
    mapping = aes(xintercept = x, color = factor(x)),
    size = 1,
    linetype = 2,
    show.legend = FALSE
  ) +
  geom_ribbon(
    data = plottable_ensemble_predictions,
    mapping = aes(x = lubridate::ymd(target_end_date),
                  ymin=lower, ymax=upper,
                  fill=week_and_level)) +
  geom_line(
    data=observed %>%
      dplyr::filter(
        location %in% locations_to_plot,
        target_end_date > lubridate::ymd('2020-03-15'),
        base_target == 'wk ahead cum death'
      ) %>%
      dplyr::mutate(
        location_abbreviation = factor(
          location_abbreviation,
          levels = location_abbreviations_to_plot)
      ),
    mapping = aes(x = target_end_date, y = observed)) +
  geom_line(
    data = all_forecasts %>%
      dplyr::filter(
        location %in% locations_to_plot,
        forecast_week_end_date %in% forecast_week_end_dates_to_plot,
        grepl('cum', target),
        quantile == 0.5) %>%
      dplyr::mutate(
        forecast_week_end_date = as.character(lubridate::ymd(forecast_week_end_date) + 2),
        location_abbreviation = factor(
          location_abbreviation,
          levels = location_abbreviations_to_plot)
      ),
    mapping = aes(
      x = lubridate::ymd(target_end_date),
      y = value,
      color = factor(forecast_week_end_date),
      group = factor(forecast_week_end_date)),
    size = 1) +
  geom_point(
    data= points_to_plot,
    mapping = aes(
      x = target_end_date, y = observed,
      shape = point_type, size = point_type),
  ) +
  scale_color_manual(
    "Date Forecast\nMade",
    values = c('#045a8d', '#800026')
  ) +
  scale_fill_manual(
    "Interval Level",
    #        values = c('#a6bddb', '#3690c0', '#0570b0')
    values = c('#bdc9e1', '#74a9cf', '#2b8cbe', '#fed976', '#fd8d3c', '#e31a1c')
  ) +
  scale_shape_manual(
    'Observed Data or\nPoint Prediction',
    values = c('Observed' = 16, 'Point Prediction' = 88)
  ) +
  scale_size_manual(
    'Observed Data or\nPoint Prediction',
    values = c('Observed' = 2, 'Point Prediction' = 4)
  ) +
  #      scale_linetype_discrete("Observation or\nPrediction Date") +
  #      scale_shape_discrete("Observation or\nPrediction Date") +
  xlab('Date') +
  ylab('Cumulative Deaths') +
  facet_wrap( ~ location_abbreviation, scales = "free_y", ncol=4) +
  theme_bw(base_size = 13)
print(p_forecasts)

dev.off()


png('writing/manuscript/forecast_plots_all_locations2.png', width = 10, height = 11, units = "in", res = 300)
location_abbreviations_to_plot <- tail(all_locations_to_plot, length(all_locations_to_plot) - 28)
to_search <- covidData::fips_codes %>% dplyr::filter(!is.na(abbreviation))
locations_to_plot <- purrr::map_chr(
  location_abbreviations_to_plot,
  function(abbr) {
    to_search$location[to_search$abbreviation == abbr]
  }
)

observed <- observed_by_location_target_end_date %>%
  dplyr::filter(
    location %in% locations_to_plot,
    base_target == 'wk ahead cum death'
  ) %>%
  dplyr::mutate(target_end_date = lubridate::ymd(target_end_date)) %>%
  dplyr::left_join(fips_codes, by='location')

plottable_ensemble_predictions <- all_forecasts %>%
  dplyr::filter(
    location %in% locations_to_plot,
    forecast_week_end_date %in% forecast_week_end_dates_to_plot,
    grepl('cum', target)
  ) %>%
  dplyr::mutate(
    endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
    quantile = as.numeric(quantile),
    alpha = ifelse(
      endpoint_type == 'lower',
      format(2*quantile, digits=3, nsmall=3),
      format(2*(1-quantile), digits=3, nsmall=3)),
    interval_level = ifelse(
      endpoint_type == 'lower',
      paste0(format((1 - 2*quantile)*100, digits=2, nsmall=0), '%'),
      paste0(format((1 - 2*(1-quantile))*100, digits=2, nsmall=0), '%')
    )
  ) %>%
  dplyr::filter(alpha != "1.000") %>%
  dplyr::select(-quantile) %>%
  tidyr::pivot_wider(names_from='endpoint_type', values_from='value') %>%
  dplyr::filter(interval_level %in% c('  0%', ' 50%', ' 80%', ' 95%')) %>%
  dplyr::mutate(
    interval_level = factor(interval_level, levels = c(' 95%', ' 80%', ' 50%', '  0%'))
  ) %>%
  dplyr::mutate(
    location_abbreviation = factor(
      location_abbreviation,
      levels = location_abbreviations_to_plot),
    week_and_level = factor(
      paste0(lubridate::ymd(forecast_week_end_date)+2, ':', interval_level),
      levels = paste0(rep(lubridate::ymd(forecast_week_end_dates_to_plot) + 2, each=3), c(': 95%', ': 80%', ': 50%'))
    )
  )

for(fwed in forecast_week_end_dates_to_plot) {
  fwed <- lubridate::ymd(fwed)
  interp_endpoint_dates <- c(fwed, fwed + 7)
  interp_date <- fwed + 2
  start_intervals <- purrr::map_dfr(
    locations_to_plot,
    function(location) {
      interp_endpoint_obs <- observed %>%
        dplyr::filter(
          location == UQ(location),
          target_end_date %in% interp_endpoint_dates,
          base_target == 'wk ahead cum death'
        ) %>%
        dplyr::arrange(target_end_date) %>%
        dplyr::pull(observed)
      interp_obs <- approx(
        x = interp_endpoint_dates,
        y = interp_endpoint_obs,
        xout = interp_date
      )
      tidyr::expand_grid(
        forecast_week_end_date = as.character(fwed),
        target_end_date = as.character(interp_date),
        location = location,
        location_abbreviation = plottable_ensemble_predictions %>%
          dplyr::filter(location == UQ(location)) %>%
          head(1) %>%
          dplyr::pull(location_abbreviation),
        lower = interp_obs$y,
        upper = interp_obs$y,
        week_and_level = factor(
          paste0(as.character(fwed+2), c(': 95%', ': 80%', ': 50%')),
          levels = paste0(rep(lubridate::ymd(forecast_week_end_dates_to_plot) + 2, each=3), c(': 95%', ': 80%', ': 50%'))
        )
      )
    })
  plottable_ensemble_predictions <- dplyr::bind_rows(
    plottable_ensemble_predictions,
    start_intervals
  )
}

plottable_ensemble_predictions <- plottable_ensemble_predictions %>%
  dplyr::mutate(
    forecast_week_end_date = as.character(lubridate::ymd(forecast_week_end_date) + 2)
  )

points_to_plot <- dplyr::bind_rows(
  observed %>%
    dplyr::filter(
      location %in% locations_to_plot,
      target_end_date > lubridate::ymd('2020-03-15'),
      base_target == 'wk ahead cum death'
    ) %>%
    dplyr::mutate(
      point_type = 'Observed',
      location_abbreviation = factor(
        location_abbreviation,
        levels = location_abbreviations_to_plot)
    ),
  all_forecasts %>%
    dplyr::filter(
      location %in% locations_to_plot,
      forecast_week_end_date %in% forecast_week_end_dates_to_plot,
      grepl('cum', target),
      quantile == 0.5) %>%
    dplyr::mutate(
      forecast_week_end_date = as.character(lubridate::ymd(forecast_week_end_date) + 2),
      location_abbreviation = factor(
        location_abbreviation,
        levels = location_abbreviations_to_plot),
      target_end_date = lubridate::ymd(target_end_date),
      observed = value,
      point_type = 'Point Prediction'
    )
)

p_forecasts <- ggplot() +
  geom_vline(
    data = data.frame(
      x = lubridate::ymd(forecast_week_end_dates_to_plot) + 2),
    mapping = aes(xintercept = x, color = factor(x)),
    size = 1,
    linetype = 2,
    show.legend = FALSE
  ) +
  geom_ribbon(
    data = plottable_ensemble_predictions,
    mapping = aes(x = lubridate::ymd(target_end_date),
                  ymin=lower, ymax=upper,
                  fill=week_and_level)) +
  geom_line(
    data=observed %>%
      dplyr::filter(
        location %in% locations_to_plot,
        target_end_date > lubridate::ymd('2020-03-15'),
        base_target == 'wk ahead cum death'
      ) %>%
      dplyr::mutate(
        location_abbreviation = factor(
          location_abbreviation,
          levels = location_abbreviations_to_plot)
      ),
    mapping = aes(x = target_end_date, y = observed)) +
  geom_line(
    data = all_forecasts %>%
      dplyr::filter(
        location %in% locations_to_plot,
        forecast_week_end_date %in% forecast_week_end_dates_to_plot,
        grepl('cum', target),
        quantile == 0.5) %>%
      dplyr::mutate(
        forecast_week_end_date = as.character(lubridate::ymd(forecast_week_end_date) + 2),
        location_abbreviation = factor(
          location_abbreviation,
          levels = location_abbreviations_to_plot)
      ),
    mapping = aes(
      x = lubridate::ymd(target_end_date),
      y = value,
      color = factor(forecast_week_end_date),
      group = factor(forecast_week_end_date)),
    size = 1) +
  geom_point(
    data= points_to_plot,
    mapping = aes(
      x = target_end_date, y = observed,
      shape = point_type, size = point_type),
  ) +
  scale_color_manual(
    "Date Forecast\nMade",
    values = c('#045a8d', '#800026')
  ) +
  scale_fill_manual(
    "Interval Level",
    #        values = c('#a6bddb', '#3690c0', '#0570b0')
    values = c('#bdc9e1', '#74a9cf', '#2b8cbe', '#fed976', '#fd8d3c', '#e31a1c')
  ) +
  scale_shape_manual(
    'Observed Data or\nPoint Prediction',
    values = c('Observed' = 16, 'Point Prediction' = 88)
  ) +
  scale_size_manual(
    'Observed Data or\nPoint Prediction',
    values = c('Observed' = 2, 'Point Prediction' = 4)
  ) +
  #      scale_linetype_discrete("Observation or\nPrediction Date") +
  #      scale_shape_discrete("Observation or\nPrediction Date") +
  xlab('Date') +
  ylab('Cumulative Deaths') +
  facet_wrap( ~ location_abbreviation, scales = "free_y", ncol=4) +
  theme_bw(base_size = 13)
print(p_forecasts)

dev.off()



# Table 1
# Coverage rates by horizon, subset of target weeks for which all horizons are available
# Top section (unmodified ensemble forecasts)
coverage_fewer_weeks <- all_scores %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 2)),
    target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon,
    base_target = ifelse(
      grepl('cum', target),
      'Cumulative',
      'Incident'
    )
  ) %>%
  dplyr::filter(
    num_models > 1,
    target_end_date >= '2020-05-23', target_end_date <= last_target_date) %>%
  dplyr::group_by(horizon, base_target) %>%
  dplyr::summarize_at(vars(starts_with('coverage_')), mean) %>%
  tidyr::pivot_longer(
    cols = colnames(.)[!(colnames(.) %in% c('horizon', 'base_target'))],
    names_to = 'nominal_coverage',
    names_prefix = 'coverage_',
    values_to = 'empirical_coverage') %>%
  dplyr::mutate(nominal_coverage = as.numeric(nominal_coverage))
coverage_fewer_weeks %>%
  filter(nominal_coverage %in% c(0.5, 0.95), base_target == 'Cumulative') %>%
  pivot_wider(names_from = 'horizon', values_from = empirical_coverage)


# Coverage for table 1 bottom section -- ensemble forecasts after rounding
rounded_coverage_fewer_weeks <- all_scores_rounded %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 2)),
    target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon,
    base_target = ifelse(
      grepl('cum', target),
      'Cumulative',
      'Incident'
    )
  ) %>%
  dplyr::filter(
    num_models > 1,
    target_end_date >= '2020-05-23', target_end_date <= last_target_date) %>%
  dplyr::group_by(horizon, base_target) %>%
  dplyr::summarize_at(vars(starts_with('coverage_')), mean) %>%
  tidyr::pivot_longer(
    cols = colnames(.)[!(colnames(.) %in% c('horizon', 'base_target'))],
    names_to = 'nominal_coverage',
    names_prefix = 'coverage_',
    values_to = 'empirical_coverage') %>%
  dplyr::mutate(nominal_coverage = as.numeric(nominal_coverage))
rounded_coverage_fewer_weeks %>%
  filter(nominal_coverage %in% c(0.5, 0.95), base_target == 'Cumulative') %>%
  pivot_wider(names_from = 'horizon', values_from = empirical_coverage)


# Understand differences between original and rounded
differential_coverage_fewer_weeks <- all_scores %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 2)),
    target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon,
    base_target = ifelse(
      grepl('cum', target),
      'Cumulative',
      'Incident'
    )
  ) %>%
  dplyr::filter(
    base_target == 'Cumulative',
    num_models > 1,
    target_end_date >= '2020-05-23', target_end_date <= last_target_date) %>%
  dplyr::left_join(
    all_scores_rounded %>%
      dplyr::mutate(
        horizon = as.integer(substr(target, 1, 2)),
        target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon,
        base_target = ifelse(
          grepl('cum', target),
          'Cumulative',
          'Incident'
        )
      ) %>%
      dplyr::filter(
        base_target == 'Cumulative',
        num_models > 1,
        target_end_date >= '2020-05-23', target_end_date <= last_target_date),
    by = c('location', 'forecast_week_end_date', 'target')
  )

differential_coverage_fewer_weeks %>%
  dplyr::filter(coverage_0.95.x != coverage_0.95.y) %>%
  pull(observed_inc.x) %>%
  table()

differential_coverage_fewer_weeks %>%
  dplyr::filter(coverage_0.95.x != coverage_0.95.y) %>%
  nrow()

99/120

differential_coverage_fewer_weeks %>%
  dplyr::filter(coverage_0.10.x != coverage_0.10.y) %>%
  pull(observed_inc.x) %>%
  table()



# Supplemental Table 2: Coverage rates by horizon, all weeks
coverage_all_weeks <- all_scores %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 2)),
    target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon,
    base_target = ifelse(
      grepl('cum', target),
      'Cumulative',
      'Incident'
    )
  ) %>%
  dplyr::filter(
    num_models > 1,
    target_end_date <= last_target_date) %>%
  dplyr::group_by(horizon, base_target) %>%
  dplyr::summarize_at(vars(starts_with('coverage_')), mean) %>%
  tidyr::pivot_longer(
    cols = colnames(.)[!(colnames(.) %in% c('horizon', 'base_target'))],
    names_to = 'nominal_coverage',
    names_prefix = 'coverage_',
    values_to = 'empirical_coverage') %>%
  dplyr::mutate(nominal_coverage = as.numeric(nominal_coverage))
coverage_all_weeks %>%
  filter(nominal_coverage %in% c(0.5, 0.95), base_target == 'Cumulative') %>%
  pivot_wider(names_from = 'horizon', values_from = empirical_coverage)


# Coverage for supplemental table 2 bottom section -- ensemble forecasts after rounding
rounded_coverage_all_weeks <- all_scores_rounded %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 2)),
    target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon,
    base_target = ifelse(
      grepl('cum', target),
      'Cumulative',
      'Incident'
    )
  ) %>%
  dplyr::filter(
    num_models > 1,
    target_end_date <= last_target_date) %>%
  dplyr::group_by(horizon, base_target) %>%
  dplyr::summarize_at(vars(starts_with('coverage_')), mean) %>%
  tidyr::pivot_longer(
    cols = colnames(.)[!(colnames(.) %in% c('horizon', 'base_target'))],
    names_to = 'nominal_coverage',
    names_prefix = 'coverage_',
    values_to = 'empirical_coverage') %>%
  dplyr::mutate(nominal_coverage = as.numeric(nominal_coverage))
rounded_coverage_all_weeks %>%
  filter(nominal_coverage %in% c(0.5, 0.95), base_target == 'Cumulative') %>%
  pivot_wider(names_from = 'horizon', values_from = empirical_coverage)


rounded_coverage_nonzero_inc_weeks <- all_scores_rounded %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 2)),
    target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon,
    base_target = ifelse(
      grepl('cum', target),
      'Cumulative',
      'Incident'
    )
  ) %>%
  dplyr::filter(
    num_models > 1,
    observed_inc > 0,
    target_end_date >= '2020-05-23', target_end_date <= last_target_date) %>%
  dplyr::group_by(horizon, base_target) %>%
  dplyr::summarize_at(vars(starts_with('coverage_')), mean) %>%
  tidyr::pivot_longer(
    cols = colnames(.)[!(colnames(.) %in% c('horizon', 'base_target'))],
    names_to = 'nominal_coverage',
    names_prefix = 'coverage_',
    values_to = 'empirical_coverage') %>%
  dplyr::mutate(nominal_coverage = as.numeric(nominal_coverage))
rounded_coverage_all_weeks %>%
  filter(nominal_coverage %in% c(0.5, 0.95), base_target == 'Cumulative') %>%
  pivot_wider(names_from = 'horizon', values_from = empirical_coverage)


# Supplemental Table 1
maes_by_location <- all_scores %>%
  dplyr::left_join(
    covidData::fips_codes %>%
      dplyr::select(location, location_abbreviation = abbreviation),
    by = 'location'
  ) %>%
  dplyr::filter(num_models > 1) %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 2)),
    target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon
  ) %>%
  dplyr::filter(
    target_end_date >= '2020-05-23', target_end_date <= '2020-07-11',
    grepl('cum', target)) %>%
  dplyr::group_by(location, location_abbreviation, horizon) %>%
  dplyr::summarize(mae = mean(wis_1)) %>%
  tidyr::pivot_wider(names_from = 'horizon', values_from = 'mae') %>%
  dplyr::left_join(
    observed_by_location_target_end_date %>%
      dplyr::filter(base_target == 'wk ahead inc death') %>%
      dplyr::group_by(location) %>%
      dplyr::summarize(
        max_inc = max(observed, na.rm = TRUE)
      )
  ) %>%
  dplyr::left_join(
    observed_by_location_target_end_date %>%
      dplyr::filter(
        target_end_date == '2020-07-25',
        base_target == 'wk ahead cum death') %>%
      dplyr::transmute(
        location = location,
        cum_death = observed
      )
  ) %>%
  arrange(desc(cum_death))

maes_by_location <- dplyr::bind_rows(
  all_scores %>%
    dplyr::left_join(
      covidData::fips_codes %>%
        dplyr::select(location, location_abbreviation = abbreviation),
      by = 'location'
    ) %>%
    dplyr::filter(num_models > 1) %>%
    dplyr::mutate(
      horizon = as.integer(substr(target, 1, 2)),
      target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon
    ) %>%
    dplyr::filter(
      target_end_date >= '2020-05-23', target_end_date <= '2020-07-11',
      grepl('cum', target)) %>%
    dplyr::group_by(horizon) %>%
    dplyr::summarize(mae = mean(wis_1)) %>%
    tidyr::pivot_wider(names_from = 'horizon', values_from = 'mae'),
  maes_by_location
) %>%
  dplyr::select(location_abbreviation, `1`, `2`, `3`, `4`, max_inc, cum_death) %>%
  dplyr::mutate(
    `1` = round(`1`, 1),
    `2` = round(`2`, 1),
    `3` = round(`3`, 1),
    `4` = round(`4`, 1)
  )
write_csv(maes_by_location, 'writing/manuscript/maes_by_location.csv')

# Supplemental Figure 3
coverage_for_plot <- bind_rows(
  coverage_fewer_weeks %>% mutate(forecast_type = 'Original Forecasts'),
  rounded_coverage_fewer_weeks %>% mutate(forecast_type = 'Rounded Forecasts'),
  rounded_coverage_nonzero_inc_weeks %>% mutate(forecast_type = 'Rounded Forecasts,\nWeeks with No Incident Deaths')
)

png('writing/manuscript/all_interval_calibration_by_horizon.png', width = 8, height = 7, units = "in", res = 300)
ggplot() +
  geom_line(
    data = dplyr::bind_rows(
      coverage_for_plot %>%
        dplyr::filter(base_target == 'Cumulative') %>%
        dplyr::mutate(
          nominal_coverage = factor(nominal_coverage, levels = rev(sort(unique(coverage_for_plot$nominal_coverage)))),
          type = 'Empirical'
        ),
      tidyr::expand_grid(
        empirical_coverage = c(seq(from=0.1, to=0.9, by=0.1), 0.95, 0.98),
        horizon = 1:4,
        forecast_type = c('Original Forecasts', 'Rounded Forecasts', 'Rounded Forecasts,\nWeeks with No Incident Deaths')
      ) %>%
        dplyr::mutate(
          nominal_coverage = factor(empirical_coverage, levels = rev(sort(unique(empirical_coverage)))),
          type = 'Nominal'
        )
    ),
    mapping = aes(x = horizon, y = empirical_coverage,
                  color = nominal_coverage, linetype = type,
                  group = paste0(nominal_coverage, type))) +
  geom_label(
    data = coverage_for_plot %>%
      dplyr::filter(base_target == 'Cumulative', horizon %in% c(2, 3)) %>%
      dplyr::group_by(nominal_coverage, forecast_type) %>%
      dplyr::summarize(
        mean_empirical_coverage = mean(empirical_coverage)
      ) %>%
      dplyr::mutate(
        mean_empirical_coverage = ifelse(
          nominal_coverage == 0.95,
          mean_empirical_coverage + 0.005,
          ifelse(
            nominal_coverage == 0.98,
            mean_empirical_coverage + 0.015,
            mean_empirical_coverage
          )
        ),
        horizon = 2.5
      ),
    mapping = aes(x = horizon, y = mean_empirical_coverage, label = nominal_coverage)) +
  scale_linetype_discrete("Coverage Type") +
  scale_color_viridis_d("Nominal\nCoverage\nRate", begin = 0.2, end = 0.9) +
  scale_y_continuous(
    name = "Coverage Rate",
    breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.98, 1.0),
    minor_breaks = NULL,
    limits = c(0, 1),
    expand = c(0, 0)) +
  scale_x_continuous(
    name = "Forecast Horizon",
    minor_breaks = NULL) +
  facet_wrap( ~ forecast_type) +
  theme_bw()
dev.off()


## Supplemental Figure 4: One-sided calibration
one_sided_coverage_fewer_weeks <- all_scores %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 2)),
    target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon,
    base_target = ifelse(
      grepl('cum', target),
      'Cumulative',
      'Incident'
    )
  ) %>%
  dplyr::filter(
    num_models > 1,
    base_target == 'Cumulative',
    target_end_date >= '2020-05-23', target_end_date <= last_target_date) %>%
  dplyr::group_by(horizon, base_target) %>%
  dplyr::summarize_at(vars(starts_with('one_sided_coverage_')), mean) %>%
  tidyr::pivot_longer(
    cols = colnames(.)[!(colnames(.) %in% c('horizon', 'base_target'))],
    names_to = 'nominal_coverage',
    names_prefix = 'one_sided_coverage_',
    values_to = 'empirical_coverage') %>%
  dplyr::mutate(nominal_coverage = as.numeric(nominal_coverage))


# Coverage for table 1 bottom section -- ensemble forecasts after rounding
one_sided_rounded_coverage_fewer_weeks <- all_scores_rounded %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 2)),
    target_end_date = lubridate::ymd(forecast_week_end_date) + 7*horizon,
    base_target = ifelse(
      grepl('cum', target),
      'Cumulative',
      'Incident'
    )
  ) %>%
  dplyr::filter(
    num_models > 1,
    base_target == 'Cumulative',
    target_end_date >= '2020-05-23', target_end_date <= last_target_date) %>%
  dplyr::group_by(horizon, base_target) %>%
  dplyr::summarize_at(vars(starts_with('one_sided_coverage_')), mean) %>%
  tidyr::pivot_longer(
    cols = colnames(.)[!(colnames(.) %in% c('horizon', 'base_target'))],
    names_to = 'nominal_coverage',
    names_prefix = 'one_sided_coverage_',
    values_to = 'empirical_coverage') %>%
  dplyr::mutate(nominal_coverage = as.numeric(nominal_coverage))

one_sided_coverage_for_plot <- bind_rows(
  one_sided_coverage_fewer_weeks %>% mutate(forecast_type = 'Original Forecasts'),
  one_sided_rounded_coverage_fewer_weeks %>% mutate(forecast_type = 'Rounded Forecasts')
)

ggplot(
  data = one_sided_coverage_for_plot %>%
    dplyr::ungroup() %>%
    dplyr::mutate(horizon = paste0('Horizon ', horizon)),
  mapping = aes(
    x = nominal_coverage,
    y = empirical_coverage,
    color = forecast_type)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_line(mapping = aes(linetype = forecast_type)) +
  geom_point(mapping = aes(shape = forecast_type)) +
  facet_wrap( ~ horizon) +
  theme_bw()




