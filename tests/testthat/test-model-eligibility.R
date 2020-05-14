context("model eligibility")
library(covidEnsembles)
library(dplyr)

test_that("calc_forecast_missingness works: lookback_length 0, none missing", {
  forecast_df <- expand.grid(
    unit = letters[1:4],
    forecast_week_end_date = c('2020-04-18', '2020-04-25', '2020-05-02'),
    model = paste0('m', 1:3),
    q_prob = c(0.025, 0.5, 0.975),
    stringsAsFactors = FALSE
  )
  forecast_df$q_val <- rnorm(nrow(forecast_df))
  forecast_df$q_val[
    forecast_df$unit == 'b' &
    forecast_df$forecast_week_end_date == '2020-04-18' &
    forecast_df$model == 'm2' &
    forecast_df$q_prob == 0.975
  ] <- NA_real_

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df,
    model_col = 'model',
    id_cols = c('unit', 'forecast_week_end_date'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val'
  )

  actual <- calc_forecast_missingness(
    qfm=forecast_matrix,
    lookback_length=0,
    model_id_name='model'
  )

  expected <- expand.grid(
    unit = letters[1:4],
    model = paste0('m', 1:3),
    missingness_eligibility = 'eligible',
    stringsAsFactors = FALSE
  )

  expect_equal(
    actual,
    expected
  )
})


test_that("calc_forecast_missingness works: lookback_length 1, none missing", {
  forecast_df <- expand.grid(
    unit = letters[1:4],
    forecast_week_end_date = c('2020-04-18', '2020-04-25', '2020-05-02'),
    model = paste0('m', 1:3),
    q_prob = c(0.025, 0.5, 0.975),
    stringsAsFactors = FALSE
  )
  forecast_df$q_val <- rnorm(nrow(forecast_df))
  forecast_df$q_val[
    forecast_df$unit == 'b' &
      forecast_df$forecast_week_end_date == '2020-04-18' &
      forecast_df$model == 'm2' &
      forecast_df$q_prob == 0.975
    ] <- NA_real_

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df,
    model_col = 'model',
    id_cols = c('unit', 'forecast_week_end_date'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val'
  )

  actual <- calc_forecast_missingness(
    qfm=forecast_matrix,
    lookback_length=1,
    model_id_name='model'
  )

  expected <- expand.grid(
    unit = letters[1:4],
    model = paste0('m', 1:3),
    missingness_eligibility = 'eligible',
    stringsAsFactors = FALSE
  )

  expect_equal(
    actual,
    expected
  )
})


test_that("calc_forecast_missingness works: lookback_length 2, missing", {
  forecast_df <- expand.grid(
    unit = letters[1:4],
    forecast_week_end_date = c('2020-04-18', '2020-04-25', '2020-05-02'),
    model = paste0('m', 1:3),
    q_prob = c(0.025, 0.5, 0.975),
    stringsAsFactors = FALSE
  )
  forecast_df$q_val <- rnorm(nrow(forecast_df))
  forecast_df$q_val[
    forecast_df$unit == 'b' &
      forecast_df$forecast_week_end_date == '2020-04-18' &
      forecast_df$model == 'm2' &
      forecast_df$q_prob == 0.975
    ] <- NA_real_

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df,
    model_col = 'model',
    id_cols = c('unit', 'forecast_week_end_date'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val'
  )

  actual <- calc_forecast_missingness(
    qfm=forecast_matrix,
    lookback_length=2,
    model_id_name='model'
  )

  expected <- expand.grid(
    unit = letters[1:4],
    model = paste0('m', 1:3),
    missingness_eligibility = 'eligible',
    stringsAsFactors = FALSE
  )
  expected$missingness_eligibility[
    expected$unit == 'b' &
    expected$model == 'm2'
  ] <- 'missing required forecasts'

  expect_equal(
    actual,
    expected
  )
})


test_that("calc_q10_check works", {
  forecast_df <- expand.grid(
    unit = letters[1:4],
    forecast_week_end_date = c('2020-04-18', '2020-04-25', '2020-05-02'),
    target = paste0(1:4, ' wk ahead cum death'),
    model = paste0('m', 1:3),
    q_prob = c(0.025, 0.1, 0.5, 0.9, 0.975),
    stringsAsFactors = FALSE
  )

  forecast_df$q_val <- 5
  forecast_df$q_val[
    forecast_df$unit %in% c('b', 'c') &
      forecast_df$forecast_week_end_date == '2020-05-02' &
      forecast_df$model == 'm2' &
      forecast_df$q_prob == 0.1
    ] <- 0
  forecast_df$q_val[
    forecast_df$unit %in% c('a') &
      forecast_df$forecast_week_end_date == '2020-04-25' &
      forecast_df$model == 'm1' &
      forecast_df$q_prob == 0.1
    ] <- 0

  observed_by_unit_target_end_date <- expand.grid(
    unit = letters[1:4],
    target_end_date = c('2020-04-18', '2020-04-25', '2020-05-02'),
    observed = 2,
    stringsAsFactors = FALSE
  )

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df,
    model_col = 'model',
    id_cols = c('unit', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val'
  )

  actual <- calc_q10_check(
    qfm=forecast_matrix,
    observed_by_unit_target_end_date=observed_by_unit_target_end_date,
    model_id_name='model'
  )

  expected <- expand.grid(
    unit = letters[1:4],
    model = paste0('m', 1:3),
    q10_eligibility = 'eligible',
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) %>%
    arrange(unit, model)

  expected$q10_eligibility[
    expected$unit %in% c('b', 'c') &
      expected$model == 'm2'
    ] <- 'quantile 0.1 of forecast for horizon 1 is less than most recent observed'

  expect_equal(
    actual,
    expected
  )
})

