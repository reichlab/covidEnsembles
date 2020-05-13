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
