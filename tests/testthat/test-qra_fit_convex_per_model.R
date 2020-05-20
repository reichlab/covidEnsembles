context("qra_convex_per_model")
library(covidEnsembles)
library(dplyr)


test_that("init_par_constructor_convex_per_model works", {
  forecast_df <- expand.grid(
    unit = letters[1:4],
    forecast_week_end_date = c('2020-04-18', '2020-04-25', '2020-05-02'),
    target = paste0(1:4, ' wk ahead cum death'),
    model = paste0('m', 1:3),
    q_prob = c(0.025, 0.1, 0.5, 0.9, 0.975),
    stringsAsFactors = FALSE
  )
  forecast_df$q_val <- rnorm(nrow(forecast_df))

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df,
    model_col = 'model',
    id_cols = c('unit', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val'
  )

  actual <- init_par_constructor_convex_per_model(
    qfm_train = forecast_matrix
  )
  expected <- rep(0.0, 3)

  expect_equal(
    actual,
    expected
  )
})


test_that("model_constructor_convex_per_model works", {
  forecast_df <- expand.grid(
    unit = letters[1:4],
    forecast_week_end_date = c('2020-04-18', '2020-04-25', '2020-05-02'),
    target = paste0(1:4, ' wk ahead cum death'),
    model = paste0('m', 1:3),
    q_prob = c(0.025, 0.1, 0.5, 0.9, 0.975),
    stringsAsFactors = FALSE
  )
  forecast_df$q_val <- rnorm(nrow(forecast_df))

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df,
    model_col = 'model',
    id_cols = c('unit', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val'
  )

  actual <- init_par_constructor_convex_per_model(
    qfm_train = forecast_matrix
  )
  expected <- rep(0.0, 3)

  expect_equal(
    actual,
    expected
  )
})
