context("train/test split")
library(covidEnsembles)
library(dplyr)

# utility function to reset row index row names
reset_row_index_names <- function(qfm) {
  new_row_index <- attr(qfm, "row_index")
  attr(new_row_index, "row.names") <- seq_len(nrow(new_row_index))
  attr(qfm, "row_index") <- new_row_index
  return(qfm)
}

test_that("train_test_split works: window_size equal to available, weekly targets", {
  forecast_df <- expand.grid(
    location = letters[1:4],
    forecast_week_end_date = as.Date(c('2020-04-18', '2020-04-25', '2020-05-02')),
    horizon = 1:4,
    model = paste0('m', 1:3),
    q_prob = c(0.025, 0.5, 0.975),
    stringsAsFactors = FALSE
  ) %>%
  dplyr::mutate(
    target = paste0(horizon, " wk ahead inc death")
  )
  forecast_df$q_val <- rnorm(nrow(forecast_df))

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val'
  )

  actual <- covidEnsembles:::train_test_split(
    forecast_matrix = forecast_matrix,
    forecast_week_end_date = "2020-05-02",
    window_size = 2
  )

  expected_train_df <- forecast_df %>%
    dplyr::filter(
      (forecast_week_end_date == as.Date('2020-04-18') & horizon %in% 1:2) |
      (forecast_week_end_date == as.Date('2020-04-25') & horizon == 1L)
    )
  expected_train_qfm <- new_QuantileForecastMatrix_from_df(
    expected_train_df,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val',
    drop_missing_id_levels = TRUE
  )

  expected_test_df <- forecast_df %>%
    dplyr::filter(forecast_week_end_date == as.Date('2020-05-02'))
  expected_test_qfm <- new_QuantileForecastMatrix_from_df(
    expected_test_df,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val',
    drop_missing_id_levels = TRUE
  )

  # before testing equality, reset irrelevant attributes to standard value
  expected_train_qfm <- reset_row_index_names(expected_train_qfm)
  actual$qfm_train <- reset_row_index_names(actual$qfm_train)
  expected_test_qfm <- reset_row_index_names(expected_test_qfm)
  actual$qfm_test <- reset_row_index_names(actual$qfm_test)
  expect_equal(actual$qfm_train, expected_train_qfm)
  expect_equal(actual$qfm_test, expected_test_qfm)
})


test_that("train_test_split works: window_size shorter than available, weekly targets", {
  forecast_df <- expand.grid(
    location = letters[1:4],
    forecast_week_end_date = as.Date(c('2020-04-18', '2020-04-25', '2020-05-02')),
    horizon = 1:4,
    model = paste0('m', 1:3),
    q_prob = c(0.025, 0.5, 0.975),
    stringsAsFactors = FALSE
  ) %>%
  dplyr::mutate(
    target = paste0(horizon, " wk ahead inc death")
  )
  forecast_df$q_val <- rnorm(nrow(forecast_df))

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val'
  )

  actual <- covidEnsembles:::train_test_split(
    forecast_matrix = forecast_matrix,
    forecast_week_end_date = "2020-05-02",
    window_size = 1
  )

  expected_train_df <- forecast_df %>%
    dplyr::filter(
      (forecast_week_end_date == as.Date('2020-04-25') & horizon == 1L)
    )
  expected_train_qfm <- new_QuantileForecastMatrix_from_df(
    expected_train_df,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val',
    drop_missing_id_levels = TRUE
  )

  expected_test_df <- forecast_df %>%
    dplyr::filter(forecast_week_end_date == as.Date('2020-05-02'))
  expected_test_qfm <- new_QuantileForecastMatrix_from_df(
    expected_test_df,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val',
    drop_missing_id_levels = TRUE
  )

  # before testing equality, reset irrelevant attributes to standard value
  expected_train_qfm <- reset_row_index_names(expected_train_qfm)
  actual$qfm_train <- reset_row_index_names(actual$qfm_train)
  expected_test_qfm <- reset_row_index_names(expected_test_qfm)
  actual$qfm_test <- reset_row_index_names(actual$qfm_test)
  expect_equal(actual$qfm_train, expected_train_qfm)
  expect_equal(actual$qfm_test, expected_test_qfm)
})

test_that("train_test_split works: window_size 0, weekly targets", {
  forecast_df <- expand.grid(
    location = letters[1:4],
    forecast_week_end_date = as.Date(c('2020-04-18', '2020-04-25', '2020-05-02')),
    horizon = 1:4,
    model = paste0('m', 1:3),
    q_prob = c(0.025, 0.5, 0.975),
    stringsAsFactors = FALSE
  ) %>%
  dplyr::mutate(
    target = paste0(horizon, " wk ahead inc death")
  )
  forecast_df$q_val <- rnorm(nrow(forecast_df))

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val'
  )

  actual <- covidEnsembles:::train_test_split(
    forecast_matrix = forecast_matrix,
    forecast_week_end_date = "2020-05-02",
    window_size = 0
  )

  expected_train_df <- forecast_df %>%
    dplyr::filter(
      (forecast_week_end_date == as.Date('2020-05-02') & horizon == 1L)
    )
  expected_train_qfm <- new_QuantileForecastMatrix_from_df(
    expected_train_df,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val',
    drop_missing_id_levels = TRUE
  )

  expected_test_df <- forecast_df %>%
    dplyr::filter(forecast_week_end_date == as.Date('2020-05-02'))
  expected_test_qfm <- new_QuantileForecastMatrix_from_df(
    expected_test_df,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val',
    drop_missing_id_levels = TRUE
  )

  # before testing equality, reset irrelevant attributes to standard value
  expected_train_qfm <- reset_row_index_names(expected_train_qfm)
  actual$qfm_train <- reset_row_index_names(actual$qfm_train)
  expected_test_qfm <- reset_row_index_names(expected_test_qfm)
  actual$qfm_test <- reset_row_index_names(actual$qfm_test)
  expect_equal(actual$qfm_train, expected_train_qfm)
  expect_equal(actual$qfm_test, expected_test_qfm)
})



test_that("train_test_split works: window_size 2, daily targets", {
  forecast_df <- expand.grid(
    location = letters[1:4],
    forecast_week_end_date = as.Date(c('2020-04-18', '2020-04-25', '2020-05-02')),
    horizon = 1:28,
    model = paste0('m', 1:3),
    q_prob = c(0.025, 0.5, 0.975),
    stringsAsFactors = FALSE
  ) %>%
  dplyr::mutate(
    target = paste0(horizon, " day ahead inc death")
  )
  forecast_df$q_val <- rnorm(nrow(forecast_df))

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val'
  )

  actual <- covidEnsembles:::train_test_split(
    forecast_matrix = forecast_matrix,
    forecast_week_end_date = "2020-05-02",
    window_size = 2
  )

  expected_train_df <- forecast_df %>%
    dplyr::filter(
      (forecast_week_end_date == as.Date('2020-04-18') & horizon %in% 1:14) |
      (forecast_week_end_date == as.Date('2020-04-25') & horizon %in% 1:7)
    )
  expected_train_qfm <- new_QuantileForecastMatrix_from_df(
    expected_train_df,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val',
    drop_missing_id_levels = TRUE
  )

  expected_test_df <- forecast_df %>%
    dplyr::filter(forecast_week_end_date == as.Date('2020-05-02'))
  expected_test_qfm <- new_QuantileForecastMatrix_from_df(
    expected_test_df,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'q_prob',
    quantile_value_col = 'q_val',
    drop_missing_id_levels = TRUE
  )

  # before testing equality, reset irrelevant attributes to standard value
  expected_train_qfm <- reset_row_index_names(expected_train_qfm)
  actual$qfm_train <- reset_row_index_names(actual$qfm_train)
  expected_test_qfm <- reset_row_index_names(expected_test_qfm)
  actual$qfm_test <- reset_row_index_names(actual$qfm_test)
  expect_equal(actual$qfm_train, expected_train_qfm)
  expect_equal(actual$qfm_test, expected_test_qfm)
})

