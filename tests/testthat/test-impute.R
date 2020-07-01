context("imputing missing forecasts")
library(covidEnsembles)

test_that("impute_missing_per_quantile method='mean' works", {
  forecast_matrix <- matrix(1:30, nrow = 5, ncol = 6)
  forecast_matrix[cbind(
    rep(c(1, 2, 3, 4, 2, 4, 3), times=2),
    c(c(1, 1, 1, 1, 2, 2, 3)*2-1, c(1, 1, 1, 1, 2, 2, 3)*2))
  ] <- NA_real_

  qfm <- new_QuantileForecastMatrix(
    qfm=forecast_matrix,
    row_index=data.frame(location = letters[1:5], stringsAsFactors = FALSE),
    col_index=tidyr::expand_grid(model = LETTERS[1:3], quantile = c(0.1, 0.9)),
    model_col='model',
    quantile_name_col='quantile',
    quantile_value_col='value'
  )

  actual <- impute_missing_per_quantile(qfm, impute_method = 'mean')
  actual_qfm_imputed <- actual$qfm_imputed
  actual_weight_transfer <- actual$weight_transfer

  expected_qfm_imputed <- qfm
  expected_qfm_imputed[1, 1] <- mean(expected_qfm_imputed[1, c(3, 5)])
  expected_qfm_imputed[1, 2] <- mean(expected_qfm_imputed[1, c(4, 6)])
  expected_qfm_imputed[2, c(1, 3)] <- expected_qfm_imputed[2, 5]
  expected_qfm_imputed[2, c(2, 4)] <- expected_qfm_imputed[2, 6]
  expected_qfm_imputed[3, c(1, 5)] <- expected_qfm_imputed[3, 3]
  expected_qfm_imputed[3, c(2, 6)] <- expected_qfm_imputed[3, 4]
  expected_qfm_imputed[4, c(1, 3)] <- expected_qfm_imputed[4, 5]
  expected_qfm_imputed[4, c(2, 4)] <- expected_qfm_imputed[4, 6]

  expected_weight_transfer <- matrix(
    c(0.2, 0.0, 0.0,
      0.2 + 0.2/2, 3/5, 1/5,
      0.4 + 0.2/2, 0.4, 0.8),
    nrow = 3, ncol = 3,
    byrow = TRUE
  )

  expect_equal(actual_weight_transfer, expected_weight_transfer)
  expect_equal(actual_qfm_imputed, expected_qfm_imputed)
})



