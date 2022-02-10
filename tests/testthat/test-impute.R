context("imputing missing forecasts") 
library(covidEnsembles)
library(dplyr)

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

  expected_weight_transfer <- tibble(
    weight_transfer = list(matrix(
    c(0.2, 0.0, 0.0,
      0.2 + 0.2/2, 3/5, 1/5,
      0.4 + 0.2/2, 0.4, 0.8),
    nrow = 3, ncol = 3,
    byrow = TRUE
  )))

  expect_equal(actual_weight_transfer, expected_weight_transfer)
  expect_equal(actual_qfm_imputed, expected_qfm_imputed)
})
  
test_that("impute_missing_per_quantile method='mean' works per group", {
  forecast_matrix <- matrix(1:60, nrow = 10, ncol = 6)
  forecast_matrix[cbind(
    rep(c(
      1, 1, 2, 3, 
      4, 4, 5, 5,
      6, 
      7, 
      9, 10), 
    times=2),
    c(c(
      1, 2, 1, 3, 
      1, 2, 2, 3,
      2,
      1,
      1, 1)*2-1, 
      c(
      1, 2, 1, 3, 
      1, 2, 2, 3,
      2,
      1,
      1, 1)*2)
  )] <- NA_real_

  qfm <- new_QuantileForecastMatrix(
    qfm=forecast_matrix,
    row_index=data.frame(
      location = letters[c(
        1, 1, 1,
        2, 2,
        3, 
        4, 4,
        5, 5
      )], 
      target = letters[c(
        6, 7, 8,
        6, 7,
        7,
        7, 8,
        6, 8)],
      stringsAsFactors = FALSE),
    col_index=tidyr::expand_grid(model = LETTERS[1:3], quantile = c(0.1, 0.9)),
    model_col='model',
    quantile_name_col='quantile',
    quantile_value_col='value'
  )

  actual <- impute_missing_per_quantile(qfm, impute_method = 'mean', 
    weight_transfer_per_group = TRUE)
  actual_qfm_imputed <- actual$qfm_imputed
  actual_weight_transfer <- actual$weight_transfer

  expected_qfm_imputed <- qfm

  # loc a
  expected_qfm_imputed[1, c(1, 3)] <- expected_qfm_imputed[1, 5]
  expected_qfm_imputed[1, c(2, 4)] <- expected_qfm_imputed[1, 6]
  expected_qfm_imputed[2, 1] <- mean(expected_qfm_imputed[2, c(3, 5)])  
  expected_qfm_imputed[2, 2] <- mean(expected_qfm_imputed[2, c(4, 6)])  
  expected_qfm_imputed[3, 5] <- mean(expected_qfm_imputed[3, c(1, 3)])
  expected_qfm_imputed[3, 6] <- mean(expected_qfm_imputed[3, c(2, 4)])
  
  # loc b
  expected_qfm_imputed[4, c(1, 3)] <- expected_qfm_imputed[4, 5]
  expected_qfm_imputed[4, c(2, 4)] <- expected_qfm_imputed[4, 6]
  expected_qfm_imputed[5, c(3, 5)] <- expected_qfm_imputed[5, 1]
  expected_qfm_imputed[5, c(4, 6)] <- expected_qfm_imputed[5, 2]

  # loc c
  expected_qfm_imputed[6, 3] <- mean(expected_qfm_imputed[6, c(1, 5)])
  expected_qfm_imputed[6, 4] <- mean(expected_qfm_imputed[6, c(2, 6)])

  # loc d (only one row needs imputing)
  expected_qfm_imputed[7, 1] <- mean(expected_qfm_imputed[7, c(3, 5)])  
  expected_qfm_imputed[7, 2] <- mean(expected_qfm_imputed[7, c(4, 6)])  

  # loc e
  expected_qfm_imputed[9, 1] <- mean(expected_qfm_imputed[9, c(3, 5)])  
  expected_qfm_imputed[9, 2] <- mean(expected_qfm_imputed[9, c(4, 6)])  
  expected_qfm_imputed[10, 1] <- mean(expected_qfm_imputed[10, c(3, 5)])  
  expected_qfm_imputed[10, 2] <- mean(expected_qfm_imputed[10, c(4, 6)])  

  wt_a001 <- matrix(c(0,0,1,0,0,1,0,0,1),3,3)
  wt_a011 <- matrix(c(c(0,1,1)/2,0,1,0,0,0,1),3,3)
  wt_a110 <- matrix(c(1,0,0,0,1,0,c(1,1,0)/2),3,3)

  expected_weight_transfer_a <- (wt_a001 + wt_a110 + wt_a011)/3

  wt_b001 <- matrix(c(0,0,1,0,0,1,0,0,1),3,3)
  wt_b100 <- matrix(c(1,0,0,1,0,0,1,0,0),3,3)

  expected_weight_transfer_b <- (wt_b001 + wt_b100)/2

  expected_weight_transfer_c <- matrix(c(1,0,0,c(1,0,1)/2,0,0,1),3,3)
  
  expected_weight_transfer_d <- (matrix(c(c(0,1,1)/2,0,1,0,0,0,1),3,3) + diag(3))/2

  expected_weight_transfer_e <- matrix(c(c(0,1,1)/2,0,1,0,0,0,1),3,3)

  expected_weight_transfer <- tibble(
    location = letters[1:5],
    row_inds_per_group = list(
      1:3,
      4:5,
      6,
      7:8,
      9:10
    ),
    weight_transfer = list(
      expected_weight_transfer_a,
      expected_weight_transfer_b,
      expected_weight_transfer_c,
      expected_weight_transfer_d,
      expected_weight_transfer_e
    )
  )

  # overall weight transer - not using now...
  # expected_weight_transfer_overall <- (
  #   matrix(c(0,0,1,0,0,1,0,0,1),3,3)*2 + # rows 1, 4
  #   matrix(c(c(0,1,1)/2,0,1,0,0,0,1),3,3)*4 + # rows 2, 7, 9, 10
  #   matrix(c(1,0,0,0,1,0,c(1,1,0)/2),3,3) + # row 3
  #   matrix(c(1,0,0,1,0,0,1,0,0),3,3) + # row 5
  #   matrix(c(1,0,0,c(1,0,1)/2,0,0,1),3,3) + # row 6
  #   diag(3) # row 8
  # ) / 10

  expect_equal(actual_weight_transfer, expected_weight_transfer)
  expect_equal(actual_qfm_imputed, expected_qfm_imputed)
})





