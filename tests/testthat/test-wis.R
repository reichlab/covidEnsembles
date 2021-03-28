context("qra_ew")
library(covidEnsembles)
library(dplyr)


test_that("wis works, median only", {
  y <- c(1, -15, 22)
  quantiles <- matrix(c(1, 2, 3), ncol = 1)
  quantile_probs <- 0.5

  attributes(quantiles) <- c(
    attributes(quantiles),
    list(
      row_index = data.frame(
        letters[1:3],
        stringsAsFactors = FALSE),
      col_index = data.frame(
        q_prob = as.character(quantile_probs),
        model = 'm1',
        stringsAsFactors = FALSE),
      model_col = 'model',
      quantile_name_col = 'q_prob',
      quantile_value_col = 'q_val'
    )
  )

  actual <- wis(y=y, qfm=quantiles)
  expected <- abs(y - quantiles[, 1])

  expect_identical(actual, expected)
})


test_that("wis works, 1 interval only", {
  y <- c(1, -15, 22)
  quantiles <- rbind(c(0, 2), c(1, 2), c(0, 3))
  quantile_probs <- c(0.25, 0.75)
  attributes(quantiles) <- c(
    attributes(quantiles),
    list(
      row_index = data.frame(
        letters[1:3],
        stringsAsFactors = FALSE),
      col_index = data.frame(
        q_prob = as.character(quantile_probs),
        model = 'm1',
        stringsAsFactors = FALSE),
      model_col = 'model',
      quantile_name_col = 'q_prob',
      quantile_value_col = 'q_val'
    )
  )

  alpha <- 0.5

  actual <- wis(y=y, qfm=quantiles)
  expected <- (quantiles[, 2] - quantiles[, 1])*(alpha/2) + c(0, 1-(-15), 22-3)

  expect_identical(actual, expected)
})


test_that("wis works, 1 interval and median", {
  y <- c(1, -15, 22)
  quantiles <- rbind(c(0, 1, 2), c(1, 2, 2), c(0, 3, 3))
  quantile_probs <- c(0.25, 0.5, 0.75)
  attributes(quantiles) <- c(
    attributes(quantiles),
    list(
      row_index = data.frame(
        letters[1:3],
        stringsAsFactors = FALSE),
      col_index = data.frame(
        q_prob = as.character(quantile_probs),
        model = 'm1',
        stringsAsFactors = FALSE),
      model_col = 'model',
      quantile_name_col = 'q_prob',
      quantile_value_col = 'q_val'
    )
  )

  alpha <- 0.5

  actual <- wis(y=y, qfm=quantiles)
  expected <- ( 1 / (1 + 0.5)) * (
    0.5 * abs(y - quantiles[, 2]) +
    (quantiles[, 3] - quantiles[, 1])*(alpha/2) + c(0, 1-(-15), 22-3)
  )

  expect_identical(actual, expected)
})


test_that("wis works, 2 intervals and median", {
  y <- c(1, -15, 22)
  quantiles <- rbind(c(-1, 0, 1, 2, 3), c(-2, 1, 2, 2, 4), c(-2, 0, 3, 3, 4))
  quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  attributes(quantiles) <- c(
    attributes(quantiles),
    list(
      row_index = data.frame(
        letters[1:3],
        stringsAsFactors = FALSE),
      col_index = data.frame(
        q_prob = as.character(quantile_probs),
        model = 'm1',
        stringsAsFactors = FALSE),
      model_col = 'model',
      quantile_name_col = 'q_prob',
      quantile_value_col = 'q_val'
    )
  )

  alpha1 <- 0.2
  alpha2 <- 0.5

  actual <- wis(y=y, qfm=quantiles)
  expected <- (1 / (2 + 0.5)) * (
    0.5 * abs(y - quantiles[, 3]) +
    (quantiles[, 5] - quantiles[, 1])*(alpha1/2) + c(0, (-2)-(-15), 22-4) +
    (quantiles[, 4] - quantiles[, 2])*(alpha2/2) + c(0, 1-(-15), 22-3)
  )

  expect_identical(actual, expected)
})
