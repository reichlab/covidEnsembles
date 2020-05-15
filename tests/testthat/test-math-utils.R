context("math utils")
library(covidEnsembles)

test_that("softmax_matrix_rows works", {
  X <- matrix(seq(from = -3, length = 12), nrow = 4)

  actual <- softmax_matrix_rows(X)
  expected <- exp(X)
  for(i in seq_len(nrow(X))) {
    expected[i, ] <- expected[i, ] / sum(expected[i, ])
  }

  expect_equal(
    actual,
    expected
  )

  expect_equal(
    apply(actual, 1, sum),
    rep(1.0, nrow(actual))
  )
})
