context("qra_ew")
library(covidEnsembles)
library(dplyr)


test_that("fit_qra_ew works", {
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

  actual <- estimate_qra(
    qfm_train = forecast_matrix,
    combine_method = 'ew'
  )

  expected <- structure(
    list(
      coefficients=data.frame(
        model=paste0('m', 1:3),
        beta=1/3,
        stringsAsFactors = FALSE
      ),
      intercept=data.frame(
        beta=0,
        stringsAsFactors = FALSE
      )
    ),
    convex=FALSE,
    class='qra_fit'
  )

  expect_equal(
    actual,
    expected
  )
})
