context("qra_fit_common")
library(covidEnsembles)
library(dplyr)


test_that("predict.qra_fit works: single weight per model, no normalization", {
  set.seed(8457)
  forecast_df <- expand.grid(
    unit = letters[1:4],
    forecast_week_end_date = c("2020-04-18", "2020-04-25", "2020-05-02"),
    target = paste0(1:4, " wk ahead cum death"),
    model = paste0("m", 1:3),
    q_prob = c(0.025, 0.1, 0.5, 0.9, 0.975),
    stringsAsFactors = FALSE
  )
  forecast_df$q_val <- rnorm(nrow(forecast_df))

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df,
    model_col = "model",
    id_cols = c("unit", "forecast_week_end_date", "target"),
    quantile_name_col = "q_prob",
    quantile_value_col = "q_val"
  )

  qra_fit <- structure(
    list(
      coefficients = data.frame(
        model = paste0("m", 1:3),
        beta = rexp(3),
        stringsAsFactors = FALSE
      ),
      intercept = data.frame(
        beta = 1.5,
        stringsAsFactors = FALSE
      )
    ),
    convex = FALSE,
    class = "qra_fit"
  )

  actual <- predict(qra_fit, forecast_matrix, sort_quantiles = FALSE)
  # list of null dimnames was causing test to fail pointlessly
  attr(actual, "dimnames") <- NULL

  expected_df <- forecast_df %>%
    left_join(qra_fit$coefficients, by = "model") %>%
    mutate(q_val = q_val * beta) %>%
    group_by(unit, forecast_week_end_date, target, q_prob) %>%
    summarize(
      q_val = sum(q_val) + qra_fit$intercept$beta
    ) %>%
    mutate(model = "qra")

  expected <- new_QuantileForecastMatrix_from_df(
    expected_df,
    model_col = "model",
    id_cols = c("unit", "forecast_week_end_date", "target"),
    quantile_name_col = "q_prob",
    quantile_value_col = "q_val"
  )

  expect_equal(
    actual,
    expected
  )
})


test_that(
  "predict.qra_fit works: weight per model and quantile, no normalization", {
  set.seed(8457)
  forecast_df <- expand.grid(
    unit = letters[1:4],
    forecast_week_end_date = c("2020-04-18", "2020-04-25", "2020-05-02"),
    target = paste0(1:4, " wk ahead cum death"),
    model = paste0("m", 1:3),
    q_prob = c(0.025, 0.1, 0.5, 0.9, 0.975),
    stringsAsFactors = FALSE
  )
  forecast_df$q_val <- rnorm(nrow(forecast_df))

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df,
    model_col = "model",
    id_cols = c("unit", "forecast_week_end_date", "target"),
    quantile_name_col = "q_prob",
    quantile_value_col = "q_val"
  )

  qra_fit <- structure(
    list(
      coefficients = tidyr::expand_grid(
        model = paste0("m", 1:3),
        q_prob = c(0.025, 0.1, 0.5, 0.9, 0.975)
      ) %>%
        dplyr::mutate(
          beta = rexp(15)
        ),
      intercept = data.frame(
        q_prob = c(0.025, 0.1, 0.5, 0.9, 0.975),
        beta = rexp(5),
        stringsAsFactors = FALSE
      )
    ),
    convex = FALSE,
    class = "qra_fit"
  )

  actual <- predict(qra_fit, forecast_matrix, sort_quantiles = FALSE)
  # list of null dimnames was causing test to fail pointlessly
  attr(actual, "dimnames") <- NULL

  expected_df <- forecast_df %>%
    left_join(qra_fit$coefficients, by = c("model", "q_prob")) %>%
    left_join(
      qra_fit$intercept %>% select(q_prob, intercept = beta),
      by = "q_prob") %>%
    mutate(q_val = q_val * beta) %>%
    group_by(unit, forecast_week_end_date, target, q_prob) %>%
    summarize(
      q_val = sum(q_val) + intercept[1]
    ) %>%
    mutate(model = "qra")

  expected <- new_QuantileForecastMatrix_from_df(
    expected_df,
    model_col = "model",
    id_cols = c("unit", "forecast_week_end_date", "target"),
    quantile_name_col = "q_prob",
    quantile_value_col = "q_val"
  )

  expect_equal(
    actual,
    expected
  )
})


test_that("predict.qra_fit works: single weight per model, convex constraint", {
  set.seed(8457)
  forecast_df <- expand.grid(
    unit = letters[1:4],
    forecast_week_end_date = c("2020-04-18", "2020-04-25", "2020-05-02"),
    target = paste0(1:4, " wk ahead cum death"),
    model = paste0("m", 1:3),
    q_prob = c(0.025, 0.1, 0.5, 0.9, 0.975),
    stringsAsFactors = FALSE
  )
  forecast_df$q_val <- rnorm(nrow(forecast_df))

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df,
    model_col = "model",
    id_cols = c("unit", "forecast_week_end_date", "target"),
    quantile_name_col = "q_prob",
    quantile_value_col = "q_val"
  )

  qra_fit <- model_constructor_convex_per_model(
    par = c(-1, 4, 5),
    qfm_train = forecast_matrix)

  actual <- predict(qra_fit, forecast_matrix, sort_quantiles = FALSE)
  # list of null dimnames was causing test to fail pointlessly
  attr(actual, "dimnames") <- NULL

  a <- c(-1, 4, 5)
  weights <- data.frame(
    model = paste0("m", 1:3),
    beta = exp(a) / sum(exp(a)),
    stringsAsFactors = FALSE
  )

  expected_df <- forecast_df %>%
    left_join(weights, by = "model") %>%
    mutate(q_val = q_val * beta) %>%
    group_by(unit, forecast_week_end_date, target, q_prob) %>%
    summarize(
      q_val = sum(q_val) + qra_fit$intercept$beta
    ) %>%
    mutate(model = "qra")

  expected <- new_QuantileForecastMatrix_from_df(
    expected_df,
    model_col = "model",
    id_cols = c("unit", "forecast_week_end_date", "target"),
    quantile_name_col = "q_prob",
    quantile_value_col = "q_val"
  )

  expect_equal(
    actual,
    expected
  )
})


test_that("predict.median_qra_fit works", {
  set.seed(8457)
  forecast_df <- expand.grid(
    unit = letters[1:4],
    forecast_week_end_date = c("2020-04-18", "2020-04-25", "2020-05-02"),
    target = paste0(1:4, " wk ahead cum death"),
    model = paste0("m", 1:3),
    q_prob = c(0.025, 0.1, 0.5, 0.9, 0.975),
    stringsAsFactors = FALSE
  )
  forecast_df$q_val <- rnorm(nrow(forecast_df))

  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df,
    model_col = "model",
    id_cols = c("unit", "forecast_week_end_date", "target"),
    quantile_name_col = "q_prob",
    quantile_value_col = "q_val"
  )

  qra_fit <- new_median_qra_fit()

  actual <- predict(qra_fit, forecast_matrix)
  # list of null dimnames was causing test to fail pointlessly
  attr(actual, "dimnames") <- NULL

  expected_df <- forecast_df %>%
    group_by(unit, forecast_week_end_date, target, q_prob) %>%
    summarize(
      q_val = median(q_val)
    ) %>%
    mutate(model = "qra")

  expected <- new_QuantileForecastMatrix_from_df(
    expected_df,
    model_col = "model",
    id_cols = c("unit", "forecast_week_end_date", "target"),
    quantile_name_col = "q_prob",
    quantile_value_col = "q_val"
  )

  expect_equal(
    actual,
    expected
  )
})
