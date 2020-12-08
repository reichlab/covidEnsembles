context("date management")
library(covidEnsembles)

test_that("calc_forecast_week_end_date works", {
  timezero <- paste0('2020-04-', 1:30)
  target <- paste0(rep(1:2, each = 15), " wk ahead inc death")

  expected <- c(
    rep('2020-04-04', 6),
    rep('2020-04-11', 7),
    rep('2020-04-18', 7),
    rep('2020-04-25', 7),
    rep('2020-05-02', 3)
  )

  actual <- calc_forecast_week_end_date(timezero, target)

  expect_identical(actual, expected)
})

test_that("calc_target_week_end_date works", {
  timezero <- lubridate::ymd(rep(paste0('2020-04-', 1:30), times=2))
  horizon <- rep(1:2, each = 30)

  expected <- c(
    rep('2020-04-11', 6),
    rep('2020-04-18', 7),
    rep('2020-04-25', 7),
    rep('2020-05-02', 7),
    rep('2020-05-09', 3),
    rep('2020-04-18', 6),
    rep('2020-04-25', 7),
    rep('2020-05-02', 7),
    rep('2020-05-09', 7),
    rep('2020-05-16', 3)
  )

  actual <- calc_target_week_end_date(timezero, horizon)

  expect_identical(actual, expected)
})

test_that("calc_relative_target (implicitly calc_relative_horizon) works", {
  # Monday submission -- expected output same as input
  forecast_week_end_date <- lubridate::ymd("2020-11-02")
  target_end_date <- forecast_week_end_date + c(1:14, 7, 14, 21)
  target <- c(paste0(1:14, " day ahead inc hosp"),
    paste0(1:3, " wk ahead inc case"))
  expected <- c(paste0(1:14, " day ahead inc hosp"),
    paste0(1:3, " wk ahead inc case"))

  actual <- calc_relative_target(
    forecast_week_end_date,
    target_end_date,
    target)

  expect_identical(actual, expected)

  # Thursday submission -- expected output different from input
  forecast_week_end_date <- lubridate::ymd("2020-11-02")
  target_end_date <- forecast_week_end_date + c(1:14, 7, 14, 21)
  target <- c(paste0(1:14 + 4, " day ahead inc hosp"),
    paste0(1:3, " wk ahead inc case"))
  expected <- c(paste0(1:14, " day ahead inc hosp"),
    paste0(1:3, " wk ahead inc case"))

  actual <- calc_relative_target(
    forecast_week_end_date,
    target_end_date,
    target)

  expect_identical(actual, expected)
})

