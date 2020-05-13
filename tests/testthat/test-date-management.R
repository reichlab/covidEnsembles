context("date management")
library(covidEnsembles)

test_that("calc_forecast_week_end_date works", {
  timezero <- paste0('2020-04-', 1:30)

  expected <- c(
    rep('2020-04-04', 6),
    rep('2020-04-11', 7),
    rep('2020-04-18', 7),
    rep('2020-04-25', 7),
    rep('2020-05-02', 3)
  )

  actual <- calc_forecast_week_end_date(timezero)

  expect_identical(actual, expected)
})

test_that("calc_target_week_end_date works", {
  timezero <- rep(paste0('2020-04-', 1:30), times=2)
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



