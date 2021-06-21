context("loading forecasts")
library(covidEnsembles)

test_that("load_covid_forecasts_relative_horizon as_of works correctly", {
  orig_forecasts <- load_covid_forecasts_relative_horizon(
    hub = "US",
    source = "zoltar",
    data_processed_subpath = "data-processed/",
    monday_dates = "2021-02-01",
    as_of = "2021-02-01",
    model_abbrs = "SteveMcConnell-CovidComplete",
    timezero_window_size = 6,
    locations = NULL,
    targets = paste0(1:4, " wk ahead cum death"),
    max_horizon = 14,
    required_quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975)
  )

  updated_forecasts <- load_covid_forecasts_relative_horizon(
    hub = "US",
    source = "zoltar",
    data_processed_subpath = "data-processed/",
    monday_dates = "2021-02-01",
    as_of = "2021-02-03",
    model_abbrs = "SteveMcConnell-CovidComplete",
    timezero_window_size = 6,
    locations = NULL,
    targets = paste0(1:4, " wk ahead cum death"),
    max_horizon = 14,
    required_quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975)
  )

  forecasts_diff <- dplyr::anti_join(orig_forecasts, updated_forecasts)

  # only cumulative death forecasts in the US changed:
  # https://github.com/reichlab/covid19-forecast-hub/pull/2801/files
  expect_equal(
    forecasts_diff[, c("location", "quantile", "target")],
    tidyr::expand_grid(
      location = "US",
      quantile = as.character(c(0.025, 0.25, 0.5, 0.75, 0.975)),
      target = paste0(1:4, " wk ahead cum death")
    ) %>%
      dplyr::arrange(target)
  )
})
