# load packages
library(covidData)
library(covidHubUtils)
library(covidEnsembles)
library(tidyverse)
library(gridExtra)
library(knitr)
library(here)

setwd(here())

knitr::opts_chunk$set(echo = FALSE, cache.lazy = FALSE)
options(width = 200)

# Dates of forecast submission for forecasts included in this analysis
first_forecast_date <- lubridate::ymd("2020-05-11")
last_forecast_date <- lubridate::ymd("2021-01-25")
num_forecast_weeks <- as.integer(last_forecast_date -
                         first_forecast_date) / 7 + 1

forecast_dates <- first_forecast_date +
  seq(from = 0, length = num_forecast_weeks) * 7

all_scores <- calc_retrospective_ensemble_scores(
  submissions_root = "code/application/retrospective-qra-comparison/retrospective-forecasts/",
  forecast_dates = forecast_dates,
  spatial_scales = c("national", "state", "state_national", "county"),
  truth_as_of = Sys.Date()
)

saveRDS(
  all_scores,
  "code/application/retrospective-qra-comparison/analyses/retrospective-scores/retrospective_scores.rds")
