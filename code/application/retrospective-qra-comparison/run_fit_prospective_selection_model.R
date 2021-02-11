library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(zeallot)
library(covidEnsembles)

args <- commandArgs(trailingOnly = TRUE)

output_path <- "code/application/retrospective-qra-comparison/log/"

# define analysis combinations to run
# number of cores to use for local runs
num_cores <- 8L

first_forecast_date <- lubridate::ymd("2020-05-11")
last_forecast_date <- lubridate::ymd("2021-02-08")

#last_forecast_date <- lubridate::floor_date(Sys.Date(), unit = "week") + 1
num_forecast_weeks <-
  as.numeric(last_forecast_date - first_forecast_date) / 7 + 1

analysis_combinations <- tidyr::expand_grid(
  spatial_resolution = c("county", "state", "national"),
  response_var = c("inc_case", "inc_death", "cum_death", "inc_hosp"),
  forecast_date = as.character(
    lubridate::ymd(first_forecast_date) +
      seq(from = 0, length = num_forecast_weeks) * 7),
  include_full_history = "FALSE" # c("TRUE", "FALSE")
) %>%
  dplyr::filter(
    (response_var %in% c("cum_death", "inc_death") &
      spatial_resolution != "county" &
      forecast_date >= "2020-06-22") |
    (response_var == "inc_case" & forecast_date >= "2020-09-14") |
    (response_var == "inc_hosp" & forecast_date >= "2020-11-23" &
      spatial_resolution != "county")
  )

library(doParallel)
registerDoParallel(cores = num_cores)

run_status <- foreach(row_ind = seq_len(nrow(analysis_combinations))) %dopar% {
#run_status <- foreach(row_ind = seq_len(2)) %dopar% {
  response_var <- analysis_combinations$response_var[row_ind]
  forecast_date <- analysis_combinations$forecast_date[row_ind]
  spatial_resolution <- analysis_combinations$spatial_resolution[row_ind]
  include_full_history <- analysis_combinations$include_full_history[row_ind]

  run_cmd <- paste0(
    "R CMD BATCH --vanilla \'--args ",
    response_var, " ",
    forecast_date, " ",
    spatial_resolution, " ",
    include_full_history,
    "\' code/application/retrospective-qra-comparison/fit_prospective_selection_model.R ",
    output_path, "output-", response_var, "-", forecast_date, "-",
    spatial_resolution, "-", include_full_history, ".Rout")

  system(run_cmd)
}
