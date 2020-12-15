library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(doParallel)
library(zeallot)
library(covidEnsembles)

registerDoParallel(cores = 20)

output_path <- "code/application/retrospective-qra-comparison/log/"

first_forecast_date <- lubridate::ymd("2020-05-11")
last_forecast_date <- lubridate::floor_date(Sys.Date(), unit = "week") + 1
num_forecast_weeks <-
  as.numeric(last_forecast_date - first_forecast_date) / 7 + 1

trained_analysis_combinations <- tidyr::expand_grid(
  spatial_resolution = c("county", "state", "national", "state_national"),
  response_var = c("inc_case", "inc_death", "cum_death", "inc_hosp"),
  forecast_date = as.character(
    lubridate::ymd(first_forecast_date) +
      seq(from = 0, length = num_forecast_weeks) * 7),
  intercept = c("FALSE"),
  combine_method = c("convex"),
  quantile_group_str = c("per_quantile", "3_groups", "per_model"),
  missingness = c("mean_impute"),
  window_size = 3:10,
  check_missingness_by_target = "FALSE",
  do_standard_checks = "FALSE",
  do_baseline_check = "FALSE"
) %>%
  dplyr::filter(
    response_var %in% c("cum_death", "inc_death") |
    (response_var == "inc_case" & forecast_date >= "2020-08-03") |
    (response_var == "inc_hosp" & forecast_date >= "2020-11-16" &
      spatial_resolution != "county" & window_size <= 4),# |
#    (response_var == "inc_hosp" & forecast_week >= "2020-11-16"),
    spatial_resolution != "county" | window_size <= 5
  ) %>%
  dplyr::arrange(window_size, forecast_date)


unweighted_analysis_combinations <- tidyr::expand_grid(
  spatial_resolution = c("county", "state", "national"),
  response_var = c("inc_case", "inc_death", "cum_death", "inc_hosp"),
  forecast_date = as.character(
    lubridate::ymd(first_forecast_date) +
      seq(from = 0, length = num_forecast_weeks) * 7),
  intercept = c("FALSE"),
  combine_method = c("ew", "median"),
  quantile_group_str = c("per_model"),
  missingness = c("by_location_group"),
  window_size = 0,
  check_missingness_by_target = "FALSE",
  do_standard_checks = "FALSE",
  do_baseline_check = "FALSE"
) %>%
  dplyr::filter(
    (response_var %in% c("cum_death", "inc_death") & spatial_resolution != "county") |
    (response_var == "inc_case" & forecast_date >= "2020-08-03") |
    (response_var == "inc_hosp" & forecast_date >= "2020-11-16" & spatial_resolution != "county")
  )

analysis_combinations <- dplyr::bind_rows(
  trained_analysis_combinations,
  unweighted_analysis_combinations
)

# analysis_combinations <- analysis_combinations %>%
#   dplyr::filter(response_var == "inc_hosp")

run_status <- foreach(row_ind = seq_len(nrow(analysis_combinations))) %dopar% {
# foreach(row_ind = seq_len(2)) %dopar% {
  response_var <- analysis_combinations$response_var[row_ind]
  forecast_date <- analysis_combinations$forecast_date[row_ind]
  intercept <- analysis_combinations$intercept[row_ind]
  combine_method <- analysis_combinations$combine_method[row_ind]
  quantile_group_str <- analysis_combinations$quantile_group_str[row_ind]
  missingness <- analysis_combinations$missingness[row_ind]
  window_size <- analysis_combinations$window_size[row_ind]
  check_missingness_by_target <-
    analysis_combinations$check_missingness_by_target[row_ind]
  do_standard_checks <- analysis_combinations$do_standard_checks[row_ind]
  do_baseline_check <- analysis_combinations$do_baseline_check[row_ind]
  spatial_resolution <- analysis_combinations$spatial_resolution[row_ind]

  run_cmd <- paste0(
    "R CMD BATCH --vanilla \'--args ",
    response_var, " ",
    forecast_date, " ",
    intercept, " ",
    combine_method, " ",
    missingness, " ",
    quantile_group_str, " ",
    window_size, " ",
    check_missingness_by_target, " ",
    do_standard_checks, " ",
    do_baseline_check, " ",
    spatial_resolution,
    "\' code/application/retrospective-qra-comparison/fit_retrospective_model.R ",
    output_path, "output-", response_var, "-", forecast_date, "-",
    intercept, "-", combine_method, "-", missingness, "-", quantile_group_str,
    "-", window_size, "-", check_missingness_by_target, "-",
    do_standard_checks, "-", do_baseline_check, "-", spatial_resolution,
    ".Rout")

  system(run_cmd)
}
