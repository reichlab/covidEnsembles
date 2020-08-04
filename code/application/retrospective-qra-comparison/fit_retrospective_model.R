library(quantmod)
library(covidEnsembles)
library(tidyverse)
library(zeallot)
library(gridExtra)
library(zoltr)

submissions_root = '~/Documents/research/epi/covid/upstream-covid19-forecast-hub/covid19-forecast-hub/data-processed/'

# List of candidate models for inclusion in ensemble
model_dirs <- Sys.glob(paste0(submissions_root, '*'), dirmark = TRUE)
model_dirs <- model_dirs[substr(model_dirs, nchar(model_dirs), nchar(model_dirs)) == '/']

model_info <- purrr::map_dfr(
  model_dirs,
  function(model_dir) {
    metadata_path <- Sys.glob(paste0(model_dir, 'metadata*'))
    return(as.data.frame(
      yaml::read_yaml(metadata_path)[c('model_abbr', 'team_model_designation')],
      stringsAsFactors = FALSE
    ))
  }
)

candidate_model_abbreviations_to_include <- model_info %>%
  dplyr::filter(team_model_designation %in% c('primary', 'secondary', 'proposed')) %>%
  dplyr::pull(model_abbr)

#options(warn=2, error=recover)

#debug(covidEnsembles:::get_by_location_group_ensemble_fits_and_predictions)
#debug(covidEnsembles:::estimate_qra_quantmod)

# extract arguments specifying details of analysis
#args <- c("cum_death", "2020-05-09", "FALSE", "convex", "by_location_group", "3_groups", "2")
#args <- c("cum_death", "2020-05-09", "TRUE", "positive", "by_location_group", "3_groups", "2")
#args <- c("cum_death", "2020-05-09", "TRUE", "positive", "mean_impute", "3_groups", "2")
#args <- c("cum_death", "2020-05-16", "FALSE", "convex", "mean_impute", "per_model", "3")
#args <- c("cum_death", "2020-06-06", "FALSE", "convex", "mean_impute", "per_model", "6", "TRUE")
#args <- c("cum_death", "2020-06-13", "FALSE", "convex", "by_location_group", "per_quantile", "4", "TRUE")
#args <- c("cum_death", "2020-06-13", "FALSE", "convex", "by_location_group", "per_quantile", "4", "TRUE", "TRUE")
#args <- c("inc_death", "2020-05-23", "TRUE", "positive", "mean_impute", "3_groups", "3", "FALSE", "TRUE")

args <- commandArgs(trailingOnly = TRUE)
response_var <- args[1]
forecast_week_end_date <- lubridate::ymd(args[2])
intercept <- as.logical(args[3])
constraint <- args[4]
missingness <- args[5]
quantile_group_str <- args[6]
window_size <- as.integer(args[7])
do_standard_checks <- as.logical(args[8])
do_baseline_check <- as.logical(args[9])

if(quantile_group_str == "per_model") {
  quantile_groups <- rep(1, 23)
} else if(quantile_group_str == "3_groups") {
  quantile_groups <- c(rep(1, 3), rep(2, 23 - 6), rep(3, 3))
} else if(quantile_group_str == "per_quantile") {
  quantile_groups <- 1:23
} else {
  stop('invalid quantile_groups')
}

if(missingness == 'mean_impute') {
  missingness <- 'impute'
  impute_method <- 'mean'
} else {
  impute_method <- NULL
}

if(response_var %in% c('inc_death', 'cum_death')) {
  required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  spatial_resolution <- c('state', 'national')
  horizon <- 4L
} else if(response_var == 'inc_case') {
  required_quantiles <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
  spatial_resolution <- c('county', 'state', 'national')
  horizon <- 4L
}

result_filename <- paste0('code/application/retrospective-qra-comparison/retrospective-fits/',
                          response_var,
                          '-forecast_week_', as.character(forecast_week_end_date),
                          '-intercept_', as.character(intercept),
                          '-constraint_', constraint,
                          '-missingness_', missingness,
                          '-quantile_groups_', quantile_group_str,
                          '-window_size_', window_size,
                          '-do_standard_checks_', do_standard_checks,
                          '-do_baseline_check_', do_baseline_check,
                          '.rds')

if(!file.exists(result_filename)) {
  do_q10_check <- do_nondecreasing_quantile_check <- do_standard_checks
  #do_q10_check <- do_nondecreasing_quantile_check <- (response_var == 'cum_death')
  #do_q10_check <- FALSE
  #do_nondecreasing_quantile_check <- FALSE
  #debug(covidEnsembles::get_imputed_ensemble_fits_and_predictions)

  #c(model_eligibility, wide_model_eligibility, location_groups, component_forecasts) %<-%
  results <-
    build_covid_ensemble_from_local_files(
      candidate_model_abbreviations_to_include = candidate_model_abbreviations_to_include,
      spatial_resolution = spatial_resolution,
      targets = paste0(1:horizon, ' wk ahead ', gsub('_', ' ', response_var)),
      forecast_week_end_date = forecast_week_end_date,
      timezero_window_size = 1,
      window_size = window_size,
      intercept = intercept,
      constraint = constraint,
      quantile_groups = quantile_groups,
      missingness = missingness,
      impute_method = impute_method,
      backend = 'quantmod',
      submissions_root = '~/Documents/research/epi/covid/upstream-covid19-forecast-hub/covid19-forecast-hub/data-processed/',
      required_quantiles = required_quantiles,
      do_q10_check = do_q10_check,
      do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
      do_baseline_check = do_baseline_check,
      baseline_tol = 1.0,
      manual_eligibility_adjust = NULL,
      return_eligibility = TRUE,
      return_all = TRUE
    )

  saveRDS(results, file = result_filename)
}
