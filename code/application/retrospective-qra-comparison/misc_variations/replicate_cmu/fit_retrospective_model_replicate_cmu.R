library(gurobi)
library(quantgen)
library(covidData)
library(covidEnsembles)
library(tidyverse)
library(zeallot)
library(gridExtra)
library(yaml)

submissions_root <- "~/research/epi/covid/covid19-forecast-hub/data-processed/"

# List of candidate models for inclusion in ensemble
model_dirs <- Sys.glob(paste0(submissions_root, "*"), dirmark = TRUE)
model_dirs <- model_dirs[
  substr(model_dirs, nchar(model_dirs), nchar(model_dirs)) == "/"
]

model_info <- purrr::map_dfr(
  model_dirs,
  function(model_dir) {
    metadata_path <- Sys.glob(paste0(model_dir, "metadata*"))
    return(as.data.frame(
      yaml::read_yaml(metadata_path)[c("model_abbr", "team_model_designation")],
      stringsAsFactors = FALSE
    ))
  }
)

candidate_model_abbreviations_to_include <- model_info %>%
  # dplyr::filter(team_model_designation %in%
  #   c("primary", "secondary", "proposed")) %>%
  dplyr::pull(model_abbr)

candidate_model_abbreviations_to_include <-
  candidate_model_abbreviations_to_include[
    !(candidate_model_abbreviations_to_include == "COVIDhub-ensemble")
  ]


#options(warn=2, error=recover)

#debug(covidEnsembles:::get_by_location_group_ensemble_fits_and_predictions)
#debug(covidEnsembles:::estimate_qra_quantgen)

# extract arguments specifying details of analysis
#args <- c("cum_death", "2020-05-09", "FALSE", "convex", "by_location_group", "3_groups", "2")
#args <- c("cum_death", "2020-05-09", "TRUE", "positive", "by_location_group", "3_groups", "2")
#args <- c("cum_death", "2020-05-09", "TRUE", "positive", "mean_impute", "3_groups", "2")
#args <- c("cum_death", "2020-05-16", "FALSE", "convex", "mean_impute", "per_model", "3")
#args <- c("cum_death", "2020-06-06", "FALSE", "convex", "mean_impute", "per_model", "6", "TRUE")
#args <- c("cum_death", "2020-06-13", "FALSE", "convex", "by_location_group", "per_quantile", "4", "TRUE")
#args <- c("cum_death", "2020-06-13", "FALSE", "convex", "by_location_group", "per_quantile", "4", "TRUE", "TRUE")
#args <- c("inc_death", "2020-05-23", "TRUE", "positive", "mean_impute", "3_groups", "3", "FALSE", "TRUE")
#args <- c("inc_death", "2020-07-25", "TRUE", "positive", "mean_impute", "per_quantile", "5", "TRUE", "FALSE")
#args <- c("cum_death", "2020-05-16", "FALSE", "convex", "by_location_group", "per_quantile", "5", "FALSE", "FALSE")
#args <- c("inc_death", "2020-07-25", "TRUE", "positive", "mean_impute", "3_groups", "3", "TRUE", "FALSE", "FALSE")
#args <- c("cum_death", "2020-05-09", "FALSE", "convex", "mean_impute", "3_groups", "2", "FALSE", "TRUE", "FALSE")
#args <- c("cum_death", "2020-08-01", "FALSE", "ew", "by_location_group", "per_model", "0", "FALSE", "FALSE", "FALSE")
#args <- c("inc_case", "2020-08-01", "FALSE", "convex", "by_location_group", "per_quantile", "2", "FALSE", "TRUE", "FALSE")
#args <- c("inc_case", "2020-10-24", "TRUE", "positive", "mean_impute", "3_groups", "4", "FALSE", "FALSE", "FALSE")
#args <- c("inc_case", "2020-05-09", "FALSE", "ew", "by_location_group", "per_model", "0", "FALSE", "FALSE", "FALSE")
#args <- c("inc_death", "2020-05-09", "FALSE", "convex", "mean_impute", "3_groups", "4", "FALSE", "FALSE", "FALSE", "state_no_territories")
#args <- c("inc_death", "2020-10-05", "FALSE", "ew", "by_location_group", "per_model", "0", "FALSE", "FALSE", "FALSE", "state")
#args <- c("inc_death", "2020-10-05", "FALSE", "convex", "mean_impute", "3_groups", "4", "FALSE", "FALSE", "FALSE", "state")

args <- commandArgs(trailingOnly = TRUE)
response_var <- args[1]
forecast_date <- lubridate::ymd(args[2])
intercept <- as.logical(args[3])
combine_method <- args[4]
missingness <- args[5]
quantile_group_str <- args[6]
window_size <- as.integer(args[7])
check_missingness_by_target <- as.logical(args[8])
do_standard_checks <- as.logical(args[9])
do_baseline_check <- as.logical(args[10])
spatial_resolution_arg <- args[11]

if (missingness == "mean_impute") {
  missingness <- "impute"
  impute_method <- "mean"
} else {
  impute_method <- NULL
}

if (response_var %in% c("inc_death", "cum_death")) {
  required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  if (spatial_resolution_arg == "all") {
    spatial_resolution <- c("state", "national")
  } else {
    spatial_resolution <- spatial_resolution_arg
  }
  temporal_resolution <- "wk"
  horizon <- 1L
  targets <- paste0(1:horizon, " wk ahead ", gsub("_", " ", response_var))
  forecast_week_end_date <- forecast_date - 2
} else if (response_var == "inc_case") {
  required_quantiles <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
  if (spatial_resolution_arg == "all") {
    spatial_resolution <- c("county", "state", "national")
  } else {
    spatial_resolution <- spatial_resolution_arg
  }
#  spatial_resolution <- c("state", "national")
  horizon <- 1L
}



if(quantile_group_str == "per_model") {
  quantile_groups <- rep(1, length(required_quantiles))
} else if(quantile_group_str == "3_groups") {
  if (length(required_quantiles) == 23) {
    quantile_groups <- c(rep(1, 4), rep(2, 23 - 8), rep(3, 4))
  } else if (length(required_quantiles) == 7) {
    quantile_groups <- c(1, rep(2, 5), 3)
  }
} else if(quantile_group_str == "per_quantile") {
  quantile_groups <- seq_along(required_quantiles)
} else {
  stop("invalid quantile_groups")
}

if (spatial_resolution_arg == "all") {
  spatial_resolution_path <- ""
} else {
  spatial_resolution_path <- spatial_resolution
}

result_filename <- paste0(
  "code/application/retrospective-qra-comparison/misc_variations/replicate_cmu/retrospective-fits/", spatial_resolution_path, "/",
  response_var,
  "-forecast_week_", as.character(forecast_date),
  "-intercept_", as.character(intercept),
  "-combine_method_", combine_method,
  "-missingness_", missingness,
  "-quantile_groups_", quantile_group_str,
  "-window_size_", window_size,
  "-check_missingness_by_target_", check_missingness_by_target,
  "-do_standard_checks_", do_standard_checks,
  "-do_baseline_check_", do_baseline_check,
  ".rds")

case_str <- paste0(
  "intercept_", as.character(intercept),
  "-combine_method_", combine_method,
  "-missingness_", missingness,
  "-quantile_groups_", quantile_group_str,
  "-window_size_", window_size,
  "-check_missingness_by_target_", check_missingness_by_target,
  "-do_standard_checks_", do_standard_checks,
  "-do_baseline_check_", do_baseline_check)

csv_dir <- paste0(
  "code/application/retrospective-qra-comparison/misc_variations/replicate_cmu/retrospective-forecasts/", spatial_resolution_path, "/",
  case_str, "/")

if (!dir.exists(csv_dir)) {
  dir.create(csv_dir)
}

csv_filename <- paste0(
  csv_dir,
  response_var, "-", forecast_date, "-",
  case_str, ".csv")

tic <- Sys.time()
if(!file.exists(csv_filename)) {
  do_q10_check <- do_nondecreasing_quantile_check <- do_standard_checks

  results <- build_covid_ensemble_from_local_files(
    candidate_model_abbreviations_to_include =
      candidate_model_abbreviations_to_include,
    spatial_resolution = spatial_resolution,
    targets = targets,
    forecast_date = forecast_date,
    forecast_week_end_date = forecast_week_end_date,
    horizon = horizon,
    timezero_window_size = 6,
    window_size = window_size,
    intercept = intercept,
    combine_method = combine_method,
    quantile_groups = quantile_groups,
    missingness = missingness,
    impute_method = impute_method,
    backend = "quantgen",
    submissions_root = submissions_root,
    required_quantiles = required_quantiles,
    check_missingness_by_target = check_missingness_by_target,
    do_q10_check = do_q10_check,
    do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
    do_baseline_check = do_baseline_check,
    baseline_tol = 1.0,
    manual_eligibility_adjust = NULL,
    return_eligibility = TRUE,
    return_all = TRUE
  )

  # save full results including estimated weights, etc.
  saveRDS(results, file = result_filename)

  # save csv formatted forecasts
  if (missingness == "impute") {
    c(model_eligibility, wide_model_eligibility, location_groups,
      weight_transfer, component_forecasts) %<-% results
    
    col_index <- attr(location_groups$qfm_test[[1]], "col_index")
    models_used <- purrr::map_dfc(
      unique(col_index$model),
      function(model) {
        col_ind <- min(which(col_index$model == model))
        result <- data.frame(
          m = !is.na(unclass(location_groups$qfm_test[[1]])[, col_ind]))
        colnames(result) <- model
        return(result)
      }
    )
    model_counts <- apply(
      models_used,
      1,
      sum
    )

    locations_to_drop <- unique(
      attr(location_groups$qfm_test[[1]], "row_index")[
        model_counts == 1, "location"])
    
    ensemble_predictions <- location_groups$qra_forecast[[1]] %>%
      dplyr::filter(!(location %in% locations_to_drop))
  } else {
    c(model_eligibility, wide_model_eligibility, location_groups,
      component_forecasts) %<-% results
    
    model_counts <- apply(
      location_groups %>% select_if(is.logical),
      1,
      sum)
    location_groups <- location_groups[model_counts > 1, ]

    if (nrow(location_groups) > 0) {
      ensemble_predictions <- bind_rows(location_groups[['qra_forecast']])
    }
  }

  if (nrow(ensemble_predictions) > 0) {
    # save the results in required format
    formatted_ensemble_predictions <- ensemble_predictions %>%
      dplyr::transmute(
        forecast_date = forecast_date,
        target = target,
        target_end_date = covidHubUtils::calc_target_end_date(
          forecast_date,
          as.integer(substr(target, 1, regexpr(" ", target, fixed = TRUE) - 1)),
          rep(temporal_resolution, nrow(ensemble_predictions))),
        location = location,
        type = 'quantile',
        quantile = quantile,
        value = value # ifelse(
        #   quantile < 0.5,
        #   floor(value),
        #   ifelse(
        #     quantile == 0.5,
        #     round(value),
        #     ceiling(value)
        #   )
        # )
      )

    formatted_ensemble_predictions <- bind_rows(
      formatted_ensemble_predictions,
      formatted_ensemble_predictions %>%
        filter(format(quantile, digits = 3, nsmall = 3) == "0.500") %>%
        mutate(
          type = "point",
          quantile = NA_real_
        )
    )
    
    write_csv(formatted_ensemble_predictions, csv_filename)
  }
}
toc <- Sys.time()
toc - tic
