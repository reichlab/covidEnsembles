library(gurobi)
library(quantgen)
library(covidData)
library(covidEnsembles)
library(matrixStats)
library(tidyverse)
library(zeallot)
library(gridExtra)
library(yaml)
library(reticulate)
Sys.setenv(CUDA_VISIBLE_DEVICES = '-1')

#options(warn=2, error=recover)

# arguments specifying details of analysis
args <- c("local", "inc_death", "2021-02-15", "FALSE", "rel_wis_weighted_median", "mean_impute", "per_model", "sort", "6", "TRUE", "FALSE", "FALSE", "state_national")
run_setting <- args[1]

if (run_setting %in% c("local", "cluster_single_node")) {
  # running locally -- run settings passed as command line arguments
  response_var <- args[2]
  forecast_date <- lubridate::ymd(args[3])
  intercept <- as.logical(args[4])
  combine_method <- args[5]
  missingness <- args[6]
  quantile_group_str <- args[7]
  noncross <- args[8]
  window_size_arg <- args[9]
  check_missingness_by_target <- as.logical(args[10])
  do_standard_checks <- as.logical(args[11])
  do_baseline_check <- as.logical(args[12])
  spatial_resolution_arg <- args[13]

  if (run_setting == "local") {
    submissions_root <- "~/research/epi/covid/covid19-forecast-hub/data-processed/"
  } else {
    submissions_root <- "/project/uma_nicholas_reich/covid19-forecast-hub/data-processed/"
  }
} else {
  # running on midas cluster -- extract run settings from csv file of analysis
  # combinations, row specified by job run index in environment variable
  job_ind <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  print(paste0("Job index: ", job_ind))

  analysis_combinations <- readr::read_csv(
    "code/application/retrospective-qra-comparison/analysis_combinations.csv"
  )
  
  response_var <- analysis_combinations$response_var[job_ind]
  forecast_date <- analysis_combinations$forecast_date[job_ind]
  intercept <- analysis_combinations$intercept[job_ind]
  combine_method <- analysis_combinations$combine_method[job_ind]
  missingness <- analysis_combinations$missingness[job_ind]
  quantile_group_str <- analysis_combinations$quantile_group_str[job_ind]
  window_size_arg <- analysis_combinations$window_size[job_ind]
  check_missingness_by_target <- analysis_combinations$check_missingness_by_target[job_ind]
  do_standard_checks <- analysis_combinations$do_standard_checks[job_ind]
  do_baseline_check <- analysis_combinations$do_baseline_check[job_ind]
  spatial_resolution_arg <- analysis_combinations$spatial_resolution[job_ind]

  submissions_root <- "~/covid19-forecast-hub/data-processed/"
}

# List of candidate models for inclusion in ensemble
candidate_model_abbreviations_to_include <- get_candidate_models(
  submissions_root = submissions_root,
  include_designations = c("primary", "secondary"),
  include_COVIDhub_ensemble = FALSE,
  include_COVIDhub_baseline = TRUE)

# Drop hospitalizations ensemble from JHU APL and ensemble from FDANIHASU
candidate_model_abbreviations_to_include <-
  candidate_model_abbreviations_to_include[
    !(candidate_model_abbreviations_to_include %in% c("JHUAPL-SLPHospEns", "FDANIHASU-Sweight", "COVIDhub-trained_ensemble"))
  ]


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
  } else if (spatial_resolution_arg == "state_national") {
    spatial_resolution <- c("state", "national")
  } else {
    spatial_resolution <- spatial_resolution_arg
  }
  temporal_resolution <- "wk"
  horizon <- 4L
  targets <- paste0(1:horizon, " wk ahead ", gsub("_", " ", response_var))
  forecast_week_end_date <- forecast_date - 2
  full_history_start <- lubridate::ymd("2020-06-22") - 7 * 10
} else if (response_var == "inc_case") {
  required_quantiles <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
  if (spatial_resolution_arg == "all") {
    spatial_resolution <- c("county", "state", "national")
  } else if (spatial_resolution_arg == "state_national") {
    spatial_resolution <- c("state", "national")
  } else {
    spatial_resolution <- spatial_resolution_arg
  }
  temporal_resolution <- "wk"
  horizon <- 4L
  targets <- paste0(1:horizon, " wk ahead ", gsub("_", " ", response_var))
  forecast_week_end_date <- forecast_date - 2
  full_history_start <- lubridate::ymd("2020-09-14") - 7 * 10
} else if (response_var == "inc_hosp") {
  required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  if (spatial_resolution_arg == "all") {
    spatial_resolution <- c("state", "national")
  } else if (spatial_resolution_arg == "state_national") {
    spatial_resolution <- c("state", "national")
  } else {
    spatial_resolution <- spatial_resolution_arg
  }
  temporal_resolution <- "day"
  horizon <- 28L
  targets <- paste0(1:(horizon + 6), " day ahead ", gsub("_", " ", response_var))
  forecast_week_end_date <- forecast_date
  full_history_start <- lubridate::ymd("2020-11-16") - 7 * 10
}

if (window_size_arg == "full_history") {
  window_size <- as.integer((forecast_date - full_history_start) / 7)
} else {
  window_size <- as.integer(window_size_arg)
}


if (quantile_group_str == "per_model") {
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
} else if (spatial_resolution_arg == "state_national") {
  spatial_resolution_path <- "state_national"
} else {
  spatial_resolution_path <- spatial_resolution
}

case_str <- paste0(
  "intercept_", as.character(intercept),
  "-combine_method_", combine_method,
  "-missingness_", missingness,
  "-quantile_groups_", quantile_group_str,
  "-noncross_", noncross,
  "-window_size_", window_size_arg,
  "-check_missingness_by_target_", check_missingness_by_target,
  "-do_standard_checks_", do_standard_checks,
  "-do_baseline_check_", do_baseline_check)

# create folder where model fits should be saved
fits_dir <- file.path(
  "code/application/retrospective-qra-comparison/retrospective-fits",
  spatial_resolution_path,
  case_str)
if (!dir.exists(fits_dir)) {
  dir.create(fits_dir)
}
fit_filename <- paste0(
  fits_dir, "/",
  response_var, "-", forecast_date, "-",
  case_str, ".rds")

# create folder where model weights should be saved
weights_dir <- file.path(
  "code/application/retrospective-qra-comparison/retrospective-weights",
  spatial_resolution_path,
  case_str)
if (!dir.exists(weights_dir)) {
  dir.create(weights_dir)
}
weight_filename <- paste0(
  weights_dir, "/",
  response_var, "-", forecast_date, "-",
  case_str, ".csv")

# create folder where model forecasts should be saved
forecasts_dir <- file.path(
  "code/application/retrospective-qra-comparison/retrospective-forecasts",
  spatial_resolution_path,
  case_str)
if (!dir.exists(forecasts_dir)) {
  dir.create(forecasts_dir)
}
forecast_filename <- paste0(
  forecasts_dir, "/",
  response_var, "-", forecast_date, "-",
  case_str, ".csv")


do_q10_check <- do_nondecreasing_quantile_check <- do_standard_checks

tic <- Sys.time()
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
  data_as_of_date = forecast_date - 1,
  intercept = intercept,
  combine_method = combine_method,
  quantile_groups = quantile_groups,
  noncross = noncross,
  missingness = missingness,
  impute_method = impute_method,
  backend = "optim",
  submissions_root = submissions_root,
  required_quantiles = required_quantiles,
  check_missingness_by_target = check_missingness_by_target,
  do_q10_check = do_q10_check,
  do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
  do_baseline_check = do_baseline_check,
  do_sd_check = FALSE,
  baseline_tol = 1.0,
  manual_eligibility_adjust = NULL,
  return_eligibility = TRUE,
  return_all = TRUE
)
toc <- Sys.time()
toc - tic


y_train <- results$location_groups$y_train[[1]]
qfm_train <- results$location_groups$qfm_train[[1]]

qra_fit <- results$location_groups$qra_fit[[1]]

all_weights_by_par <- purrr::map_dfr(
    seq(from = 0, to = 20),
    function(par) {
        covidEnsembles:::model_constructor_rel_wis_weighted_median(par, qfm_train, y_train)$coefficients %>%
            dplyr::mutate(par = par)
    })

pdf('weight_by_par.pdf')
ggplot(data = all_weights_by_par, mapping = aes(x = par, y = beta, color = model)) + geom_line()
dev.off()

pdf('est_weights.pdf', width=10, height = 8)
ggplot(data = ungroup(qra_fit$coefficients)) +
  geom_line(mapping = aes(x = model, y = beta, group = "a")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
dev.off()
