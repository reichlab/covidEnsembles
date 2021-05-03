library(covidData)
library(covidEnsembles)
library(tidyverse)
library(zeallot)
library(gridExtra)
library(yaml)
library(reticulate)
Sys.setenv(CUDA_VISIBLE_DEVICES = '-1')
Sys.setenv(LANG = "en_US.UTF-8")


#options(warn=2, error=recover)

#debug(covidEnsembles:::get_by_location_group_ensemble_fits_and_predictions)
#debug(covidEnsembles:::estimate_qra_quantgen)
#debug(covidEnsembles::estimate_qra)
#debug(quantgen:::quantile_ensemble_flex)

# extract arguments specifying details of analysis
#args <- c("local", "inc_death", "2020-11-23", "FALSE", "convex", "mean_impute", "per_quantile", "full_history", "TRUE", "FALSE", "FALSE", "state_national")
#args <- c("local", "inc_hosp", "2021-01-18", "FALSE", "convex", "mean_impute", "per_model", "3", "TRUE", "FALSE", "FALSE", "state")
#args <- c("local", "inc_death", "2021-01-25", "FALSE", "convex", "mean_impute", "per_quantile", "constrain", "full_history", "TRUE", "FALSE", "FALSE", "state_national")
#args <- c("local", "inc_death", "2021-02-15", "FALSE", "convex", "mean_impute", "per_model", "sort", "6", "TRUE", "FALSE", "FALSE", "state_national")
#args <- c("local", "inc_death", "2021-02-15", "FALSE", "convex", "renormalize", "per_model", "sort", "6", "TRUE", "FALSE", "FALSE", "state_national")
#args <- c("local", "inc_death", "2021-02-15", "FALSE", "convex_median", "renormalize", "per_model", "sort", "6", "TRUE", "FALSE", "FALSE", "state_national")
#args <- c("local", "inc_death", "2020-10-26", "FALSE", "convex", "renormalize", "per_model", "sort", "4", "all_models", "TRUE", "FALSE", "FALSE", "state_national")
#args <- c("cluster_single_node", "inc_death", "2021-02-15", "FALSE", "convex", "renormalize", "per_model", "sort", "6", "TRUE", "FALSE", "FALSE", "state_national")
#args <- c("local", "cum_death", "2020-06-22", "FALSE", "convex", "mean_impute", "3_groups", "sort", "full_history", "TRUE", "FALSE", "FALSE", "state")
#args <- c("local", "cum_death", "2020-07-13", "FALSE", "convex", "mean_impute", "3_groups", "sort", "full_history", "TRUE", "FALSE", "FALSE", "state")
#args <- c("local", "cum_death", "2021-02-15", "FALSE", "convex", "mean_impute", "3_groups", "constrain", "5", "TRUE", "FALSE", "FALSE", "state")
#args <- c("local", "inc_hosp", "2021-04-26", "FALSE", "convex", "renormalize", "per_model", "sort", "full_history", "TRUE", "FALSE", "FALSE", "state_national")
#args <- c("local", "inc_case", "2021-04-26", "FALSE", "convex", "renormalize", "per_model", "sort", "full_history", "TRUE", "FALSE", "FALSE", "county")
#args <- c("local", "inc_hosp", "2021-04-26", "FALSE", "median", "none", "per_model", "sort", "full_history", "5", "TRUE", "FALSE", "FALSE", "state_national")

args <- commandArgs(trailingOnly = TRUE)
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
  top_models_arg <- args[10]
  check_missingness_by_target <- as.logical(args[11])
  do_standard_checks <- as.logical(args[12])
  do_baseline_check <- as.logical(args[13])
  spatial_resolution_arg <- args[14]

  if (run_setting == "local") {
    submissions_root <- "~/research/epi/covid/covid19-forecast-hub/data-processed/"
  } else {
    submissions_root <- "/project/uma_nicholas_reich/covid19-forecast-hub/data-processed/"
    reticulate::use_python("/usr/bin/python3.8")
  }
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
    !(candidate_model_abbreviations_to_include %in% c("JHUAPL-SLPHospEns", "FDANIHASU-Sweight", "COVIDhub-trained_ensemble", "KITmetricslab-select_ensemble"))
  ]


if (missingness == "mean_impute") {
  case_missingness <- "impute"
  missingness <- "impute"
  impute_method <- "mean"
} else if (missingness == "renormalize" || missingness == "none") {
  case_missingness <- missingness
  missingness <- "impute"
  impute_method <- "none"
} else {
  case_missingness <- missingness
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

if (top_models_arg == "all_models") {
  top_models <- 0L
} else {
  top_models <- as.integer(top_models_arg)
}

case_str <- paste0(
  "intercept_", as.character(intercept),
  "-combine_method_", combine_method,
  "-missingness_", case_missingness,
  "-quantile_groups_", quantile_group_str,
  "-noncross_", noncross,
  "-window_size_", window_size_arg,
  "-top_models_", top_models_arg,
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
partial_save_filename <- paste0(
  fits_dir, "/",
  response_var, "-", forecast_date, "-",
  case_str, ".pkl")
loss_trace_filename <- paste0(
  fits_dir, "/",
  response_var, "-", forecast_date, "-",
  case_str, "_loss_trace.rds")

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


tic <- Sys.time()
if (!file.exists(forecast_filename)) {
  do_q10_check <- do_nondecreasing_quantile_check <- do_standard_checks

  tictic <- Sys.time()
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
    backend = "qenspy",
    submissions_root = submissions_root,
    required_quantiles = required_quantiles,
    check_missingness_by_target = check_missingness_by_target,
    do_q10_check = do_q10_check,
    do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
    do_baseline_check = do_baseline_check,
    do_sd_check = FALSE,
    baseline_tol = 1.0,
    top_models = top_models,
    manual_eligibility_adjust = NULL,
    return_eligibility = TRUE,
    return_all = TRUE,
    partial_save_frequency = 10,
    partial_save_filename = partial_save_filename
  )
  toctoc <- Sys.time()
  toctoc - tictic

  # save full results including estimated weights, training data, etc.
  # only if running locally; cluster has limited space
  if (run_setting == "local") {
    saveRDS(results, file = fit_filename)
  }

  # extract and save just the estimated weights in csv format
  if (!(combine_method %in% c("ew", "mean", "median"))) {
    # save loss trace as a function of optimization iteration
    loss_trace <- results$location_groups$qra_fit[[1]]$loss_trace
    saveRDS(loss_trace, file = loss_trace_filename)

    estimated_weights <- purrr::pmap_dfr(
      results$location_groups %>% dplyr::select(locations, qra_fit),
      function(locations, qra_fit) {
        covidEnsembles:::extract_weights_qenspy_qra_fit(qra_fit) %>%
          dplyr::mutate(join_field = "temp") %>%
          dplyr::left_join(
            data.frame(
              location = locations,
              join_field = "temp",
              stringsAsFactors = FALSE
            )
          ) %>%
          dplyr::select(-join_field)

        # weights <- qra_fit$coefficients

        # data.frame(
        #   quantile = if ("quantile" %in% colnames(weights)) {
        #       weights$quantile
        #     } else {
        #       rep(NA, nrow(weights))
        #     },
        #   model = weights$model,
        #   weight = weights$beta, #[, 1],
        #   join_field = "temp",
        #   stringsAsFactors = FALSE
        # ) %>%
        #   dplyr::left_join(
        #     data.frame(
        #       location = locations,
        #       join_field = "temp",
        #       stringsAsFactors = FALSE
        #     )
        #   ) %>%
        #   dplyr::select(-join_field)
      }
    )
    write_csv(estimated_weights, weight_filename)
  }


  # save csv formatted forecasts
  if (missingness == "impute") {
    c(model_eligibility, wide_model_eligibility, location_groups,
      weight_transfer, component_forecasts) %<-% results
    
    col_index <- attr(location_groups$imputed_qfm_test[[1]], "col_index")
    models_used <- purrr::map_dfc(
      unique(col_index$model),
      function(model) {
        col_ind <- min(which(col_index$model == model))
        result <- data.frame(
          m = !is.na(unclass(location_groups$imputed_qfm_test[[1]])[, col_ind]))
        colnames(result) <- model
        return(result)
      }
    )
    model_counts <- apply(
      models_used,
      1,
      sum
    )

    # locations_to_drop <- unique(
    #   attr(location_groups$imputed_qfm_test[[1]], "row_index")[
    #     model_counts == 1, "location"])
    
    ensemble_predictions <- location_groups$qra_forecast[[1]] #%>%
      #dplyr::filter(!(location %in% locations_to_drop))
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
        value = ifelse(
          quantile < 0.5,
          floor(value),
          ifelse(
            quantile == 0.5,
            round(value),
            ceiling(value)
          )
        )
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

    write_csv(formatted_ensemble_predictions, forecast_filename)
  }
}
toc <- Sys.time()
toc - tic
