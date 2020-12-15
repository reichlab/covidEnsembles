# load packages
library(covidData)
library(covidEnsembles)
library(tidyverse)
library(gridExtra)
library(knitr)

knitr::opts_chunk$set(echo = FALSE, cache.lazy = FALSE)
options(width = 200)

# load data

# dates for "truth" data used to compute scores and used in plots
jhu_issue_date <- max(covidData::jhu_deaths_data$issue_date)
healthdata_issue_date <- max(covidData::healthdata_hosp_data$issue_date)

# load data
observed_deaths <-
  covidData::load_jhu_data(
    issue_date = jhu_issue_date,
    spatial_resolution = c("state", "national"),
    temporal_resolution = "weekly",
    measure = "deaths") %>%
  tidyr::pivot_longer(
    cols = c("inc", "cum"),
    names_to = "base_target",
    values_to = "observed"
  ) %>%
  dplyr::transmute(
    location = location,
    base_target = paste0("wk ahead ", base_target, " death"),
    target_end_date = as.character(date),
    observed = observed
  )

observed_deaths <- observed_deaths[!duplicated(observed_deaths), ]

observed_cases <-
  covidData::load_jhu_data(
    issue_date = jhu_issue_date,
    spatial_resolution = c("county", "state", "national"),
    temporal_resolution = "weekly",
    measure = "cases") %>%
  tidyr::pivot_longer(
    cols = c("inc", "cum"),
    names_to = "base_target",
    values_to = "observed"
  ) %>%
  dplyr::transmute(
    location = location,
    base_target = paste0("wk ahead ", base_target, " case"),
    target_end_date = as.character(date),
    observed = observed
  )

observed_cases <- observed_cases[!duplicated(observed_cases), ]

observed_hosps <-
  covidData::load_healthdata_data(
    issue_date = healthdata_issue_date,
    spatial_resolution = c("state", "national"),
    temporal_resolution = "daily",
    measure ="hospitalizations") %>%
  tidyr::pivot_longer(
    cols = c("inc", "cum"),
    names_to = "base_target",
    values_to = "observed"
  ) %>%
  dplyr::transmute(
    location = location,
    base_target = paste0("day ahead ahead ", base_target, " hosp"),
    target_end_date = as.character(date),
    observed = observed
  )

observed_hosps <- observed_hosps[!duplicated(observed_hosps), ]

observed <- dplyr::bind_rows(observed_deaths, observed_cases, observed_hosps)

# last target date to evaluate:
#  - most recent Saturday with observed data for weekly targets
#  - most recent day with observed data for daily targets
last_weekly_target_date <- max(observed_deaths$target_end_date)
last_daily_target_date <- max(observed_hosps$target_end_date)

# dates for saturdays included in the analysis:
#  - we consider ensemble forecasts generated 2 days after this saturday date
#  - week ahead targets are defined relative to this saturday date
first_forecast_week_end_date <- lubridate::ymd("2020-05-09")
last_forecast_week_end_date <- lubridate::ymd(last_weekly_target_date) - 7
num_forecast_weeks <- as.integer(last_forecast_week_end_date -
                         first_forecast_week_end_date) / 7 + 1
forecast_week_end_dates <- as.character(
  first_forecast_week_end_date +
    seq(from = 0, length = num_forecast_weeks) * 7
)

# Dates of forecast submission for forecasts included in this analysis:
# 2 days after the saturdays
forecast_dates <- lubridate::ymd(forecast_week_end_dates) + 2


# load forecasts

# targets
all_targets <- c(
  paste0(1:4, " wk ahead cum death"),
  paste0(1:4, " wk ahead inc death"),
  paste0(1:4, " wk ahead inc case"),
  paste0(1:28, " day ahead inc hosp")
)

# function to extract model identifiers from abbreviation
parse_model_case <- function(model_abbr) {
  case_parts <- strsplit(model_abbr, split = "-")[[1]]
  purrr::map_dfc(
    case_parts,
    function(case_part) {
      nc <- nchar(case_part)
      if(substr(case_part, 1, min(nc, 9)) == "intercept") {
        return(data.frame(
          intercept = as.logical(substr(case_part, 11, nc))
        ))
      } else if(substr(case_part, 1, min(nc, 14)) == "combine_method") {
        return(data.frame(
          combine_method = substr(case_part, 16, nc)
        ))
      } else if(substr(case_part, 1, min(nc, 11)) == "missingness") {
        return(data.frame(
          missingness = substr(case_part, 13, nc)
        ))
      } else if(substr(case_part, 1, min(nc, 15)) == "quantile_groups") {
        return(data.frame(
          quantile_groups = substr(case_part, 17, nc)
        ))
      } else if(substr(case_part, 1, min(nc, 11)) == "window_size") {
        return(data.frame(
          window_size = substr(case_part, 13, nc)
        ))
      } else if(substr(case_part, 1, min(nc, 27)) ==
          "check_missingness_by_target") {
        return(data.frame(
          check_missingness_by_target = substr(case_part, 29, nc)
        ))
      } else if(substr(case_part, 1, min(nc, 18)) == "do_standard_checks") {
        return(data.frame(
          do_standard_checks = substr(case_part, 20, nc)
        ))
      } else if(substr(case_part, 1, min(nc, 17)) == "do_baseline_check") {
        return(data.frame(
          do_baseline_check = substr(case_part, 19, nc)
        ))
      } else {
        message("Unsupported case part")
      }
    }
  )
}

all_forecasts <- purrr::map_dfr(
  c("national", "state", "state_national", "county"),
  function(spatial_scale) {
    # Path to forecasts to evaluate
    submissions_root <- paste0(
      "code/application/retrospective-qra-comparison/retrospective-forecasts-",
      spatial_scale, "/"
    )

    # models to read in
    model_abbrs <- list.dirs(submissions_root, full.names = FALSE)
    model_abbrs <- model_abbrs[nchar(model_abbrs) > 0]

    if (spatial_scale == "county") {
      model_abbrs <- c(
        "intercept_FALSE-combine_method_convex-missingness_impute-quantile_groups_per_quantile-window_size_3-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE",
        "intercept_FALSE-combine_method_convex-missingness_impute-quantile_groups_per_quantile-window_size_4-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE",
        "intercept_FALSE-combine_method_ew-missingness_by_location_group-quantile_groups_per_model-window_size_0-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE",
        "intercept_FALSE-combine_method_median-missingness_by_location_group-quantile_groups_per_model-window_size_0-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE"
      )
    }

    if (spatial_scale %in% c("national", "state_national")) {
      response_vars <- c("cum_death", "inc_death", "inc_case")
    } else if (spatial_scale == "state") {
      response_vars <- c("cum_death", "inc_death", "inc_case", "inc_hosp")
    } else if (spatial_scale == "county") {
      response_vars <- "inc_case"
    }

    spatial_scale_forecasts <- purrr::map_dfr(
      response_vars,
      function(response_var) {
        if (response_var %in% c("inc_death", "cum_death")) {
          required_quantiles <-
            c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
          temporal_resolution <- "wk"
          horizon <- 4L
          targets <-
            paste0(1:horizon, " wk ahead ", gsub("_", " ", response_var))
          all_locations <- unique(observed_deaths$location)
        } else if (response_var == "inc_case") {
          required_quantiles <-
            c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
          temporal_resolution <- "wk"
          horizon <- 4L
          targets <- paste0(
            1:horizon, " wk ahead ", gsub("_", " ", response_var))
          all_locations <- unique(observed_cases$location)
        } else if (response_var == "inc_hosp") {
          required_quantiles <-
            c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
          temporal_resolution <- "day"
          horizon <- 28L
          targets <- paste0(
            1:(horizon + 6), " day ahead ", gsub("_", " ", response_var))
          all_locations <- unique(observed_hosps$location)
        }

        load_covid_forecasts_relative_horizon(
          monday_dates = forecast_dates,
          model_abbrs = model_abbrs,
          timezero_window_size = 6,
          locations = all_locations,
          targets = targets,
          horizon = horizon,
          required_quantiles = required_quantiles,
          submissions_root = submissions_root,
          include_null_point_forecasts = FALSE,
          keep_last = FALSE
        )
      }
    ) %>%
      dplyr::mutate(spatial_scale = spatial_scale)

    return(spatial_scale_forecasts)
  }
)

all_forecasts <- all_forecasts %>%
  dplyr::mutate(
    base_target = substr(target, regexpr(" ", target) + 1, nchar(target)),
    model_and_scale = paste0(model, "-estimation_scale_", spatial_scale)
  )

# calculate scores
get_scores <- function(
  qfm,
  observed_by_location_target_end_date) {
  row_index <- attr(qfm, 'row_index')
  col_index <- attr(qfm, 'col_index')

  y_test <- row_index %>%
    dplyr::mutate(
      base_target = substr(target, regexpr(" ", target) + 1, nchar(target)),
      target_end_date = as.character(lubridate::ymd(forecast_week_end_date) +
                                       as.numeric(substr(target, 1, regexpr(" ", target) - 1)) * 7)
    ) %>%
    dplyr::left_join(
      observed_by_location_target_end_date,
      by = c('location', 'target_end_date', 'base_target')
    ) %>%
    pull(observed)

  row_index$pit <- NA_real_
  for(i in seq_len(nrow(qfm))) {
    if(!is.na(y_test[i])) {
      qfm_row <- unclass(qfm)[i, ]
      if(y_test[i] < qfm_row[1]) {
        row_index$pit[i] <- as.numeric(col_index$quantile[1])
      } else if(y_test[i] > tail(qfm_row, 1)) {
        row_index$pit[i] <- 1.0
      } else if(any(qfm_row == y_test[i])) {
        row_index$pit[i] <- col_index$quantile[qfm_row == y_test[i]] %>%
          as.numeric() %>%
          median()
      } else {
        start_ind <- max(which(unclass(qfm)[i, ] < y_test[i]))
        row_index$pit[i] <- approx(
          x = qfm_row[start_ind:(start_ind+1)],
          y = as.numeric(col_index$quantile[start_ind:(start_ind+1)]),
          xout = y_test[i],
          method = "linear"
        )$y
      }
    }
  }

  for (i in seq_len((nrow(col_index) - 1) / 2)) {
    coverage_name <- paste0(
      "coverage_",
      format(1 - as.numeric(col_index$quantile[i]) * 2, nsmall = 2, digits = 2))
    wis_name <- paste0(
      "wis_",
      format(as.numeric(col_index$quantile[i]) * 2, nsmall = 2, digits = 2))
    wiw_name <- paste0(
      "wiw_",
      format(as.numeric(col_index$quantile[i]) * 2, nsmall = 2, digits = 2))
    wip_name <- paste0(
      "wip_",
      format(as.numeric(col_index$quantile[i]) * 2, nsmall = 2, digits = 2))

    pred_quantiles <- qfm[, c(i, nrow(col_index) + 1 - i)]
    row_index[[coverage_name]] <- (unclass(pred_quantiles)[, 1] <= y_test) &
      (unclass(pred_quantiles)[, 2] >= y_test)
    row_index[[wis_name]] <- covidEnsembles::wis(y_test, pred_quantiles)
    row_index[[wiw_name]] <- covidEnsembles::wiw(y_test, pred_quantiles)
    row_index[[wip_name]] <- covidEnsembles::wip(y_test, pred_quantiles)
  }

  i <- i + 1
  row_index[["wis_1"]] <- abs(y_test - unclass(qfm[, i]))
  row_index[["wiw_1"]] <- 0.0
  row_index[["wip_1"]] <- abs(y_test - unclass(qfm[, i]))

  row_index[["wis"]] <- covidEnsembles::wis(y_test, qfm)

  for (i in seq_len(nrow(col_index))) {
    coverage_name <- paste0("one_sided_coverage_", col_index$quantile[i])

    pred_quantiles <- qfm[, i]
    row_index[[coverage_name]] <- (y_test <= unclass(pred_quantiles)[, 1])
  }

  row_index$observed_inc <- row_index %>%
    dplyr::mutate(
      base_target = "wk ahead inc death",
      target_end_date = as.character(
        lubridate::ymd(forecast_week_end_date) +
                       as.numeric(substr(target, 1, regexpr(" ", target) - 1)) * 7)
    ) %>%
    dplyr::left_join(
      observed_by_location_target_end_date,
      by = c("location", "target_end_date", "base_target")
    ) %>%
    dplyr::pull(observed)

  row_index$observed <- y_test

  return(row_index[!is.na(y_test), ])
}

# scores for forecasts available for all models
all_scores <- purrr::map_dfr(
  unique(all_forecasts$model_and_scale),
  function(model_and_scale) {
    # extract model case
    model_case <- suppressMessages(parse_model_case(model_and_scale)) %>%
      dplyr::mutate(model_and_scale = model_and_scale, join_field = "temp")

    if(nrow(model_case) == 0) {
      model_case <- data.frame(
        model_and_scale = model_and_scale,
        join_field = "temp",
        stringsAsFactors = FALSE
      )
    }

    model_forecasts <- all_forecasts %>%
        dplyr::filter(model_and_scale == UQ(model_and_scale))

    purrr::map_dfr(
      unique(model_forecasts$target),
      function(target) {
        model_qfm <- covidEnsembles::new_QuantileForecastMatrix_from_df(
          forecast_df = model_forecasts %>%
            dplyr::filter(
              target == UQ(target)
            ),
          model_col = 'model_and_scale',
          id_cols = c('location', 'forecast_week_end_date', 'target'),
          quantile_name_col = 'quantile',
          quantile_value_col = 'value',
          drop_missing_id_levels = TRUE
        )

        get_scores(
          qfm = model_qfm,
          observed_by_location_target_end_date = observed) %>%
          dplyr::mutate(join_field = "temp") %>%
          dplyr::left_join(model_case, by = "join_field") %>%
          dplyr::select(-join_field)
      }) %>%
      dplyr::mutate(model_and_scale = model_and_scale)
  })

saveRDS(
  all_scores,
  "code/application/retrospective-qra-comparison/analyses/retrospective-scores/retrospective_scores.rds")
