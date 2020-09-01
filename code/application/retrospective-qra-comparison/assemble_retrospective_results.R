library(covidData)
library(covidEnsembles)
library(tidyverse)
library(gridExtra)

# Path to forecasts to evaluate
submissions_root <-
  "code/application/retrospective-qra-comparison/retrospective-forecasts/"

# date for "truth" data used to compute scores and used in plots
issue_date <- max(covidData::jhu_deaths_data$issue_date)

# last target date to evaluate: most recent Saturday with observed data
last_target_date <- lubridate::ymd(issue_date) %>%
  lubridate::floor_date(unit = "week") %>%
  `-`(1) %>%
  as.character()

# dates for saturdays included in the analysis:
#  - we consider ensemble forecasts generated 2 days after this saturday date
#  - week ahead targets are defined relative to this saturday date
first_forecast_week_end_date <- lubridate::ymd("2020-05-09")
last_forecast_week_end_date <- lubridate::ymd(last_target_date) - 7
num_forecast_weeks <- as.integer(last_forecast_week_end_date -
                         first_forecast_week_end_date) / 7 + 1
forecast_week_end_dates <- as.character(
  first_forecast_week_end_date +
    seq(from = 0, length = num_forecast_weeks) * 7
)

# Dates of forecast submission for forecasts included in this analysis:
# 2 days after the saturdays
forecast_dates <- lubridate::ymd(forecast_week_end_dates) + 2

# Load observed values
observed_deaths <-
  covidData::load_jhu_data(
    issue_date = issue_date,
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
    issue_date = issue_date,
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

observed <- dplyr::bind_rows(observed_deaths, observed_cases)

# Load forecasts
all_targets <- c(
  paste0(1:4, " wk ahead cum death"),
  paste0(1:4, " wk ahead inc death"),
  paste0(1:8, " wk ahead inc case")
)

model_abbrs <- list.dirs(submissions_root, full.names = FALSE)
model_abbrs <- model_abbrs[nchar(model_abbrs) > 0]

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
        stop("Unsupported case part")
      }
    }
  )
}

all_forecasts <- purrr::map_dfr (
  model_abbrs,
  function (model_abbr) {
    # get paths to results files for this model_abbr
    results_paths <- Sys.glob(paste0(submissions_root, model_abbr,
      "/*", forecast_dates, "-", model_abbr, ".csv"))
    
    # if no results, return
    if (length(results_paths) == 0) {
      return(NULL)
    }

    # extract model case
    model_case <- parse_model_case(model_abbr) %>%
      dplyr::mutate(join_field = "temp")

    purrr::map_dfr(
      results_paths,
      function(results_path) {
        readr::read_csv(
          results_path,
          col_types = cols(
            forecast_date = col_date(format = ""), 
            target = col_character(),
            target_end_date = col_date(format = ""), 
            location = col_character(),
            type = col_character(), 
            quantile = col_double(),
            value = col_double()
          )) %>%
          dplyr::filter(
            tolower(type) == "quantile",
            tolower(target) %in% all_targets) %>%
          dplyr::transmute(
            model = model_abbr, 
            timezero = forecast_date,
            location = location,
            target = tolower(target),
            quantile = quantile,
            value = value,
            join_field = "temp") %>%
          dplyr::left_join(model_case, by = "join_field") %>%
          dplyr::select(-join_field)
      })
    }
  )


# Calculate scores for forecasts
get_scores <- function(
  qfm,
  observed_by_location_target_end_date) {
  row_index <- attr(qfm, 'row_index')
  col_index <- attr(qfm, 'col_index')

  y_test <- row_index %>%
    dplyr::mutate(
      base_target = substr(target, 3, nchar(target)),
      target_end_date = as.character(lubridate::ymd(forecast_week_end_date) +
                                       as.numeric(substr(target, 1, 1)) * 7)
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
                       as.numeric(substr(target, 1, 1)) * 7)
    ) %>%
    dplyr::left_join(
      observed_by_location_target_end_date,
      by = c("location", "target_end_date", "base_target")
    ) %>%
    dplyr::pull(observed)

  row_index$observed <- y_test

  return(row_index[!is.na(y_test), ])
}

all_scores <- purrr::map_dfr(
  unique(all_forecasts$model),
  function(model) {
    # extract model case
    model_case <- parse_model_case(model) %>%
      dplyr::mutate(model = model, join_field = "temp")
    
    model_qfm <- covidEnsembles::new_QuantileForecastMatrix_from_df(
      forecast_df = all_forecasts %>%
        dplyr::filter(model == UQ(model)) %>%
        dplyr::mutate(
          forecast_week_end_date = as.character(lubridate::ymd(timezero) - 2)
        ),
      model_col = 'model',
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
  })

scores_by_week <- all_scores %>%
  dplyr::filter(forecast_week_end_date > "2020-05-09") %>%
  dplyr::mutate(
    base_target = substr(target, 3, nchar(target)),
    location_type = ifelse(location == "US", "US", "State")) %>%
  dplyr::group_by(
    model, intercept, combine_method, missingness, quantile_groups,
    window_size, check_missingness_by_target, do_standard_checks,
    do_baseline_check, forecast_week_end_date, base_target, location_type) %>%
  dplyr::summarize(
    across(starts_with("wis"), mean),
    across(starts_with("wiw"), mean),
    across(starts_with("wip"), mean),
    across(starts_with("coverage"), mean),
    across(starts_with("one_sided_coverage"), mean)
  )

scores_overall <- all_scores %>%
  dplyr::filter(forecast_week_end_date > "2020-05-09") %>%
  dplyr::mutate(
    base_target = substr(target, 3, nchar(target)),
    location_type = ifelse(location == "US", "US", "State")
  ) %>%
  dplyr::group_by(
    model, intercept, combine_method, missingness, quantile_groups,
    window_size, check_missingness_by_target, do_standard_checks,
    do_baseline_check, base_target, location_type) %>%
  dplyr::summarize(
    across(starts_with("wis"), mean),
    across(starts_with("wiw"), mean),
    across(starts_with("wip"), mean),
    across(starts_with("coverage"), mean),
    across(starts_with("one_sided_coverage"), mean)
  )


# In a second set of scores, pull out dates affected by reporting anomalies --
# specifically, switches in reporting from confirmed to probable + confirmed

# this list assembled from https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data#data-modification-records
anomalies <- dplyr::bind_rows(
  data.frame(location = "36", target_end_date = lubridate::ymd("2020-04-25")), # NYC April 23
  data.frame(location = "08", target_end_date = lubridate::ymd("2020-04-25")), # CO April 24
  data.frame(location = "26", target_end_date = lubridate::ymd("2020-06-06")), # MI June 5
  data.frame(location = "29", target_end_date = lubridate::ymd("2020-06-13")), # MO June 12
  data.frame(location = "25", target_end_date = lubridate::ymd("2020-06-13")), # MA June 12
  data.frame(location = "28", target_end_date = lubridate::ymd("2020-06-27")), # MS June 22
  data.frame(location = "10", target_end_date = lubridate::ymd("2020-06-27")), # DE June 23
  data.frame(location = "34", target_end_date = lubridate::ymd("2020-06-27")), # NJ June 25
  data.frame(location = "36", target_end_date = lubridate::ymd("2020-07-04")), # NY June 30
  data.frame(location = "48", target_end_date = lubridate::ymd("2020-08-01"))  # TX July 27
)

# a forecast may be affected by a given reporting anomaly if:
#   * it is a forecast of incident deaths...
#     - for that date, at any horizon
#     - potentially later dates made that week, if the model was affected by
#       the reporting anomaly (not included in this analysis)
#   * it is a forecast of cumulative deaths...
#     - for that date, at any horizon
#     - for that date + 7, at a horizon of 2 or more
#     - for that date + 14, at a horizon of 3 or more
#     - for that date + 21, at a horizon of 4
#     - potentially also other dates, if the model was affected by the
#       reporting anomaly (not included in this analysis)
affected_forecasts <- purrr::map_dfr(
  seq_len(nrow(anomalies)),
  function(i) {
    dplyr::bind_rows(
      tidyr::expand_grid(
        base_target = "wk ahead inc death",
        location = anomalies$location[i],
        target_end_date = anomalies$target_end_date[i],
        horizon = 1:4
      ),
      tidyr::expand_grid(
        base_target = "wk ahead cum death",
        location = anomalies$location[i],
        target_end_date = anomalies$target_end_date[i],
        horizon = 1:4
      ),
      tidyr::expand_grid(
        base_target = "wk ahead cum death",
        location = anomalies$location[i],
        target_end_date = anomalies$target_end_date[i] + 7,
        horizon = 2:4
      ),
      tidyr::expand_grid(
        base_target = "wk ahead cum death",
        location = anomalies$location[i],
        target_end_date = anomalies$target_end_date[i] + 14,
        horizon = 3:4
      ),
      tidyr::expand_grid(
        base_target = "wk ahead cum death",
        location = anomalies$location[i],
        target_end_date = anomalies$target_end_date[i] + 21,
        horizon = 4
      )
    )
  }
) %>%
  dplyr::mutate(affected = TRUE)

all_scores <- all_scores %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 1)),
    base_target = substr(target, 3, nchar(target)),
    target_end_date = lubridate::ymd(forecast_week_end_date) + horizon*7
  ) %>%
  dplyr::left_join(
    affected_forecasts,
    by = c("base_target", "location", "target_end_date", "horizon")) %>%
  dplyr::mutate(
    reporting_anomaly = ifelse(is.na(affected), FALSE, affected)
  )


scores_overall_by_reporting_anomaly_status <- all_scores %>%
  dplyr::filter(forecast_week_end_date > "2020-05-09") %>%
  dplyr::mutate(
    base_target = substr(target, 3, nchar(target)),
    location_type = ifelse(location == "US", "US", "State")
  ) %>%
  dplyr::group_by(
    model, intercept, combine_method, missingness, quantile_groups,
    window_size, check_missingness_by_target, do_standard_checks,
    do_baseline_check, base_target, location_type, reporting_anomaly) %>%
  dplyr::summarize(
    across(starts_with("wis"), mean),
    across(starts_with("wiw"), mean),
    across(starts_with("wip"), mean),
    across(starts_with("coverage"), mean),
    across(starts_with("one_sided_coverage"), mean)
  )



# Plots of coverage, WIS, WIW, WIP
wis_two_models <- scores_overall %>%
  dplyr::filter(
    model %in% c(
      "intercept_FALSE-constraint_convex-missingness_impute-quantile_groups_per_quantile-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE",
#      "intercept_FALSE-constraint_convex-missingness_impute-quantile_groups_3_groups-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE",
#      "intercept_FALSE-constraint_convex-missingness_impute-quantile_groups_per_model-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE",
      "intercept_TRUE-constraint_positive-missingness_impute-quantile_groups_per_quantile-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE",
#      "intercept_TRUE-constraint_positive-missingness_impute-quantile_groups_3_groups-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE",
#      "intercept_TRUE-constraint_positive-missingness_impute-quantile_groups_per_model-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE",
      "intercept_FALSE-constraint_ew-missingness_by_location_group-quantile_groups_per_model-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE"
    )
  ) %>%
  dplyr::mutate(
    model = dplyr::case_when(
      model == "intercept_FALSE-constraint_convex-missingness_impute-quantile_groups_per_quantile-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "convex_per_quantile",
      model == "intercept_FALSE-constraint_convex-missingness_impute-quantile_groups_3_groups-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "convex_3_groups",
      model == "intercept_FALSE-constraint_convex-missingness_impute-quantile_groups_per_model-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "convex_per_model",
      model == "intercept_TRUE-constraint_positive-missingness_impute-quantile_groups_per_quantile-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "positive_per_quantile",
      model == "intercept_TRUE-constraint_positive-missingness_impute-quantile_groups_3_groups-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "positive_3_groups",
      model == "intercept_TRUE-constraint_positive-missingness_impute-quantile_groups_per_model-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "positive_per_model",
      model == "intercept_FALSE-constraint_ew-missingness_by_location_group-quantile_groups_per_model-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "equal"
    )
  )

wis_two_models <- dplyr::bind_rows(
  wis_two_models %>%
    dplyr::ungroup() %>%
    dplyr::select(c(base_target, model, location_type, starts_with('wis_'))) %>%
    tidyr::pivot_longer(
      starts_with("wis_"),
      names_to = "interval_alpha",
      names_prefix = "wis_",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      one_minus_interval_alpha = 1 - as.numeric(interval_alpha),
      measure = "wis") %>%
    dplyr::select(-interval_alpha),
  wis_two_models %>%
    dplyr::ungroup() %>%
    dplyr::select(c(base_target, model, location_type, starts_with('wiw_'))) %>%
    tidyr::pivot_longer(
      starts_with("wiw_"),
      names_to = "interval_alpha",
      names_prefix = "wiw_",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      one_minus_interval_alpha = 1 - as.numeric(interval_alpha),
      measure = "weighted interval width") %>%
    dplyr::select(-interval_alpha),
  wis_two_models %>%
    dplyr::ungroup() %>%
    dplyr::select(c(base_target, model, location_type, starts_with('wip_'))) %>%
    tidyr::pivot_longer(
      starts_with("wip_"),
      names_to = "interval_alpha",
      names_prefix = "wip_",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      one_minus_interval_alpha = 1 - as.numeric(interval_alpha),
      measure = "weighted interval penalty") %>%
    dplyr::select(-interval_alpha),
  wis_two_models %>%
    dplyr::ungroup() %>%
    dplyr::select(c(base_target, model, location_type, starts_with('coverage_'))) %>%
    tidyr::pivot_longer(
      cols = grep("^coverage", colnames(.), value = TRUE),
      names_to = 'one_minus_interval_alpha',
      names_prefix = 'coverage_',
      values_to = 'value') %>%
    dplyr::mutate(
      one_minus_interval_alpha = as.numeric(one_minus_interval_alpha),
      measure = "empirical coverage"
    )
)# %>%
#  dplyr::mutate(interval_alpha = as.numeric(interval_alpha))


individual_wis_two_models <- all_scores %>%
  dplyr::mutate(
    base_target = substr(target, 3, nchar(target)),
    location_type = ifelse(location == "US", "US", "State")
  ) %>%
  dplyr::filter(
    model %in% c(
      "intercept_TRUE-constraint_convex-missingness_impute-quantile_groups_per_quantile-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE",
      "intercept_TRUE-constraint_convex-missingness_impute-quantile_groups_3_groups-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE",
      "intercept_TRUE-constraint_convex-missingness_impute-quantile_groups_per_model-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE",
      "intercept_TRUE-constraint_positive-missingness_impute-quantile_groups_per_quantile-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE",
      "intercept_TRUE-constraint_positive-missingness_impute-quantile_groups_3_groups-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE",
      "intercept_TRUE-constraint_positive-missingness_impute-quantile_groups_per_model-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE",
      "intercept_FALSE-constraint_ew-missingness_by_location_group-quantile_groups_per_model-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE"
    )
  ) %>%
  dplyr::mutate(
    model = dplyr::case_when(
      model == "intercept_TRUE-constraint_convex-missingness_impute-quantile_groups_per_quantile-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "convex_per_quantile",
      model == "intercept_TRUE-constraint_convex-missingness_impute-quantile_groups_3_groups-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "convex_3_groups",
      model == "intercept_TRUE-constraint_convex-missingness_impute-quantile_groups_per_model-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "convex_per_model",
      model == "intercept_TRUE-constraint_positive-missingness_impute-quantile_groups_per_quantile-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "positive_per_quantile",
      model == "intercept_TRUE-constraint_positive-missingness_impute-quantile_groups_3_groups-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "positive_3_groups",
      model == "intercept_TRUE-constraint_positive-missingness_impute-quantile_groups_per_model-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "positive_per_model",
      model == "intercept_FALSE-constraint_ew-missingness_by_location_group-quantile_groups_per_model-window_size_5-do_standard_checks_FALSE-do_baseline_check_FALSE" ~ "equal"
    )
  )

individual_wis_two_models <- dplyr::bind_rows(
  individual_wis_two_models %>%
    dplyr::ungroup() %>%
    dplyr::select(c(base_target, model, location_type, starts_with('wis_'))) %>%
    tidyr::pivot_longer(
      starts_with("wis_"),
      names_to = "interval_alpha",
      names_prefix = "wis_",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      one_minus_interval_alpha = 1 - as.numeric(interval_alpha),
      measure = "wis") %>%
    dplyr::select(-interval_alpha),
  individual_wis_two_models %>%
    dplyr::ungroup() %>%
    dplyr::select(c(base_target, model, location_type, starts_with('wiw_'))) %>%
    tidyr::pivot_longer(
      starts_with("wiw_"),
      names_to = "interval_alpha",
      names_prefix = "wiw_",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      one_minus_interval_alpha = 1 - as.numeric(interval_alpha),
      measure = "weighted interval width") %>%
    dplyr::select(-interval_alpha),
  individual_wis_two_models %>%
    dplyr::ungroup() %>%
    dplyr::select(c(base_target, model, location_type, starts_with('wip_'))) %>%
    tidyr::pivot_longer(
      starts_with("wip_"),
      names_to = "interval_alpha",
      names_prefix = "wip_",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      one_minus_interval_alpha = 1 - as.numeric(interval_alpha),
      measure = "weighted interval penalty") %>%
    dplyr::select(-interval_alpha),
  individual_wis_two_models %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols = grep("^coverage", colnames(.), value = TRUE),
      names_to = 'one_minus_interval_alpha',
      names_prefix = 'coverage_',
      values_to = 'value') %>%
    dplyr::mutate(
      one_minus_interval_alpha = as.numeric(one_minus_interval_alpha),
      measure = "empirical coverage"
    )
)


plot_path <- paste0(
  "code/application/retrospective-qra-comparison/ensemble-score-plots/",
  "wis_decomposition.pdf")
pdf(plot_path, width = 18, height = 9)
ggplot() +
  geom_abline(
    data = data.frame(
      intercept = 0.0,
      slope = c(0.0, 0.0, 0.0, 1.0),
      transparency = c(0, 0, 0, 1),
      measure = c("weighted interval width", "weighted interval penalty",
        "wis", "empirical coverage")
    ),
    mapping = aes(
      intercept = intercept,
      slope = slope,
      alpha = transparency),
    show.legend = FALSE
  ) +
  geom_line(
    data = wis_two_models, # %>%
#      dplyr::filter(
#        measure %in% c("wiw", "wip", "wis", "empirical coverage")),
    mapping = aes(x = one_minus_interval_alpha, y = value,
      color = model,
      linetype = model,
      group = model),
    position = "identity"
  ) +
  geom_hline(
    data = wis_two_models %>%
      filter(measure == "wis") %>%
      group_by(location_type, base_target, model) %>%
      summarize(value = mean(value)) %>%
      dplyr::full_join(
        data.frame(
          measure = c("weighted interval width", "weighted interval penalty",
            "wis", "empirical coverage")),
        by = character()) %>%
      dplyr::mutate(
        value = ifelse(measure == "wis", value, 0.0),
        transparency = as.numeric(measure == "wis")
      ),
    mapping = aes(yintercept = value, color = model, alpha = transparency, linetype = model),
    show.legend = FALSE
  ) +
  ylim(0, NA) +
  facet_wrap(measure ~ location_type + base_target, scales = "free_y") +
  theme_bw()
dev.off()


ggplot() +
  geom_boxplot(
    data = individual_wis_two_models %>%
      dplyr::filter(
        measure %in% c("weighted interval width", "weighted interval penalty",
                       "wis")),
    mapping = aes(x = one_minus_interval_alpha, y = value,
      color = constraint,
      group = constraint),
    position = "dodge"
  ) +
#  ylim(0, NA) +
  scale_y_log10() +
  facet_wrap(measure ~ location_type + base_target, scales = "free_y") +
  theme_bw()




# Plots of coverage
coverage_by_week <- dplyr::bind_rows(
  scores_by_week %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols = grep("^coverage", colnames(.), value = TRUE),
      names_to = 'nominal_coverage',
      names_prefix = 'coverage_',
      values_to = 'empirical_coverage') %>%
    dplyr::mutate(
      nominal_coverage = as.numeric(nominal_coverage),
      combine_method_and_quantile_groups = paste0(combine_method, "-", quantile_groups),
      coverage_type = "interval"
    ),
  scores_by_week %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols = grep("^one_sided_coverage", colnames(.), value = TRUE),
      names_to = 'nominal_coverage',
      names_prefix = 'one_sided_coverage_',
      values_to = 'empirical_coverage') %>%
    dplyr::mutate(
      nominal_coverage = as.numeric(nominal_coverage),
      combine_method_and_quantile_groups = paste0(combine_method, "-", quantile_groups),
      coverage_type = "quantile"
    )
)

coverage_overall <- dplyr::bind_rows(
  scores_overall %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols = grep("^coverage", colnames(.), value = TRUE),
      names_to = 'nominal_coverage',
      names_prefix = 'coverage_',
      values_to = 'empirical_coverage') %>%
    dplyr::mutate(
      nominal_coverage = as.numeric(nominal_coverage),
      combine_method_and_quantile_groups = paste0(combine_method, "-", quantile_groups),
      coverage_type = "interval"
    ),
  scores_overall %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols = grep("^one_sided_coverage", colnames(.), value = TRUE),
      names_to = 'nominal_coverage',
      names_prefix = 'one_sided_coverage_',
      values_to = 'empirical_coverage') %>%
    dplyr::mutate(
      nominal_coverage = as.numeric(nominal_coverage),
      combine_method_and_quantile_groups = paste0(combine_method, "-", quantile_groups),
      coverage_type = "quantile"
    )
)


coverage_by_reporting_anomaly_overall <- dplyr::bind_rows(
  scores_overall_by_reporting_anomaly_status %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols = grep("^coverage", colnames(.), value = TRUE),
      names_to = 'nominal_coverage',
      names_prefix = 'coverage_',
      values_to = 'empirical_coverage') %>%
    dplyr::mutate(
      nominal_coverage = as.numeric(nominal_coverage),
      combine_method_and_quantile_groups = paste0(combine_method, "-", quantile_groups),
      coverage_type = "interval"
    ),
  scores_overall_by_reporting_anomaly_status %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols = grep("^one_sided_coverage", colnames(.), value = TRUE),
      names_to = 'nominal_coverage',
      names_prefix = 'one_sided_coverage_',
      values_to = 'empirical_coverage') %>%
    dplyr::mutate(
      nominal_coverage = as.numeric(nominal_coverage),
      combine_method_and_quantile_groups = paste0(combine_method, "-", quantile_groups),
      coverage_type = "quantile"
    )
)


model_case_vars <- c("combine_method", "missingness",
  "quantile_groups", "window_size",  "do_standard_checks", "do_baseline_check",
  "check_missingness_by_target")
for(model_case_var in model_case_vars) {
  for(measure in c("cum_death", "inc_death")) {
    plot_path <- paste0(
      "code/application/retrospective-qra-comparison/ensemble-score-plots/",
      "calibration-", measure, "-", model_case_var, "-by_week.pdf")
    pdf(plot_path, width = 24, height = 14)
    p <- ggplot(
      data = coverage_by_week %>%
        dplyr::filter(
          location_type != "US",
          base_target == paste0("wk ahead ", gsub("_", " ", measure)))) +
      geom_line(
        mapping = aes_string(
          x = "nominal_coverage",
          y = "empirical_coverage",
          color = model_case_var,
          linetype = model_case_var,
          group = "model")
      ) +
      geom_point(
        mapping = aes_string(
          x = "nominal_coverage",
          y = "empirical_coverage",
          color = model_case_var,
          shape = model_case_var)
      ) +
      geom_abline(intercept = 0, slope = 1) +
      facet_grid(coverage_type ~ forecast_week_end_date) +
      theme_bw()
    print(p)

    p <- ggplot(
      data = coverage_by_week %>%
        dplyr::filter(
          location_type == "US",
          base_target == paste0("wk ahead ", gsub("_", " ", measure)))) +
      geom_line(
        mapping = aes_string(
          x = "nominal_coverage",
          y = "empirical_coverage",
          color = model_case_var,
          linetype = model_case_var,
          group = "model")
      ) +
      geom_point(
        mapping = aes_string(
          x = "nominal_coverage",
          y = "empirical_coverage",
          color = model_case_var,
          shape = model_case_var)
      ) +
      geom_abline(intercept = 0, slope = 1) +
      facet_grid(coverage_type ~ forecast_week_end_date) +
      theme_bw()
    print(p)
    dev.off()

    plot_path <- paste0(
      "code/application/retrospective-qra-comparison/ensemble-score-plots/",
      "calibration-", measure, "-", model_case_var, "-overall.pdf")
    pdf(plot_path, width = 24, height = 14)
    p <- ggplot(
      data = coverage_overall %>%
        dplyr::filter(
          base_target == paste0("wk ahead ", gsub("_", " ", measure)))) +
      geom_line(
        mapping = aes_string(
          x = "nominal_coverage",
          y = "empirical_coverage",
          color = model_case_var,
          linetype = model_case_var,
          group = "model")
      ) +
      geom_point(
        mapping = aes_string(
          x = "nominal_coverage",
          y = "empirical_coverage",
          color = model_case_var,
          shape = model_case_var)
      ) +
      geom_abline(intercept = 0, slope = 1) +
      facet_grid(coverage_type ~ location_type) +
      theme_bw()
    print(p)
    dev.off()
  }
}


model_case_vars <- c("missingness",
  "quantile_groups", "window_size", "check_missingness_by_target",
  "do_standard_checks", "do_baseline_check")
for(model_case_var in model_case_vars) {
  plot_path <- paste0(
        "code/application/retrospective-qra-comparison/ensemble-score-plots/",
        "calibration-cum_inc_death-", model_case_var,
        "-facet_combine_method-overall.pdf")
  pdf(plot_path, width = 24, height = 14)
  p <- ggplot(
    data = coverage_overall %>%
      dplyr::filter(location_type != "US")) +
    geom_line(
      mapping = aes_string(
        x = "nominal_coverage",
        y = "empirical_coverage",
        color = model_case_var,
        linetype = model_case_var,
        group = "model")
    ) +
    geom_point(
      mapping = aes_string(
        x = "nominal_coverage",
        y = "empirical_coverage",
        color = model_case_var,
        shape = model_case_var)
    ) +
    geom_abline(intercept = 0, slope = 1) +
    facet_grid(coverage_type ~ base_target + combine_method) +
    theme_bw()
  print(p)

  p <- ggplot(
    data = coverage_overall %>%
      dplyr::filter(location_type == "US")) +
    geom_line(
      mapping = aes_string(
        x = "nominal_coverage",
        y = "empirical_coverage",
        color = model_case_var,
        linetype = model_case_var,
        group = "model")
    ) +
    geom_point(
      mapping = aes_string(
        x = "nominal_coverage",
        y = "empirical_coverage",
        color = model_case_var,
        shape = model_case_var)
    ) +
    geom_abline(intercept = 0, slope = 1) +
    facet_grid(coverage_type ~ base_target + combine_method) +
    theme_bw()
  print(p)
  dev.off()
}


model_case_vars <- c(
  "quantile_groups", "window_size", "check_missingness_by_target",
  "do_standard_checks", "do_baseline_check")
for(model_case_var in model_case_vars) {
  plot_path <- paste0(
        "code/application/retrospective-qra-comparison/ensemble-score-plots/",
        "calibration-cum_inc_death-filter_missingness_impute-", model_case_var,
        "-facet_combine_method-overall.pdf")
  pdf(plot_path, width = 24, height = 14)
  p <- ggplot(
    data = coverage_overall %>%
      dplyr::filter(
        location_type != "US",
        missingness == "impute" | combine_method == "ew")) +
    geom_line(
      mapping = aes_string(
        x = "nominal_coverage",
        y = "empirical_coverage",
        color = model_case_var,
        linetype = model_case_var,
        group = "model")
    ) +
    geom_point(
      mapping = aes_string(
        x = "nominal_coverage",
        y = "empirical_coverage",
        color = model_case_var,
        shape = model_case_var)
    ) +
    geom_abline(intercept = 0, slope = 1) +
    facet_grid(coverage_type ~ base_target + combine_method) +
    theme_bw()
  print(p)

  p <- ggplot(
    data = coverage_overall %>%
      dplyr::filter(
        location_type == "US",
        missingness == "impute" | combine_method == "ew")) +
    geom_line(
      mapping = aes_string(
        x = "nominal_coverage",
        y = "empirical_coverage",
        color = model_case_var,
        linetype = model_case_var,
        group = "model")
    ) +
    geom_point(
      mapping = aes_string(
        x = "nominal_coverage",
        y = "empirical_coverage",
        color = model_case_var,
        shape = model_case_var)
    ) +
    geom_abline(intercept = 0, slope = 1) +
    facet_grid(coverage_type ~ base_target + combine_method) +
    theme_bw()
  print(p)
  dev.off()
}




model_case_vars <- c(
  "quantile_groups", "window_size", "check_missingness_by_target",
  "do_standard_checks", "do_baseline_check")
for(model_case_var in model_case_vars) {
  plot_path <- paste0(
        "code/application/retrospective-qra-comparison/ensemble-score-plots/",
        "calibration-cum_inc_death-filter_missingness_impute-", model_case_var,
        "-drop_reporting_anomaly-facet_combine_method-overall.pdf")
  pdf(plot_path, width = 24, height = 14)
  p <- ggplot(
    data = coverage_by_reporting_anomaly_overall %>%
      dplyr::filter(
        location_type != "US",
        !reporting_anomaly,
        missingness == "impute" | combine_method == "ew")) +
    geom_line(
      mapping = aes_string(
        x = "nominal_coverage",
        y = "empirical_coverage",
        color = model_case_var,
        linetype = model_case_var,
        group = "model")
    ) +
    geom_point(
      mapping = aes_string(
        x = "nominal_coverage",
        y = "empirical_coverage",
        color = model_case_var,
        shape = model_case_var)
    ) +
    geom_abline(intercept = 0, slope = 1) +
    facet_grid(coverage_type ~ base_target + combine_method) +
    theme_bw()
  print(p)

  p <- ggplot(
    data = coverage_by_reporting_anomaly_overall %>%
      dplyr::filter(
        location_type == "US",
        !reporting_anomaly,
        missingness == "impute" | combine_method == "ew")) +
    geom_line(
      mapping = aes_string(
        x = "nominal_coverage",
        y = "empirical_coverage",
        color = model_case_var,
        linetype = model_case_var,
        group = "model")
    ) +
    geom_point(
      mapping = aes_string(
        x = "nominal_coverage",
        y = "empirical_coverage",
        color = model_case_var,
        shape = model_case_var)
    ) +
    geom_abline(intercept = 0, slope = 1) +
    facet_grid(coverage_type ~ base_target + combine_method) +
    theme_bw()
  print(p)
  dev.off()
}


model_case_vars <- c("window_size", "check_missingness_by_target")
for(model_case_var in model_case_vars) {
  plot_path <- paste0(
        "code/application/retrospective-qra-comparison/ensemble-score-plots/",
        "calibration-cum_inc_death-filter_missingness_impute-filter_combine_method_positive-filter_quantile_groups_per_quantile", model_case_var,
        "-facet_location_type-overall.pdf")
  pdf(plot_path, width = 24, height = 14)
  p <- ggplot(
    data = coverage_overall %>%
      dplyr::filter(
        missingness == "impute",
        combine_method == "positive",
        quantile_groups == "per_quantile")) +
    geom_line(
      mapping = aes_string(
        x = "nominal_coverage",
        y = "empirical_coverage",
        color = model_case_var,
        linetype = model_case_var,
        group = "model")
    ) +
    geom_point(
      mapping = aes_string(
        x = "nominal_coverage",
        y = "empirical_coverage",
        color = model_case_var,
        shape = model_case_var)
    ) +
    geom_abline(intercept = 0, slope = 1) +
    facet_grid(location_type ~ base_target) +
    theme_bw()
  print(p)
  dev.off()
}



# Plots of mean absolute error
mae_by_week_and_location_type <- all_scores %>%
  dplyr::filter(forecast_week_end_date > "2020-05-09") %>%
  dplyr::mutate(
    base_target = substr(target, 3, nchar(target)),
    location_type = ifelse(location == "US", "US", "State")
  ) %>%
  dplyr::group_by(
    model, intercept, combine_method, missingness, quantile_groups,
    window_size, check_missingness_by_target, do_standard_checks,
    do_baseline_check, forecast_week_end_date, base_target, location_type) %>%
  dplyr::summarize(mae = mean(wis_1)) %>%
  dplyr::ungroup()

mae_by_location_type <- all_scores %>%
  dplyr::filter(forecast_week_end_date > "2020-05-09") %>%
  dplyr::mutate(
    base_target = substr(target, 3, nchar(target)),
    location_type = ifelse(location == "US", "US", "State")
  ) %>%
  dplyr::group_by(
    model, intercept, combine_method, missingness, quantile_groups,
    window_size, check_missingness_by_target, do_standard_checks,
    do_baseline_check, base_target, location_type) %>%
  dplyr::summarize(mae = mean(wis_1)) %>%
  dplyr::ungroup()


model_case_vars <- c("missingness",
  "quantile_groups", "window_size", "check_missingness_by_target",
  "do_standard_checks", "do_baseline_check")
for(model_case_var in model_case_vars) {
  plot_path <- paste0(
    "code/application/retrospective-qra-comparison/ensemble-score-plots/",
    "mae-cum_inc_death-", model_case_var, "-overall.pdf")
  pdf(plot_path, width = 12, height = 7)
  p <- ggplot(data = mae_by_location_type %>%
    dplyr::mutate(case_and_constraint = paste0(combine_method, get(model_case_var)))) +
    geom_point(
      mapping = aes_string(
        x = "combine_method",
        y = "mae",
        color = model_case_var,
        shape = model_case_var,
        group = "case_and_constraint"),
      position = position_dodge(width = 0.75)
    ) +
    facet_wrap(~ location_type + base_target, ncol = 2, scales = "free_y") +
    theme_bw()
  print(p)
  dev.off()
}




# Plots of weighted interval score
wis_by_week_and_location_type <- all_scores %>%
  dplyr::filter(forecast_week_end_date > "2020-05-09") %>%
  dplyr::mutate(
    base_target = substr(target, 3, nchar(target)),
    location_type = ifelse(location == "US", "US", "State")
  ) %>%
  dplyr::group_by(
    model, intercept, combine_method, missingness, quantile_groups,
    window_size, check_missingness_by_target, do_standard_checks,
    do_baseline_check, forecast_week_end_date, base_target, location_type) %>%
  dplyr::summarize(wis = mean(wis)) %>%
  dplyr::ungroup()

wis_by_location_type <- all_scores %>%
  dplyr::filter(
    forecast_week_end_date > "2020-05-09",
    wis < 1e8
  ) %>%
  dplyr::mutate(
    base_target = substr(target, 3, nchar(target)),
    location_type = ifelse(location == "US", "US", "State")
  ) %>%
  dplyr::group_by(
    model, intercept, combine_method, missingness, quantile_groups,
    window_size, check_missingness_by_target, do_standard_checks,
    do_baseline_check, base_target, location_type) %>%
  dplyr::summarize(wis = mean(wis)) %>%
  dplyr::ungroup()

# wis_by_location_type %>%
#   dplyr::filter(model == "intercept_FALSE-constraint_ew-missingness_by_location_group-quantile_groups_per_model-window_size_4-do_standard_checks_FALSE-do_baseline_check_FALSE") %>%
#   dplyr::select(location_type, base_target, wis) %>%
#   dplyr::arrange(location_type, base_target)

model_case_vars <- c("missingness", "check_missingness_by_target",
  "quantile_groups", "window_size", "do_standard_checks", "do_baseline_check")
for(model_case_var in model_case_vars) {
  plot_path <- paste0(
    "code/application/retrospective-qra-comparison/ensemble-score-plots/",
    "wis-cum_inc_death-", model_case_var, "-overall.pdf")
  pdf(plot_path, width = 12, height = 7)
  p <- ggplot(data = wis_by_location_type %>%
    dplyr::mutate(case_and_constraint = paste0(combine_method, get(model_case_var)))) +
    geom_point(
      mapping = aes_string(
        x = "combine_method",
        y = "wis",
        color = model_case_var,
        shape = model_case_var,
        group = "case_and_constraint"),
      position = position_dodge(width = 0.75)
    ) +
    facet_wrap(~ location_type + base_target, ncol = 2, scales = "free_y") +
    theme_bw()
  print(p)
  dev.off()
}


wis_by_location_type_and_reporting_anomaly <- all_scores %>%
#  dplyr::filter(forecast_week_end_date > "2020-05-09") %>%
  dplyr::mutate(
    base_target = substr(target, 3, nchar(target)),
    location_type = ifelse(location == "US", "US", "State")
  ) %>%
  dplyr::group_by(
    model, intercept, combine_method, missingness, quantile_groups,
    window_size, check_missingness_by_target, do_standard_checks,
    do_baseline_check, base_target, location_type, reporting_anomaly) %>%
  dplyr::summarize(wis = mean(wis)) %>%
  dplyr::ungroup()


model_case_vars <- c("missingness",, "check_missingness_by_target",
  "quantile_groups", "window_size", "do_standard_checks", "do_baseline_check")
for(model_case_var in model_case_vars) {
  plot_path <- paste0(
    "code/application/retrospective-qra-comparison/ensemble-score-plots/",
    "wis-cum_inc_death-", model_case_var, "-reporting_anomaly-overall.pdf")
  pdf(plot_path, width = 12, height = 7)
  p <- ggplot(data = wis_by_location_type_and_reporting_anomaly %>%
    dplyr::mutate(
      case_and_constraint = paste0(combine_method, get(model_case_var), reporting_anomaly),
      case_and_reporting_anomaly = paste0(get(model_case_var), reporting_anomaly)
    )) +
    geom_point(
      mapping = aes_string(
        x = "combine_method",
        y = "wis",
        color = model_case_var,
        shape = "reporting_anomaly",
        group = "case_and_constraint"),
      position = position_dodge(width = 0.75)
    ) +
    facet_wrap(~ location_type + base_target, ncol = 2, scales = "free_y") +
    theme_bw()
  print(p)
  dev.off()
}


model_case_vars <- c("missingness", "check_missingness_by_target",
  "quantile_groups", "window_size", "do_standard_checks", "do_baseline_check")
for(model_case_var in model_case_vars) {
  plot_path <- paste0(
    "code/application/retrospective-qra-comparison/ensemble-score-plots/",
    "wis-cum_inc_death-", model_case_var, "-drop_reporting_anomalies-overall.pdf")
  pdf(plot_path, width = 12, height = 7)
  p <- ggplot(data = wis_by_location_type_and_reporting_anomaly %>%
    dplyr::filter(!reporting_anomaly) %>%
    dplyr::mutate(
      case_and_constraint = paste0(combine_method, get(model_case_var))
    )) +
    geom_point(
      mapping = aes_string(
        x = "combine_method",
        y = "wis",
        color = model_case_var,
        shape = model_case_var,
        group = "case_and_constraint"),
      position = position_dodge(width = 0.75)
    ) +
    facet_wrap(~ location_type + base_target, ncol = 2, scales = "free_y") +
    theme_bw()
  print(p)
  dev.off()
}



# Relationship between training set and test set wis
result_filenames <- Sys.glob(
  "code/application/retrospective-qra-comparison/retrospective-fits/*.rds")

train_set_wis <- purrr::map_dfr(
  result_filenames,
  function(result_filename) {
    # get model abbreviation from file name, which includes full path
    model_abbr <- tail(strsplit(result_filename, "/")[[1]], 1)
    intercept_ind <- regexpr("intercept", model_abbr)
    measure_and_date <- substr(model_abbr, 1, intercept_ind - 2)
    forecast_week_ind <- regexpr("forecast_week", measure_and_date)
    measure <- substr(measure_and_date, 1, forecast_week_ind - 2)
    forecast_week <- substr(measure_and_date, forecast_week_ind + 14, nchar(measure_and_date))
    model_abbr <- substr(model_abbr, intercept_ind, nchar(model_abbr))

    # extract model case
    model_case <- parse_model_case(model_abbr) %>%
      dplyr::mutate(
        measure = measure,
        forecast_week = forecast_week,
        join_field = "temp"
      )

    # read in estimation results
    if(model_case$missingness == "impute") {
      c(model_eligibility, wide_model_eligibility, location_groups,
        weight_transfer, component_forecasts) %<-%
          readRDS(result_filename)

      # extract ensemble wis per training set observation
      train_set_wis_one_model <- location_groups$qfm_train %>%
        attr("row_index") %>%
        dplyr::mutate(
          train_wis = covidEnsembles::wis(
            y = location_groups$y_train,
            qfm = predict(
              location_groups$qra_fit,
              qfm = location_groups$qfm_train
            )
          ),
          join_field = "temp"
        ) %>%
        dplyr::left_join(model_case, by = "join_field") %>%
        dplyr::select(-join_field)
    } else {
      c(model_eligibility, wide_model_eligibility, location_groups,
        component_forecasts) %<-%
          readRDS(result_filename)

      # extract ensemble wis per training set observation
      train_set_wis_one_model <- purrr::map_dfr(
        seq_len(nrow(location_groups)),
        function(i) {
          location_groups$qfm_train[[i]] %>%
            attr("row_index") %>%
            dplyr::mutate(
              train_wis = covidEnsembles::wis(
                y = location_groups$y_train[[i]],
                qfm = predict(
                  location_groups$qra_fit[[i]],
                  qfm = location_groups$qfm_train[[i]]
                )
              )
            )
        }
      ) %>%
        dplyr::mutate(
          join_field = "temp"
        ) %>%
        dplyr::left_join(model_case, by = "join_field") %>%
        dplyr::select(-join_field)
    }

    return(train_set_wis_one_model)
  }
)


