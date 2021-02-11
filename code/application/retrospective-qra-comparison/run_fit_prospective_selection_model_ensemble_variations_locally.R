library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(zeallot)
library(covidEnsembles)
library(doParallel)

# number of cores to use for local runs
registerDoParallel(cores = 18L)

args <- commandArgs(trailingOnly = TRUE)

output_path <- "code/application/retrospective-qra-comparison/log/"

# define analysis combinations to run
forecast_date <- lubridate::ymd("2021-02-08")

#last_forecast_date <- lubridate::floor_date(Sys.Date(), unit = "week") + 1
num_forecast_weeks <-
  as.numeric(last_forecast_date - first_forecast_date) / 7 + 1

analysis_combinations <- tidyr::expand_grid(
  spatial_resolution = c("county", "state", "national"),
  response_var = c("inc_case", "inc_death", "cum_death", "inc_hosp"),
  forecast_date = as.character(forecast_date),
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

models_to_run <- foreach(
  row_ind = seq_len(nrow(analysis_combinations)),
  .combine = dplyr::bind_rows) %dopar% {
#run_status <- foreach(row_ind = seq_len(2)) %dopar% {
  response_var <- analysis_combinations$response_var[row_ind]
  forecast_date <- lubridate::ymd(analysis_combinations$forecast_date[row_ind])
  spatial_resolution <- analysis_combinations$spatial_resolution[row_ind]
  include_full_history <- as.logical(analysis_combinations$include_full_history[row_ind])

  if (spatial_resolution %in% c("state", "national")) {
    spatial_scales <- c(spatial_resolution, "state_national")
  } else {
    spatial_scales <- spatial_resolution
  }

  # Dates of forecast submission for forecasts included in this analysis
  if (response_var %in% c("cum_death", "inc_death")) {
    first_forecast_date <- lubridate::ymd("2020-06-22")
  } else if (response_var == "inc_case") {
    first_forecast_date <- lubridate::ymd("2020-09-14")
  } else if (response_var == "inc_hosp") {
    first_forecast_date <- lubridate::ymd("2020-11-23")
  }
  num_forecast_weeks <- as.integer(forecast_date - 7 - first_forecast_date)/7 + 1

  forecast_dates <- first_forecast_date +
    seq(from = 0, length = num_forecast_weeks) * 7

  all_scores <- calc_retrospective_ensemble_scores(
    submissions_root = "code/application/retrospective-qra-comparison/retrospective-forecasts/",
    forecast_dates = forecast_dates,
    spatial_scales = spatial_scales,
    response_vars = response_var,
    truth_as_of = forecast_date - 1
  ) %>%
    dplyr::filter(
      !grepl("ensemble_switching", model),
      !grepl("combine_method_positive", model)#,
  #    !grepl("check_missingness_by_target_TRUE", model)
    ) %>%
    dplyr::mutate(
      spatial_scale = ifelse(
        location == "US",
        "National",
        ifelse(
          nchar(location) == 2,
          "State",
          "County"
        )
      )
    )

  all_model_cases <- purrr::map_dfr(
    unique(all_scores$model),
    function(x) {
      parse_model_case(x) %>% dplyr::mutate(model = x)
    }
  )

  all_scores <- all_scores %>%
    dplyr::left_join(all_model_cases, by = "model") %>%
    dplyr::mutate(
      target = paste(horizon, temporal_resolution, "ahead", target_variable),
      model_brief = paste(
        combine_method,
        "window",
        window_size,
        quantile_groups,
        estimation_grouping,
        sep = "_"
      )
    )

  if (!include_full_history) {
    all_scores <- all_scores %>%
      dplyr::filter(window_size != "full_history")
  }


  # subset scores to those that are comparable for all models within each
  # combination of spatial scale and base target
  # only among those models with any forecasts for that combination
  all_scores_common_by_target_variable_spatial_scale <-
    purrr::pmap_dfr(
      all_scores %>%
        distinct(target_variable, spatial_scale),
      function(target_variable, spatial_scale) {
        reduced_scores <- all_scores %>%
          dplyr::filter(
            target_variable == UQ(target_variable),
            spatial_scale == UQ(spatial_scale)
          )

        # subset to same forecasts made for each ensemble method
        scores_to_keep <- reduced_scores %>%
          dplyr::select(model, forecast_date, location, target, abs_error) %>%
          tidyr::pivot_wider(
            names_from = "model", values_from = "abs_error"
          )
        all_models <- unique(reduced_scores$model)
        scores_to_keep$keep <-
          apply(scores_to_keep[all_models], 1, function(x) all(!is.na(x)))

        # message(paste0(
        #   "at ", spatial_scale, " for ", target_variable,
        #   ", missing forecasts for models: ",
        #   paste0(
        #     all_models[apply(scores_to_keep[all_models], 2, function(x) any(is.na(x)))]
        #   )
        # ))

        scores_to_keep <- scores_to_keep %>%
          dplyr::select(forecast_date, location, target, keep)

        dplyr::left_join(
          reduced_scores,
          scores_to_keep,
          by = c("forecast_date", "location", "target")
        ) %>%
          dplyr::filter(keep) %>%
          dplyr::select(-keep)
      }
    )

  # score summaries
  scores_overall <- all_scores_common_by_target_variable_spatial_scale %>%
    dplyr::mutate(
      target_variable = substr(target, regexpr(" ", target) + 1, nchar(target)),
      spatial_scale = ifelse(
        location == "US",
        "National",
        ifelse(
          nchar(location) == 2,
          "State",
          "County"
        )
      )
    ) %>%
    dplyr::group_by(
      model, model_brief, intercept, combine_method, missingness, quantile_groups,
      window_size, check_missingness_by_target, do_standard_checks,
      do_baseline_check, estimation_grouping, target_variable, spatial_scale) %>%
    dplyr::summarize(
      across(starts_with("abs_error"), function(x) round(mean(x), 3)),
      across(starts_with("wis"), function(x) round(mean(x), 3)),
      across(starts_with("coverage"), function(x) round(mean(x), 3))#,
    )

  # Extract model with lowest WIS for each combination of
  # spatial scale and target variable
  best_models <- scores_overall %>%
    dplyr::filter(!(spatial_scale == "County" & window_size == "4")) %>%
    dplyr::group_by(target_variable, spatial_scale) %>%
    dplyr::slice_min(wis, n = 2) %>%
    dplyr::filter(tolower(spatial_scale) == tolower(spatial_resolution)) %>%
    dplyr::mutate(
      response_var = response_var,
      forecast_date = forecast_date,
      spatial_resolution = spatial_resolution,
      include_full_history = include_full_history
    )

  best_models
}

analysis_combinations <- models_to_run
run_setting <- "local"
run_status <- foreach(row_ind = seq_len(rev(nrow(analysis_combinations)))) %dopar% {
# foreach(row_ind = seq_len(2)) %dopar% {
  response_var <- analysis_combinations$response_var[row_ind]
  forecast_date <- analysis_combinations$forecast_date[row_ind]
  intercept <- analysis_combinations$intercept[row_ind]
  combine_method <- analysis_combinations$combine_method[row_ind]
  quantile_group_str <- analysis_combinations$quantile_groups[row_ind]
  missingness <- analysis_combinations$missingness[row_ind]
  window_size <- analysis_combinations$window_size[row_ind]
  check_missingness_by_target <-
    analysis_combinations$check_missingness_by_target[row_ind]
  do_standard_checks <- analysis_combinations$do_standard_checks[row_ind]
  do_baseline_check <- analysis_combinations$do_baseline_check[row_ind]
  spatial_resolution <- analysis_combinations$estimation_grouping[row_ind]

  run_cmd <- paste0(
    "R CMD BATCH --vanilla \'--args ",
    run_setting, " ",
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

models_to_run <- models_to_run %>%
  dplyr::group_by(spatial_scale, target_variable) %>%
  dplyr::slice_min(wis, n = 1)

models_to_run %>%
  dplyr::select(-model, -model_brief, -intercept, -do_standard_checks, -do_baseline_check)


