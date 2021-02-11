#' Load retrospective ensemble forecasts that were made on or before 
#' 
#' @param submissions_root path to a folder containing submission files from
#' retrospective ensemble fits.  It should have subdirectories for each spatial
#' unit grouping used for estimation, i.e., "national", "state",
#' "state_national", and/or "county".  Within those folders, we expect a
#' sub-folder for each ensemble specification
#' @param response_vars character vector of response variables for which to
#' retrieve forecasts.  If missing or NULL, retrieves all appropriate for each
#' spatial scale
#' 
#' @return data frame of forecasts in format suitable for use with covidHubUtils
#' functions
#' 
#' @export
load_retrospective_ensemble_forecasts <- function(
  submissions_root,
  forecast_dates,
  all_locations,
  spatial_scales = c("national", "state", "state_national", "county"),
  response_vars = NULL
) {
  all_forecasts <- purrr::map_dfr(
    spatial_scales,
    function(spatial_scale) {
      # Path to forecasts to evaluate
      submissions_root <- paste0(submissions_root, spatial_scale, "/")

      # models to read in
      model_abbrs <- list.dirs(submissions_root, full.names = FALSE)
      model_abbrs <- model_abbrs[nchar(model_abbrs) > 0]

      if (is.null(response_vars)) {
        if (spatial_scale %in% c("national", "state_national")) {
          response_vars <- c("cum_death", "inc_death", "inc_case", "inc_hosp")
        } else if (spatial_scale == "state") {
          response_vars <- c("cum_death", "inc_death", "inc_case", "inc_hosp")
        } else if (spatial_scale == "county") {
          response_vars <- "inc_case"
        }
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
#            all_locations <- unique(observed_deaths$location)
          } else if (response_var == "inc_case") {
            required_quantiles <-
              c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
            temporal_resolution <- "wk"
            horizon <- 4L
            targets <- paste0(
              1:horizon, " wk ahead ", gsub("_", " ", response_var))
#            all_locations <- unique(observed_cases$location)
          } else if (response_var == "inc_hosp") {
            required_quantiles <-
              c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
            temporal_resolution <- "day"
            horizon <- 28L
            targets <- paste0(
              1:(horizon + 6), " day ahead ", gsub("_", " ", response_var))
#            all_locations <- unique(observed_hosps$location)
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
    dplyr::transmute(
      model = paste0(model, "-estimation_scale_", spatial_scale),
      forecast_date = timezero,
      location = location,
      location_name = location_name,
      geo_type = "state",
      horizon = as.integer(horizon),
      temporal_resolution = ifelse(grepl("wk", target), "wk", "day"),
      target_variable = substr(target, regexpr("ahead", target) + 6, nchar(target)),
      target_end_date = target_end_date,
      type = "quantile",
      quantile = as.numeric(quantile),
      value = value
    )
  
  all_forecasts <- dplyr::bind_rows(
    all_forecasts,
    all_forecasts %>%
      dplyr::filter(quantile == 0.5) %>%
      dplyr::mutate(
        type = "point",
        quantile = NA
      )
  )

  return(all_forecasts)
}


#' Calculate scores of retrospective ensemble forecasts
#' 
#' @param forecast_files character vector of paths to retrospective ensemble
#' forecast files
#' @param truth_as_of Date or string in format "yyyy-mm-dd" specifying
#' as_of date to use for truth
#' 
#' @return data frame of scores
#' 
#' @export 
calc_retrospective_ensemble_scores <- function(
  submissions_root,
  forecast_dates,
  all_locations,
  spatial_scales = c("national", "state", "state_national", "county"),
  response_vars = NULL,
  truth_as_of = NULL
) {
  # all targets for forecasts that will be scored
  all_targets <- c(
    paste0(1:4, " wk ahead cum death"),
    paste0(1:4, " wk ahead inc death"),
    paste0(1:4, " wk ahead inc case"),
    paste0(1:28, " day ahead inc hosp")
  )

  if (!is.null(response_vars)) {
    targets_keep <- plyr::laply(
      gsub("_", " ", response_vars),
      function(response_var) {
        grepl(response_var, all_targets)
      },
      .drop = FALSE) %>%
      apply(2, max)

    all_targets <- all_targets[targets_keep > 0]
  }

  # load observed data
  observed <- get_observed_by_location_target_end_date(
    as_of = truth_as_of,
    targets = all_targets,
    spatial_resolution = purrr::map(
      spatial_scales,
      function(ss) {
        strsplit(ss, "_")
      }) %>%
      unlist() %>%
      unique()
  ) %>%
    dplyr::left_join(
      covidData::fips_codes, by = "location"
    ) %>%
    dplyr::transmute(
      model = "Observed Data (JHU)",
      target_variable = substr(
        base_target,
        pmax(regexpr("inc", base_target), regexpr("cum", base_target)),
        nchar(base_target)
      ),
      target_end_date = lubridate::ymd(target_end_date),
      location = location,
      value = observed,
      geo_type = "state",
      location_name = location_name,
      abbreviation = abbreviation
    )
  
  # load forecasts
  all_forecasts <- load_retrospective_ensemble_forecasts(
    submissions_root = submissions_root,
    forecast_dates = forecast_dates,
    all_locations = unique(observed$location),
    spatial_scales = spatial_scales,
    response_vars = response_vars
  )

  all_scores <- purrr::map_dfr(
    unique(all_forecasts$target_variable),
    function(tv) {
      covidHubUtils::score_forecasts(
        forecasts = all_forecasts %>%
          dplyr::filter(target_variable == tv),
        return_format = "wide",
        truth = observed
      )
    }
  )

  return(all_scores)
}

#' function to extract model identifiers from abbreviation
#' 
#' @param model_abbr abbreviation for a retrospective ensemble fit
#' 
#' @return data frame of model fit characteristics
#' 
#' @export
parse_model_case <- function(model_abbr) {
  case_parts <- strsplit(model_abbr, split = "-")[[1]]
  purrr::map_dfc(
    case_parts,
    function(case_part) {
      nc <- nchar(case_part)
      if (substr(case_part, 1, min(nc, 9)) == "intercept") {
        return(data.frame(
          intercept = as.logical(substr(case_part, 11, nc))
        ))
      } else if (substr(case_part, 1, min(nc, 14)) == "combine_method") {
        return(data.frame(
          combine_method = substr(case_part, 16, nc)
        ))
      } else if (substr(case_part, 1, min(nc, 11)) == "missingness") {
        return(data.frame(
          missingness = substr(case_part, 13, nc)
        ))
      } else if (substr(case_part, 1, min(nc, 15)) == "quantile_groups") {
        return(data.frame(
          quantile_groups = substr(case_part, 17, nc)
        ))
      } else if (substr(case_part, 1, min(nc, 11)) == "window_size") {
        return(data.frame(
          window_size = substr(case_part, 13, nc)
        ))
      } else if (substr(case_part, 1, min(nc, 27)) ==
          "check_missingness_by_target") {
        return(data.frame(
          check_missingness_by_target = substr(case_part, 29, nc)
        ))
      } else if (substr(case_part, 1, min(nc, 18)) == "do_standard_checks") {
        return(data.frame(
          do_standard_checks = substr(case_part, 20, nc)
        ))
      } else if (substr(case_part, 1, min(nc, 17)) == "do_baseline_check") {
        return(data.frame(
          do_baseline_check = substr(case_part, 19, nc)
        ))
      } else if (substr(case_part, 1, min(nc, 16)) == "estimation_scale") {
        return(data.frame(
          estimation_grouping = substr(case_part, 18, nc)
        ))
      } else {
        message(paste0("Unsupported case part: ", case_part))
      }
    }
  )
}
