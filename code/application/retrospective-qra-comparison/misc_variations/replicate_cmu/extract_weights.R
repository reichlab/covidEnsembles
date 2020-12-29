library(tidyverse)
library(covidEnsembles)

# function to extract model identifiers from abbreviation
parse_model_case <- function(model_abbr) {
  case_parts <- strsplit(model_abbr, split = "-")[[1]]

  purrr::map_dfc(
    case_parts,
    function(case_part) {
      nc <- nchar(case_part)
      if (case_part %in%
          c("inc_death", "cum_death", "inc_case", "inc_hosp")) {
        return(data.frame(
          target_variable = case_part
        ))
      } else if(substr(case_part, 1, min(nc, 13)) == "forecast_week") {
        file_date <- lubridate::ymd(substr(case_part, 15, nc))
        if (lubridate::wday(file_date, label = TRUE) == "Sat") {
          forecast_date <- file_date + 2
        } else if(lubridate::wday(file_date, label = TRUE) == "Mon") {
          forecast_date <- file_date
        }
        return(data.frame(
          forecast_date = forecast_date
        ))
      } else if(substr(case_part, 1, min(nc, 9)) == "intercept") {
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
        message(paste0("Unsupported case part: ", case_part))
      }
    }
  )
}

included_base_targets <- c("inc_death", "inc_case")

weight_estimates <- purrr::map_dfr(
  c("state", "national"),
  function(spatial_resolution) {
    fits_path <- paste0(
      "code/application/retrospective-qra-comparison/retrospective-fits-",
      spatial_resolution)
    fit_files <- Sys.glob(paste0(fits_path, "/", included_base_targets, "*.rds"))

    junk <- fit_files[
      grepl("window_size_8", fit_files) &
      grepl("per_model", fit_files) &
      grepl("inc_case", fit_files)# &
#      (grepl("2020-08-0", fit_files) | grepl("2020-07", fit_files))
    ]

    sr_res <- purrr::map_dfr(
      fit_files,
      function(ff) {
        cat(ff)
        cat("\n")
        model_abbr <- strsplit(ff, "/") %>%
          `[[`(1) %>%
          tail(1) %>%
          substr(1, nchar(.) - 4)

        model_case <- parse_model_case(
          gsub("(\\d\\d\\d\\d)\\-(\\d\\d)\\-(\\d\\d)", "\\1\\2\\3", model_abbr)
        )

        if (
          model_case$combine_method != "convex" |
          model_case$quantile_groups != "per_model" |
          !(model_case$window_size %in% c(4, 7, 8, 9))
          ) {
          return(NULL)
        }

        model_fit <- readRDS(ff)
        purrr::pmap_dfr(
          model_fit$location_groups %>% #$qra_fit[, c("locations", "qra_fit")],
            dplyr::select(locations, qra_fit),
          function(locations, qra_fit) {
            weights <- qra_fit$coefficients

            data.frame(
              quantile = if("quantile" %in% colnames(weights)) {
                weights$quantile
              } else {
                rep(NA, nrow(weights))
              },
              model = weights$model,
              weight = weights$beta,#[, 1],
              join_field = "temp",
              stringsAsFactors = FALSE
            ) %>%
              dplyr::left_join(
                data.frame(
                  location = locations,
                  join_field = "temp",
                  stringsAsFactors = FALSE
                )
              )
          }
        ) %>%
          dplyr::mutate(join_field = "temp") %>%
          dplyr::left_join(
            model_case %>% mutate(join_field = "temp"),
            by = "join_field") %>%
          dplyr::select(-join_field)
      }
    ) %>%
      dplyr::mutate(spatial_resolution = spatial_resolution)

    return(sr_res)
  }
)

saveRDS(
  distinct(weight_estimates),
  "code/application/retrospective-qra-comparison/analyses/retrospective-weight-estimates/retrospective_weight_estimates.rds")
