library(tidyverse)
library(zeallot)
library(covidHubUtils)
library(covidEnsembles)
library(covidData)
library(googledrive)
library(yaml)
library(here)
options(error = recover)
setwd(here())

final_run <- TRUE

# Where to find component model submissions
submissions_root <- '../covid19-forecast-hub/data-processed/'

# Where to save ensemble forecasts
save_roots <- c('code/application/weekly-ensemble/forecasts/')
for (root in save_roots) {
  if (!file.exists(root)) dir.create(root, recursive = TRUE)
  if (!file.exists(paste0(root,"trained-ensemble-metadata/"))) {
    dir.create(paste0(root,"trained-ensemble-metadata/"), recursive = TRUE)
  }
}

# Where to save plots
plots_root <- 'code/application/weekly-ensemble/plots/COVIDhub-trained_ensemble/'
if (!file.exists(plots_root)) dir.create(plots_root, recursive = TRUE)

# Figure out what day it is; forecast creation date is set to a Monday,
# even if we are delayed and create it Tuesday morning.
forecast_date <- lubridate::floor_date(Sys.Date(), unit = "week") + 1

all_forecasts <- purrr::map_dfr(
  c("cum_death", "inc_death", "inc_case", "inc_hosp"),
  function(response_var) {
    if (response_var == "cum_death") {
      spatial_resolutions <- c("state", "national")
    } else if (response_var == 'inc_death') {
      spatial_resolutions <- c("state", "national")
    } else if (response_var == "inc_case") {
      spatial_resolutions <- c("county", "state", "national")
    } else if (response_var == "inc_hosp") {
      spatial_resolutions <- c("state", "national")
    }

    target_forecasts <- purrr::map_dfr(
      spatial_resolutions,
      function(spatial_resolution) {
        forecasts_dir <- file.path(
          "code/application/retrospective-qra-comparison/retrospective-forecasts",
          spatial_resolution,
          paste0("prospective_selection-include_full_history_FALSE")
        )

        forecast_filename <- paste0(
          forecasts_dir, "/",
          response_var, "-", forecast_date, "-",
          "prospective_selection-include_full_history_FALSE.csv"
        )

        readr::read_csv(
          forecast_filename,
          col_types = readr::cols(
            forecast_date = readr::col_date(format = ""),
            target = readr::col_character(),
            target_end_date = readr::col_date(format = ""),
            location = readr::col_character(),
            type = readr::col_character(),
            quantile = readr::col_double(),
            value = readr::col_double())
        )
      }
    )

    return(target_forecasts)
  })

save_dir <- paste0(root, 'data-processed/COVIDhub-trained_ensemble/')
if (!file.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
write_csv(
  all_forecasts,
  paste0(save_dir,
          all_forecasts$forecast_date[1],
          '-COVIDhub-trained_ensemble.csv')
)

# make plots of ensemble submission
root <- save_roots
plot_forecasts_single_model(
  submissions_root = paste0(root, "data-processed/"),
  plots_root = plots_root,
  forecast_date = forecast_date,
  model_abbrs = "COVIDhub-trained_ensemble",
  target_variables = c("cases", "deaths", "hospitalizations")
)


#       save_dir <- paste0(root, "ensemble-metadata/")
#       if (!file.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
#       write_csv(model_eligibility,
#         paste0(save_dir,
#           formatted_ensemble_predictions$forecast_date[1],
#           '-',
#           response_var,
#           '-model-eligibility.csv'))

#       write_csv(model_weights,
#         paste0(save_dir,
#           formatted_ensemble_predictions$forecast_date[1],
#           '-',
#           response_var,
#           '-model-weights.csv'))
#     }
#   }
# }
