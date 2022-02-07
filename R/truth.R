#' get observed cases and/or deaths
#'
#' @param as_of character date with format 'yyyy-mm-dd';
#' indicates date for which retrieved truth data should be current
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death')
#' @param spatial_resolution character vector specifying spatial unit types to
#' include: 'county', 'state', 'national', and/or 'euro_countries'
#' @param locations character vector of location codes to be passed to \code{covidData::load_forecasts}. 
#' Default to NULL.
#' For US locations, this should be a list of FIPS code or 'US'
#' For ECDC locations, this should be a list of location name abbreviation.
#'
#' @return data frame with columns location, base_target, target_end_date, and
#' observed
#'
#' @export
get_observed_by_location_target_end_date <- function(
  as_of,
  targets,
  spatial_resolution,
  locations = NULL
) {
  types_and_measures <- purrr::map_dfr(
    targets,
    function(target) {
      split_res <- strsplit(target, ' ', fixed = TRUE)[[1]]
      data.frame(
        type = split_res[4],
        measure = split_res[5],
        stringsAsFactors = FALSE
      )
    }
  ) %>%
    dplyr::distinct(type, measure)

  if (identical(spatial_resolution, "state_no_territories")) {
    effective_spatial_resolution <- "state"
  } else if (identical(spatial_resolution, "euro_countries")) {
    effective_spatial_resolution <- "national"
  } else {
    effective_spatial_resolution <- spatial_resolution
  }

  observed_by_location_target_end_date <-
    purrr::map_dfr(
      unique(types_and_measures$measure),
      function(measure) {
        types <- types_and_measures %>%
          dplyr::filter(measure == UQ(measure)) %>%
          dplyr::pull(type)

        if (measure %in% c("case", "death")) {
          temporal_resolution <- "weekly"
        } else if (measure == "hosp") {
          temporal_resolution <- "daily"
        }

        covidData::load_data(
          as_of = as_of,
          location_code = locations,
          spatial_resolution = effective_spatial_resolution,
          temporal_resolution = temporal_resolution,
          measure = measure,
          geography = ifelse(
            spatial_resolution == "euro_countries",
            "global",
            "US")
        ) %>%
          tidyr::pivot_longer(
            cols = c('cum', 'inc'),
            names_to = 'type',
            values_to = 'observed') %>%
          dplyr::filter(type %in% types) %>%
          dplyr::transmute(
            location = location,
            base_target = paste(
              ifelse(temporal_resolution == "weekly", "wk", "day"),
              "ahead", type, measure),
            target_end_date = as.character(date),
            observed = observed
          )
      }
    )

  if (identical(spatial_resolution, "state_no_territories")) {
    observed_by_location_target_end_date <-
      observed_by_location_target_end_date %>%
        dplyr::filter(location <= "56")
  } else if (identical(spatial_resolution, "euro_countries")) {
    euro_hub_locations <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "GR",
      "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT",
      "PL", "PT", "RO", "SI", "SK", "FI", "SE", "GB", "IS", "LI", "NO", "CH")
    observed_by_location_target_end_date <-
      observed_by_location_target_end_date %>%
        dplyr::filter(location %in% euro_hub_locations)
  }

  return(observed_by_location_target_end_date)
}
