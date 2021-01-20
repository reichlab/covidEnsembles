#' get observed cases and/or deaths
#'
#' @param issue_date character issue date (i.e. report date) to use for
#' constructing truths in format 'yyyy-mm-dd'
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death')
#' @param spatial_resolution character vector specifying spatial unit types to
#' include: 'county', 'state' and/or 'national'
#'
#' @return data frame with columns location, base_target, target_end_date, and
#' observed
#'
#' @export
get_observed_by_location_target_end_date <- function(
  issue_date,
  targets,
  spatial_resolution
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
          as_of = issue_date,
          spatial_resolution = effective_spatial_resolution,
          temporal_resolution = temporal_resolution,
          measure = measure
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
  }

  return(observed_by_location_target_end_date)
}
