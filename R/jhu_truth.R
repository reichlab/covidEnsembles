#' Construct table of truths for weekly incident and cumulative deaths at
#' horizons one though 6
#'
#' @param issue_date issue date (i.e. report date) to use for constructing
#' truths
#' @param spatial_resolution character vector specifying spatial unit types to
#' include: 'state' and/or 'national'
#' @param temporal_resolution character vector specifying temporal resolution
#' to include: currently only 'weekly' is supported
#'
#' @return data frame with
#'
#' @export
historical_truths <- function(
  issue_date = NULL,
  spatial_resolution = 'state',
  temporal_resolution = 'weekly'
  ) {
  # validate issue_date
  if(is.null(issue_date)) {
    issue_date <- max(covidEnsembles::jhu_data$issue_date)
  } else {
    issue_date <- as.character(lubridate::ymd(issue_date))
  }
  if(!(issue_date %in% covidEnsembles::jhu_data$issue_date)) {
    stop(paste0('Invalid issue date; must be one of: ',
                paste0(covidEnsembles::jhu_data$issue_date, collapse = ', ')))
  }

  # validate spatial_resolution
  spatial_resolution <- match.arg(
    spatial_resolution,
    choices = c('state', 'national'),
    several.ok = TRUE)
  if(!all(spatial_resolution %in% c('state', 'national'))) {
    stop('Only spatial_resolution \'state\' and \'national\' are supported.')
  }

  # validate temporal_resolution
  temporal_resolution <- match.arg(
    temporal_resolution,
    choices = 'weekly',
    several.ok = FALSE
  )

  # get report for specified issue date
  data <- covidEnsembles::jhu_data %>%
    dplyr::filter(issue_date == UQ(issue_date)) %>%
    dplyr::pull(data) %>%
    `[[`(1) %>%
    tidyr::pivot_longer(
      matches('^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}$'),
      names_to = 'date',
      values_to = 'cum_deaths') %>%
    dplyr::mutate(
      date = as.character(lubridate::mdy(date))
    )

  # if weekly temporal resolution, filter to saturdays
  if(temporal_resolution == 'weekly') {
    data <- data %>%
      dplyr::filter(
        lubridate::wday(lubridate::ymd(date), label = TRUE) == 'Sat'
      )
  }

  # summarized results for state level
  results <- NULL
  if('state' %in% spatial_resolution) {
    states_to_keep <- c(
      'Alabama', 'Alaska', 'American Samoa', 'Arizona', 'Arkansas', 'California',
      'Colorado', 'Connecticut', 'Delaware', 'District of Columbia',
      'Florida', 'Georgia', 'Guam', 'Hawaii', 'Idaho', 'Illinois',
      'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine',
      'Maryland', 'Massachusetts', 'Michigan', 'Minnesota',
      'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada',
      'New Hampshire', 'New Jersey', 'New Mexico', 'New York',
      'North Carolina', 'North Dakota', 'Northern Mariana Islands',
      'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Puerto Rico',
      'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee',
      'Texas', 'Utah', 'Vermont', 'Virgin Islands', 'Virginia',
      'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')

    results <- data %>%
      dplyr::filter(Province_State %in% states_to_keep) %>%
      dplyr::mutate(location_name = Province_State) %>%
      dplyr::group_by(location_name, date) %>%
      dplyr::summarize(cum_deaths = sum(cum_deaths)) %>%
      dplyr::mutate(inc_deaths = cum_deaths - dplyr::lag(cum_deaths, 1L)) %>%
      dplyr::left_join(covidEnsembles::fips_codes, by = 'location_name') %>%
      dplyr::select(location, location_name, location_abbreviation, date, cum_deaths, inc_deaths) %>%
      dplyr::ungroup()
  }

  if('national' %in% spatial_resolution) {
    # because we don't filter on states_to_keep as above, we are off by a total
    # of 3 deaths attributed to Diamond Princess.
    us_results <- data %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(cum_deaths = sum(cum_deaths)) %>%
      dplyr::mutate(
        inc_deaths = cum_deaths - dplyr::lag(cum_deaths, 1L),
        location = 'US',
        location_name = 'United States',
        location_abbreviation = 'US'
      ) %>%
      dplyr::select(location, location_name, location_abbreviation, date, cum_deaths, inc_deaths) %>%
      dplyr::ungroup()

    results <- dplyr::bind_rows(results, us_results)
  }

  return(results)
}
