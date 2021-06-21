#' Calculate week end date (i.e., Saturday) for the MMWR epidemic week that is
#' offset a specified number of epidemic week from a specified date
#'
#' @param forecast_date vector of dates
#' @param week_offset integer vector of week offsets.  must be either length 1 or the same
#'     length as timezero
#' @param return_type character specifying return type as "date" or "character"
#'
#' @return vector of dates in 'yyyy-mm-dd' format
date_to_week_end_date <- function(
  forecast_date,
  week_offset = 0,
  return_type = "character") {
  if (!(length(week_offset) %in% c(1, length(forecast_date)))) {
    stop("week_offset must be either length 1 or the same length as forecast_date")
  }

  result <- lubridate::ceiling_date(
    forecast_date + week_offset*7, unit = 'week') - 1

  if (identical(return_type, "character")) {
    result <- as.character(result)
  } else if (!identical(return_type, "date")) {
    stop("Invalid return_type for date_to_week_end_date")
  }

  return(result)
}

#' Calculate end date for the week a forecast was made. Following
#' https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md,
#' "For week-ahead forecasts with forecast_date of Sunday or Monday of EW12, a
#' 1 week ahead forecast corresponds to EW12 and should have target_end_date of
#' the Saturday of EW12."  This means that the forecast week end date is set to
#' Saturday of EW11 (the previous week) for Sunday and Monday of EW12, and
#' Saturday of EW12 (the current week) for Tuesday through Saturday of EW12.
#'
#' @param timezero character vector of dates in 'yyyy-mm-dd' format
#' @param return_type character specifying return type as "date" or "character"
#'
#' @return vector of dates
#'
#' @export
calc_forecast_week_end_date <- function(timezero, target, return_type = "character") {
  # result <- rep(NA_Date_, nrow(timezero))
  # inds <- 
  #   lubridate::wday(lubridate::ymd(timezero), label = TRUE) %in% c("Sun", "Mon")
  # result[inds] <- date_to_week_end_date(
  #   timezero[inds],
  #   week_offset = -1,
  #   return_type = "date")
  # result[!inds] <- date_to_week_end_date(
  #   timezero,
  #   week_offset = 0,
  #   return_type = "date")
  if(is.character(timezero)) {
    timezero <- lubridate::ymd(timezero)
  }

  result <- date_to_week_end_date(
    timezero,
    week_offset = ifelse(
      lubridate::wday(timezero, label = TRUE) %in%
        c("Sun", "Mon"),
      -1,
      0
    ),
    return_type = "date")

  result <- result + ifelse(grepl("day", target), 2, 0)

  if (identical(return_type, "character")) {
    result <- as.character(result)
  }

  return(result)
}


#' Calculate end date for the week a forecast is targeting. Following
#' https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md,
#' "For week-ahead forecasts with forecast_date of Sunday or Monday of EW12, a
#' 1 week ahead forecast corresponds to EW12 and should have target_end_date of
#' the Saturday of EW12."  This means that if horizon is 1, the forecast week
#' end date is set to Saturday of EW12 (the current week) for Sunday and Monday
#' of EW12, and Saturday of EW13 (the next week) for Tuesday through Saturday of
#' EW12.
#'
#' @param timezero character vector of dates in 'yyyy-mm-dd' format
#' @param horizon number of weeks ahead a prediction is targeting
#'
#' @return character vector of dates in 'yyyy-mm-dd' format
#'
#' @export
calc_target_week_end_date <- function(timezero, horizon) {
  result <- ifelse(
    lubridate::wday(lubridate::ymd(timezero), label=TRUE) %in% c('Sun', 'Mon'),
    date_to_week_end_date(timezero, week_offset=horizon-1),
    date_to_week_end_date(timezero, week_offset=horizon)
  )

  return(result)
}

#' Calculate the target end date by parsing a target specification string and
#' adding appropriate integer to `start_date`.
#' 
#' @param start_date a Date object 
#' @param target string specification of forecast target
#' 
#' @return target end date
#' 
#' @export
calc_target_string_end_date <- function(start_date, target) {
  if (!lubridate::is.Date(start_date)) {
    stop("start_date must be a Date object")
  }
  mult <- as.numeric(stringr::word(target, 1))
  if (any(is.na(mult))) {
    stop(paste0("number in row ", which(is.na(mult)), " failed to parse" ))
  }
  scale <- stringr::word(target, 2)
  if (!all(scale %in% c("day", "wk"))) {
    stop(paste0("scale in row ", which(!(scale %in% c("day", "wk"))), " failed to parse" ))
  }
  num_days <- ifelse(scale == "day", 1L, 7L)
  return(start_date + mult*num_days)
}

#' Calculate the effective horizon of a forecast relative to the
#' `forecast_week_end_date`
#' 
#' @param forecast_week_end_date date or vector of dates of the
#' same length as target_end_date, relative to which horizons should be
#' calculated
#' @param target_end_date vector of dates defining dates targeted by forecasts,
#' of same length as target
#' @param target vector of strings specifying targets, e.g.
#' "1 wk ahead inc case", of same length as target_end_date
#' 
#' @return vector of horizons of forecasts relative to the
#' forecast_week_end_date, in units appropriate to the target scale (wk or day)
#' 
#' @export
calc_relative_horizon <- function(
  forecast_week_end_date,
  target_end_date,
  target) {
  days_per_target_time_unit <- ifelse(grepl("day", target), 1, 7)
  return(as.integer((target_end_date - forecast_week_end_date) / days_per_target_time_unit))
}

#' Calculate the effective target relative to the forecast_week_end_date
#' 
#' @param forecast_week_end_date date or vector of dates of the
#' same length as target_end_date, relative to which horizons should be
#' calculated
#' @param target_end_date vector of dates defining dates targeted by forecasts
#' @param target vector of strings specifying targets, e.g.
#' "1 wk ahead inc case"
#' 
#' @return vector of strings defining the target relative to the
#' forecast_week_end_date
#' 
#' @export
calc_relative_target <- function(
  forecast_week_end_date,
  target_end_date,
  target) {
  paste0(
    calc_relative_horizon(forecast_week_end_date, target_end_date, target),
    substr(target, regexpr(" ", target, fixed = TRUE), nchar(target))
  )
}
