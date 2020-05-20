#' Download all forecasts for a project and assemble into a data frame
#'
#' @param zoltar_connection A 'ZoltarConnection' object as returned by new_connection
#' @param project_url URL of a project in zoltar_connection's projects
#' @param ... conditions for filtering the results; will be passed to dplyr::filter
#'
#' @return data frame with forecasts meeting the specified filter conditions
#'
#' @export
get_all_project_forecasts <- function(zoltar_connection, project_url, ...) {
  all_models <- zoltr::models(zoltar_connection, project_url)
  all_models$abbreviation <- purrr::map_chr(
    all_models$url,
    function(url) {
      zoltr::model_info(zoltar_connection, url)$abbreviation
    }
  )
  all_forecast_refs <- purrr::pmap_dfr(
    all_models %>% dplyr::select(id, url, name, abbreviation),
    function(id, url, name, abbreviation) {
      zoltr::forecasts(zoltar_connection, url) %>%
        dplyr::mutate(
          model_id = UQ(id),
          model_name = UQ(name),
          model_abbreviation = UQ(abbreviation))
    }
  )
  all_forecasts <- purrr::pmap_dfr(
    all_forecast_refs %>% dplyr::select(url, model_id, model_name, model_abbreviation),
    function(url, model_id, model_name, model_abbreviation, ...) {
      get_one_project_forecast(zoltar_connection, url, ...) %>%
        dplyr::mutate(
          model_id = model_id,
          model_name = model_name,
          model_abbreviation = model_abbreviation)
    },
    ... = ...
  )

  return(all_forecasts)
}

#' Download a single forecast for a project and assemble into a data frame
#'
#' @param zoltar_connection A 'ZoltarConnection' object as returned by new_connection
#' @param project_url URL of a project in zoltar_connection's projects
#' @param ... conditions for filtering the results; will be passed to dplyr::filter
#'
#' @return data frame with forecasts meeting the specified filter conditions
get_one_project_forecast <- function(zoltar_connection, forecast_url, ...) {
  forecast_info <- zoltr::forecast_info(zoltar_connection, forecast_url)
  forecast_data <- zoltr::download_forecast(zoltar_connection, forecast_url)
  forecast_data_frame <- zoltr::data_frame_from_forecast_data(forecast_data) %>%
    dplyr::filter(...)
  if(nrow(forecast_data_frame) > 0) {
    forecast_data_frame$timezero <- forecast_info$time_zero$timezero_date
  }

  return(forecast_data_frame)
}
