#' plot forecasts
#'
#' @param submissions_root path to folder with submission files, similar to
#' data-processed folder in covid19-forecast-hub repo
#' @param plots_root folder where plots will be saved
#' @param forecast_date date for Monday of model submissions to plot
#' @param model_abbrs model names to plot
#' @param target_variables character vector of variables like "cases", "deaths",
#' or "hospitalizations"
#' 
#' @export
plot_forecasts_single_model <- function(
  submissions_root,
  plots_root,
  forecast_date,
  model_abbrs,
  target_variables
) {
  day_plots_root <- paste0(plots_root, forecast_date, '/')
  if(!file.exists(day_plots_root)) {
    dir.create(day_plots_root)
  }

  submission_dates <- forecast_date + seq(from = -6, to = 0)

  for (model_abbr in model_abbrs) {
    # Find a submission file for this model abbreviation
    results_path <- Sys.glob(paste0(submissions_root, model_abbr, "/*",
      submission_dates, "-", model_abbr, ".csv"))

    if (length(results_path) == 0) {
      # no forecasts for this week
      next
    }

    results <- purrr::map_dfr(
      results_path,
      readr::read_csv,
      col_types = cols(
        forecast_date = col_date(format = ""),
        target = col_character(),
        target_end_date = col_date(format = ""),
        location = col_character(),
        type = col_character(),
        quantile = col_double(),
        value = col_double()
      )) %>%
      dplyr::left_join(covidData::fips_codes, by = "location")

    for (measure in target_variables) {
      plot_path <- paste0(day_plots_root, model_abbr, '-', forecast_date, '-', measure, '.pdf')
      if (!file.exists(plot_path)) {
        if(measure == "deaths") {
          data <- dplyr::bind_rows(
            covidData::load_jhu_data(
              as_of = as.character(forecast_date - 1),
              spatial_resolution = c("state", "national"),
              temporal_resolution = "weekly",
              measure = measure) %>%
              dplyr::left_join(covidData::fips_codes, by = "location") %>%
              dplyr::mutate(issue_date = as.character(forecast_date - 1)),
            covidData::load_jhu_data(
              as_of = as.character(covidData::available_issue_dates("deaths") %>% max()),
              spatial_resolution = c("state", "national"),
              temporal_resolution = "weekly",
              measure = measure) %>%
              dplyr::left_join(covidData::fips_codes, by = "location") %>%
              dplyr::mutate(
                issue_date = covidData::available_issue_dates("deaths") %>%
                  max() %>%
                  as.character()
              )
          )

          # maximum horizon to plot
          horizon <- 4L
          types <- c("inc", "cum")
          required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
          target_measure <- "death"
          days_per_target_unit <- 7L
          forecast_week_end_date <- forecast_date - 2L
        } else if (measure == "cases") {
          data <- dplyr::bind_rows(
            covidData::load_jhu_data(
              as_of = as.character(forecast_date - 1),
              spatial_resolution = c("county", "state", "national"),
              temporal_resolution = "weekly",
              measure = measure) %>%
              dplyr::left_join(covidData::fips_codes, by = "location") %>%
              dplyr::mutate(issue_date = as.character(forecast_date - 1)),
            covidData::load_jhu_data(
              as_of = as.character(covidData::available_issue_dates("cases") %>% max()),
              spatial_resolution = c("county", "state", "national"),
              temporal_resolution = "weekly",
              measure = measure) %>%
              dplyr::left_join(covidData::fips_codes, by = "location") %>%
              dplyr::mutate(
                issue_date = covidData::available_issue_dates("cases") %>%
                  max() %>%
                  as.character()
              )
          )

          # maximum horizon to plot
          horizon <- 4L
          types <- "inc"
          required_quantiles <-
            c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
          target_measure <- "case"
          days_per_target_unit <- 7L
          forecast_week_end_date <- forecast_date - 2L
        } else if (measure == "hospitalizations") {
          data <- dplyr::bind_rows(
            covidData::load_healthdata_data(
              as_of = as.character(forecast_date - 1),
              spatial_resolution = c("county", "state", "national"),
              temporal_resolution = "daily",
              measure = measure) %>%
              dplyr::left_join(covidData::fips_codes, by = "location") %>%
              dplyr::mutate(issue_date = as.character(forecast_date - 1)),
            covidData::load_healthdata_data(
              as_of = as.character(covidData::available_issue_dates("hospitalizations") %>% max()),
              spatial_resolution = c("county", "state", "national"),
              temporal_resolution = "daily",
              measure = measure) %>%
              dplyr::left_join(covidData::fips_codes, by = "location") %>%
              mutate(
                issue_date = covidData::available_issue_dates("hospitalizations") %>%
                  max() %>%
                  as.character()
              )
          )

          # maximum horizon to plot
          horizon <- 28L
          types <- "inc"
          required_quantiles <-
            c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
          target_measure <- "hosp"
          days_per_target_unit <- 1L
          forecast_week_end_date <- forecast_date
        }

        data <- data %>%
          dplyr::filter(date >= forecast_week_end_date - 6 * horizon * days_per_target_unit)

        results <- results %>%
          dplyr::filter(target_end_date <= UQ(forecast_week_end_date) + days_per_target_unit * horizon)

        location_batches <- results %>%
          dplyr::filter(grepl(target_measure, target)) %>%
          dplyr::distinct(location, location_name_with_state) %>%
          dplyr::arrange(nchar(location), 
            sub(pattern = ".*,\\s", "", location_name_with_state), 
            location_name_with_state) %>%
          dplyr::mutate(
            location_name_with_state = factor(
              location_name_with_state,
              levels = location_name_with_state),
            batch = rep(seq_len(ceiling(nrow(.)/30)), each = 30)[seq_len(nrow(.))]
          )

        made_plots <- FALSE
        pdf(plot_path, width=24, height=14)

        for(batch_val in unique(location_batches$batch)) {
          print(batch_val)
          batch_locations <- location_batches$location_name_with_state[
            location_batches$batch == batch_val]
          plottable_predictions <- results %>%
            dplyr::filter(
              location_name_with_state %in% batch_locations,
              grepl(target_measure, target)) %>%
            dplyr::mutate(
              endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
              alpha = ifelse(
                endpoint_type == 'lower',
                format(2*quantile, digits=3, nsmall=3),
                format(2*(1-quantile), digits=3, nsmall=3))
            ) %>%
            dplyr::filter(alpha != "1.000") %>%
            dplyr::select(-quantile) %>%
            tidyr::pivot_wider(names_from='endpoint_type', values_from='value')

          for(type in types) {
            type_intervals <- plottable_predictions %>%
              dplyr::filter(location_name_with_state %in% batch_locations) %>%
              filter(alpha != "1.000", grepl(UQ(type), target))
            forecast_date_intervals <- type_intervals %>%
              tidyr::separate(target,
                into = c("horizon", "temporal_resolution", "ahead", "type", "target_variable"),
                remove = FALSE, extra = "merge"
              ) %>%
              covidHubUtils::align_forecasts() %>%
              dplyr::distinct(forecast_date, reference_date, 
                temporal_resolution, target_variable,
                location, type, alpha,
                location_name_with_state, abbreviation) %>%
              dplyr::left_join(
                data %>% group_by(location) %>% slice_max(date, n = 1),
                by = c("location", "location_name_with_state", "abbreviation")
              ) %>%
              dplyr::transmute(
                forecast_date,
                target = paste("0", temporal_resolution, "ahead", type, target_variable),
                target_end_date = reference_date,
                location,
                lower = ifelse(type == "inc", inc, cum),
                upper = ifelse(type == "inc", inc, cum),
                type = "quantile",
                alpha,
                location_name,
                location_name_with_state,
                abbreviation
              )
            type_intervals <- dplyr::bind_rows(
              forecast_date_intervals,
              type_intervals
            )

            if (nrow(type_intervals) > 0) {
              made_plots <- TRUE
              p <- ggplot() +
                geom_ribbon(
                  data = type_intervals,
                  mapping = aes(
                    x = target_end_date,
                    ymin=lower, ymax=upper,
                    fill=alpha)) +
                geom_line(
                  data = results %>% dplyr::filter(location_name_with_state %in% batch_locations) %>%
                    filter(quantile == 0.5,
                          grepl(UQ(type), target),
                          grepl(target_measure, target)),
                  mapping = aes(x = target_end_date, y = value),
                  color = "blue") +
                geom_point(
                  data = results %>% dplyr::filter(location_name_with_state %in% batch_locations) %>%
                    filter(quantile == 0.5,
                          grepl(UQ(type), target),
                          grepl(target_measure, target)),
                  mapping = aes(x = target_end_date, y = value),
                  color = "blue") +
                geom_line(data=data %>%
                            dplyr::mutate(
                              date = lubridate::ymd(date),
                              group_factor = paste0(issue_date, location_name_with_state)
                            ) %>%
                            dplyr::filter(location_name_with_state %in% batch_locations),
                          mapping = aes_string(x = "date", y = type, color = "issue_date", linetype = "issue_date", group = "group_factor")) +
                geom_point(data=data %>%
                            dplyr::mutate(
                              date = lubridate::ymd(date),
                              group_factor = paste0(issue_date, location_name_with_state)
                            ) %>%
                            dplyr::filter(location_name_with_state %in% batch_locations),
                          mapping = aes_string(x = "date", y = type, color = "issue_date", shape = "issue_date", group = "group_factor")) +
                facet_wrap(~location_name_with_state, ncol=6, scales = 'free_y') +
                scale_color_manual("Issue Date", values = c("black", "orange")) +
                scale_linetype_discrete("Issue Date") +
                scale_shape_discrete("Issue Date") +
                scale_fill_viridis_d("Interval alpha", begin = 0.2, end = 0.8, direction = -1) +
                # scale_fill_brewer("Interval alpha", type = "seq", palette = "Blues") +
                ggtitle(paste(type, measure, as.character(forecast_week_end_date))) +
                theme_bw()
              print(p)
            }
          }
        }
        dev.off()
        if (!made_plots) {
          unlink(plot_path)
        }
      }
    }
  }
}
