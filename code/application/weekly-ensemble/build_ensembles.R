library(zoltr)
library(tidyverse)
library(zeallot)
library(covidEnsembles)
library(googledrive)
options(warn=2, error=recover)

final_run <- TRUE


current_wday <- lubridate::wday(Sys.Date(), label = TRUE)
forecast_date <- if(current_wday == 'Mon') {
  Sys.Date()
} else if(current_wday == 'Tue') {
  Sys.Date() - 1
}

candidate_model_abbreviations_to_include <-
  c("Auquan-SEIR", "CAN-SEIR_CAN", "Columbia_UNC-SurvCon",
    "Covid19Sim-Simulator", "CovidAnalytics-DELPHI",
    "CU-select", "epiforecasts-ensemble1", "Geneva-DeterministicGrowth",
    "GT_CHHS-COVID19", "GT-DeepCOVID", "IHME-CurveFit", "Imperial-Ensemble2",
    "IowaStateLW-STEM", "ISUandPKU-vSEIdR", "JHU_IDD-CovidSP", "LANL-GrowthRate",
    "MOBS-GLEAM_COVID", "NotreDame-FRED", "OliverWyman-Navigator",
    "PSI-DRAFT", "STH-3PU", "SWC-TerminusCM", "UA-EpiCovDA", "UChicago-CovidIL",
    "UCLA-SuEIR", "UMass-MechBayes", "USACE-ERDC_SEIR", "UT-Mobility",
    "YYG-ParamSearch")

## get observed_by_location_target_end_date
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection,
                    Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))
project_url <- 'https://www.zoltardata.com/api/project/44/'

observed_by_location_target_end_date <-
  zoltr::truth(zoltar_connection, project_url) %>%
  dplyr::transmute(
    timezero = timezero,
    location = unit,
    horizon = as.integer(substr(target, 1, 1)),
    base_target = substr(target, 3, nchar(target)),
    observed = value,
    forecast_week_end_date = calc_forecast_week_end_date(timezero),
    target_end_date = calc_target_week_end_date(timezero, horizon)
  ) %>%
  dplyr::distinct(location, base_target, target_end_date, observed)

for(response_var in c('cum_death', 'inc_death')) {
#for(response_var in 'cum_death') {
#for(response_var in 'inc_death') {
  if(response_var == 'cum_death') {
    do_q10_check <- do_nondecreasing_quantile_check <- TRUE

    # adjustments based on plots
    if(forecast_date == '2020-06-08') {
      manual_eligibility_adjust <- c(
        "Auquan-SEIR", "CAN-SEIR_CAN", "UA-EpiCovDA", "SWC-TerminusCM"
      )
    } else if(forecast_date == '2020-06-15') {
      manual_eligibility_adjust <- "Auquan-SEIR"
    } else if(forecast_date == '2020-06-29') {
      manual_eligibility_adjust <- data.frame(
        model = c("epiforecasts-ensemble1", "NotreDame-mobility"),
        location = "34"
      )
    } else {
      manual_eligibility_adjust <- NULL
    }
  } else {
    do_q10_check <- do_nondecreasing_quantile_check <- FALSE

    # adjustments based on plots
    if(forecast_date == '2020-06-08') {
      manual_eligibility_adjust <- c(
        "CAN-SEIR_CAN", "SWC-TerminusCM", "USACE-ERDC_SEIR", "IHME-CurveFit"
      )
    } else if(forecast_date == '2020-06-15') {
      manual_eligibility_adjust <- c(
        "USACE-ERDC_SEIR", "LANL-GrowthRate"
      )
    } else if(forecast_date == '2020-06-29') {
      manual_eligibility_adjust <- bind_rows(
        data.frame(
          model = c("epiforecasts-ensemble1", "NotreDame-mobility"),
          location = "34",
          stringsAsFactors = FALSE
        ),
        data.frame(
          model = "CU-select",
          location = fips_codes$location,
          stringsAsFactors = FALSE
        )
      )
    } else {
      manual_eligibility_adjust <- NULL
    }
  }

  c(model_eligibility, wide_model_eligibility, location_groups, component_forecasts) %<-%
    build_covid_ensemble_from_zoltar(
      candidate_model_abbreviations_to_include = candidate_model_abbreviations_to_include,
      targets = paste0(1:4, ' wk ahead ', gsub('_', ' ', response_var)),
      forecast_week_end_date = forecast_date - 2,
      timezero_window_size = 1,
      window_size = 0,
      intercept = FALSE,
      constraint = 'ew',
      quantile_groups = rep(1, 23),
      missingness = 'by_location_group',
      backend = NA,
      project_url = 'https://www.zoltardata.com/api/project/44/',
      required_quantiles = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99),
      do_q10_check = do_q10_check,
      do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
      manual_eligibility_adjust = manual_eligibility_adjust,
      return_eligibility = TRUE,
      return_all = TRUE
    )

  ensemble_predictions <- bind_rows(location_groups[['qra_forecast']])

  # save the results in required format
  formatted_ensemble_predictions <- ensemble_predictions %>%
    left_join(
      fips_codes,# %>% select(location, location_name = location_abbreviation),
      by='location') %>%
    dplyr::transmute(
      forecast_date = UQ(forecast_date),
      target = target,
      target_end_date = calc_target_week_end_date(
        forecast_week_end_date,
        as.integer(substr(target, 1, 1))),
      location = location,
      location_name = location_name,
      type = 'quantile',
      quantile = quantile,
      value = value
    )

  formatted_ensemble_predictions <- bind_rows(
    formatted_ensemble_predictions,
    formatted_ensemble_predictions %>%
      filter(format(quantile, digits=3, nsmall=3) == '0.500') %>%
      mutate(
        type='point',
        quantile=NA_real_
      )
  )

  # reformat model weights and eligibility for output
  model_weights <- purrr::pmap_dfr(
    location_groups %>% select(locations, qra_fit),
    function(locations, qra_fit) {
      temp <- qra_fit$coefficients %>%
        tidyr::pivot_wider(names_from = 'model', values_from = 'beta')

      return(purrr::map_dfr(
        locations,
        function(location) {
          temp %>%
            mutate(location = location)
        }
      ))
    }
  )
  model_weights <- bind_cols(
    model_weights %>%
      select(location) %>%
      left_join(fips_codes, by = 'location'),
    model_weights %>% select(-location)
  ) %>%
    arrange(location)
  model_weights[is.na(model_weights)] <- 0.0

  if(response_var == 'cum_death') {
    all_formatted_ensemble_predictions <- formatted_ensemble_predictions
  } else {
    all_formatted_ensemble_predictions <- bind_rows(
      all_formatted_ensemble_predictions,
      formatted_ensemble_predictions
    )
  }

  for(root in c('code/application/weekly-ensemble/forecasts/', '../covid19-forecast-hub/')) {
    if(final_run) {
      write_csv(all_formatted_ensemble_predictions,
                paste0(root, 'data-processed/COVIDhub-ensemble/',
                       formatted_ensemble_predictions$forecast_date[1],
                       '-COVIDhub-ensemble.csv')
      )

      write_csv(model_eligibility,
        paste0(root, 'ensemble-metadata/',
          formatted_ensemble_predictions$forecast_date[1],
          '-',
          response_var,
          '-model-eligibility.csv'))

      write_csv(model_weights,
        paste0(root, 'ensemble-metadata/',
          formatted_ensemble_predictions$forecast_date[1],
          '-',
          response_var,
          '-model-weights.csv'))
    }
  }

  # visualize
  observed <- observed_by_location_target_end_date %>%
    mutate(target_end_date = lubridate::ymd(target_end_date)) %>%
    filter(base_target == paste0('wk ahead ', gsub('_', ' ', response_var))) %>%
    left_join(fips_codes, by='location')

  plottable_ensemble_predictions <- formatted_ensemble_predictions %>%
  #  filter(quantile != 0.5) %>%
    mutate(
      endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
      alpha = ifelse(
        endpoint_type == 'lower',
        format(2*quantile, digits=3, nsmall=3),
        format(2*(1-quantile), digits=3, nsmall=3))
    ) %>%
    select(-quantile) %>%
    tidyr::pivot_wider(names_from='endpoint_type', values_from='value')

  pdf(paste0('code/application/weekly-ensemble/',
             forecast_date,
             '_prediction_plots_',
             response_var,
             ifelse(final_run, '_final', '_preliminary'),
             '.pdf'),
      width=24, height=60)
  p <- ggplot() +
    geom_line(data=observed, mapping = aes(x = target_end_date, y = observed, group = location_name)) +
    geom_point(data=observed, mapping = aes(x = target_end_date, y = observed, group = location_name)) +
    geom_ribbon(
      data = plottable_ensemble_predictions %>% filter(alpha != "1.000"),
      mapping = aes(x = lubridate::ymd(target_end_date),
                    ymin=lower, ymax=upper,
                    fill=alpha)) +
    geom_line(
      data = formatted_ensemble_predictions %>%
        filter(quantile == 0.5),
      mapping = aes(x = lubridate::ymd(target_end_date), y = value)) +
    geom_point(
      data = formatted_ensemble_predictions %>%
        filter(quantile == 0.5),
      mapping = aes(x = lubridate::ymd(target_end_date), y = value)) +
    facet_wrap(~location_name, ncol=4, scales = 'free_y') +
    theme_bw()
  print(p)
  dev.off()



  all_location_names <- unique(model_weights$location_name)
  #location_abbr <- 'AK'

  pdf(paste0('code/application/weekly-ensemble/',
             forecast_date,
             '_prediction_plots_all_locations_',
             response_var,
             ifelse(final_run, '_final', '_preliminary'),
             '.pdf'),
      width=24, height=12)
  for(location_name in all_location_names) {
    location_models <- model_weights %>%
      dplyr::filter(location_name == UQ(location_name)) %>%
      dplyr::select(-location, -location_name, -location_abbreviation) %>%
      tidyr::pivot_longer(
        cols = seq_len(ncol(.)),
        names_to = 'model',
        values_to = 'weight') %>%
      dplyr::filter(weight > .Machine$double.eps) %>%
      dplyr::pull(model)

    location_forecasts <- component_forecasts %>%
      dplyr::filter(
        location_name == UQ(location_name),
        model %in% location_models) %>%
      dplyr::mutate(
        quantile = as.numeric(quantile),
        endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
        alpha = ifelse(
          endpoint_type == 'lower',
          format(2*quantile, digits=3, nsmall=3),
          format(2*(1-quantile), digits=3, nsmall=3))
      ) %>%
      dplyr::select(-quantile) %>%
      tidyr::pivot_wider(names_from='endpoint_type', values_from='value') %>%
      dplyr::bind_rows(
        plottable_ensemble_predictions %>%
          mutate(
            model = 'ensemble'
          ) %>%
          dplyr::filter(location_name == UQ(location_name))
      )

    p <- ggplot() +
      geom_line(data=observed %>% filter(location_name == UQ(location_name)),
                mapping = aes(x = target_end_date, y = observed)) +
      geom_point(data=observed %>% filter(location_name == UQ(location_name)),
                 mapping = aes(x = target_end_date, y = observed)) +
      geom_ribbon(
        data = location_forecasts %>% filter(alpha %in% c('0.020', '0.050', '0.200', '0.500')),
                      mapping = aes(x = lubridate::ymd(target_end_date),
                      ymin=lower, ymax=upper,
                      fill=alpha),
      ) +
      scale_fill_viridis_d(begin = 0.25, option = 'B') +
      geom_line(
        data = location_forecasts %>%
          filter(alpha == '1.000'),
        mapping = aes(x = lubridate::ymd(target_end_date), y = upper)) +
      facet_wrap(~model, ncol=4, scales = 'free_y') +
      ggtitle(location_name) +
      theme_bw()

    print(p)
  }
  dev.off()
}
