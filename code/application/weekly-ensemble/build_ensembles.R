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

candidate_model_abbreviations_to_include <- c(
  "IHME-CurveFit", "UMass-MechBayes", "YYG-ParamSearch", "LANL-GrowthRate",
  "CovidAnalytics-DELPHI", "UCLA-SuEIR", "UT-Mobility",
  "Geneva-DeterministicGrowth", "UA-EpiCovDA", "CU-select",
  "IowaStateLW-STEM", "JHU_IDD-CovidSP", "Imperial-Ensemble2", "Auquan-SEIR",
  "NotreDame-FRED", "USACE-ERDC_SEIR", "UChicago-CovidIL", "GT-DeepCOVID",
  "MOBS-GLEAM_COVID", "SWC-TerminusCM", "PSI-DRAFT", "CAN-SEIR_CAN",
  "GT_CHHS-COVID19", "COVID19SimCons-COVID19Sim", "ISUandPKU-vSEIdR", "STH-3PU")

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

    # adjustments for 2020-06-08
    manual_eligibility_adjust <- c(
      "Auquan-SEIR", "CAN-SEIR_CAN", "UA-EpiCovDA", "SWC-TerminusCM"
    )
  } else {
    do_q10_check <- do_nondecreasing_quantile_check <- FALSE

    manual_eligibility_adjust <- c(
      "CAN-SEIR_CAN", "SWC-TerminusCM", "USACE-ERDC_SEIR", "IHME-CurveFit"
    )
  }

  c(model_eligibility, wide_model_eligibility, location_groups, component_forecasts) %<-%
    build_covid_ensemble_from_zoltar(
      candidate_model_abbreviations_to_include = candidate_model_abbreviations_to_include,
      targets = paste0(1:4, ' wk ahead ', gsub('_', ' ', response_var)),
      forecast_week_end_date = forecast_date - 2,
      timezero_window_size = 1,
      window_size = 0,
      ensemble_method = 'ew',
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
      dplyr::select(-location, -location_name) %>%
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



# Experimenting with summarizing predicted changes by state across models
for(response_var in 'inc_death') {
  all_forecasts_and_observed <-
    readRDS(paste0('./data/all_', response_var, '_forecasts.rds')) %>%
    #    dplyr::mutate(
    #      model_abbreviation = ifelse(
    #        model_abbreviation == 'ERDC-ERDC-SEIR',
    #        'ERDC-SEIR',
    #        model_abbreviation)
    #    ) %>%
    dplyr::filter(
      !(model_name %in% models_to_exclude),
      lubridate::wday(lubridate::ymd(timezero), label=TRUE) %in% c('Sun', 'Mon'),
      class == 'quantile',
      quantile %in% as.character(required_quantiles)) %>%
    mutate(
      value = as.numeric(value)
    ) %>%
    dplyr::select(unit, timezero, target, model_id, model_name, model_abbreviation, quantile, value) %>%
    tidyr::pivot_wider(names_from = quantile, values_from = value) %>%
    dplyr::left_join(all_truths, by = c('timezero', 'unit', 'target')) %>%
    dplyr::mutate(
      horizon = as.integer(substr(target, 1, 1)),
      forecast_week_end_date = calc_forecast_week_end_date(timezero),
      target_end_date = calc_target_week_end_date(timezero, horizon)
    ) %>%
    dplyr::group_by(
      unit, target, forecast_week_end_date, model_abbreviation
    ) %>%
    dplyr::top_n(1, timezero) %>%
    tidyr::pivot_longer(
      cols = all_of(as.character(required_quantiles)),
      names_to = 'quantile',
      values_to = 'value') %>%
    ungroup() %>%
    left_join(fips_codes, by = c("unit"))

  # compute model eligibility
  observed_by_unit_target_end_date <- all_forecasts_and_observed %>%
    dplyr::distinct(unit, target_end_date, observed)

  this_week_forecasts_and_observed <- all_forecasts_and_observed %>%
    filter(forecast_week_end_date == max(forecast_week_end_date))
  forecast_matrix <- new_QuantileForecastMatrix_from_df(
    forecast_df = this_week_forecasts_and_observed,
    model_col = 'model_abbreviation',
    id_cols = c('unit', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'quantile',
    quantile_value_col = 'value'
  )

  if(response_var == 'cum_death') {
    do_q10_check <- TRUE
  } else {
    do_q10_check <- FALSE
  }
  model_eligibility <- calc_model_eligibility_for_ensemble(
    qfm = forecast_matrix,
    observed_by_unit_target_end_date = observed_by_unit_target_end_date,
    do_q10_check = do_q10_check,
    lookback_length = 0
  ) %>%
    left_join(fips_codes %>% transmute(unit=unit, location_name=state), by = 'unit')

  ## Adjustments specific to 2020-05-18:
  #model_eligibility$eligibility[
  #  model_eligibility$model_abbreviation == "GT-DeepCOVID" &
  #  model_eligibility$location_name %in% c('CT', 'DC', 'MI', 'VA')
  #] <- 'decreasing quantiles over time'
  #
  #model_eligibility$eligibility[
  #  model_eligibility$model_abbreviation == 'CU-select'
  #] <- 'baseline misalignment'
  if(response_var == 'cum_death') {
    model_eligibility$eligibility[
      model_eligibility$model_abbreviation == "GT-DeepCOVID" &
        model_eligibility$location_name %in% c('KS', 'OR')
      ] <- 'decreasing quantiles over time'
  } else if(response_var == 'inc_death') {
    model_eligibility$eligibility[
      model_eligibility$model_abbreviation == "UT-Mobility"] <-
      'baseline misalignment'
    model_eligibility$eligibility[
      model_eligibility$model_abbreviation == "JHU_IDD-CovidSP"] <-
      'baseline misalignment'
  }

  # convert model eligibility to wide format logical with human readable names
  wide_model_eligibility <- model_eligibility %>%
    mutate(eligibility = (eligibility == 'eligible')) %>%
    pivot_wider(names_from='model_abbreviation', values_from='eligibility') %>%
    left_join(fips_codes, by = "unit") %>%
    arrange(unit) %>%
    select(state, 2:(ncol(.)-1))

  # group states by which models are included per state
  state_groups <- wide_model_eligibility %>%
    group_by_if(is.logical) %>%
    summarize(states = list(state)) %>%
    ungroup()

  # fit equally weighted ensemble per group
  fit_one_group <- function(i) {
    model_inds <- state_groups[i, -ncol(state_groups)] %>%
      as.matrix() %>%
      which()
    models <- colnames(state_groups)[model_inds]

    forecast_matrix <- new_QuantileForecastMatrix_from_df(
      forecast_df = all_forecasts_and_observed %>%
        filter(model_abbreviation %in% models),
      model_col = 'model_abbreviation',
      id_cols = c('unit', 'forecast_week_end_date', 'target'),
      quantile_name_col = 'quantile',
      quantile_value_col = 'value'
    )

    ew_qra_fit <- estimate_qra(qfm_train = forecast_matrix, qra_model = 'ew')
    return(ew_qra_fit)
  }

  state_groups$ew_qra_fits <- purrr::map(
    seq_len(nrow(state_groups)),
    fit_one_group
  )


  # obtain predictions from ensemble
  predict_one_group <- function(i) {
    model_inds <- state_groups[i, seq_len(ncol(state_groups)-2)] %>%
      as.matrix() %>%
      which()
    models <- colnames(state_groups)[model_inds]
    states <- state_groups$states[[i]]
    ew_qra_fit <- state_groups$ew_qra_fits[[i]]

    forecast_matrix <- new_QuantileForecastMatrix_from_df(
      forecast_df = all_forecasts_and_observed %>%
        filter(
          model_abbreviation %in% models,
          state %in% states,
          forecast_week_end_date == max(forecast_week_end_date)),
      model_col = 'model_abbreviation',
      id_cols = c('unit', 'forecast_week_end_date', 'target'),
      quantile_name_col = 'quantile',
      quantile_value_col = 'value'
    )

    return(
      ew_qra_fit %>%
        predict(forecast_matrix) %>%
        as.data.frame()
    )
  }

  ensemble_predictions <- purrr::map_dfr(
    seq_len(nrow(state_groups)),
    predict_one_group
  )


  # save the results in required format
  formatted_ensemble_predictions <- ensemble_predictions %>%
    left_join(fips_codes, by='unit') %>%
    dplyr::transmute(
      forecast_date = as.character(Sys.Date()),
      target = target,
      target_end_date = calc_target_week_end_date(
        forecast_week_end_date,
        as.integer(substr(target, 1, 1))),
      location = unit,
      location_name = state,
      type = 'quantile',
      quantile = quantile,
      value = value
    )
  formatted_ensemble_predictions$forecast_date <- '2020-05-25'


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
    state_groups %>% select(states, ew_qra_fits),
    function(states, ew_qra_fits) {
      temp <- ew_qra_fits$coefficients %>%
        tidyr::pivot_wider(names_from = 'model_abbreviation', values_from = 'beta')

      return(purrr::map_dfr(
        states,
        function(state) {
          temp %>%
            mutate(state = state)
        }
      ))
    }
  )
  model_weights <- bind_cols(
    model_weights %>%
      select(location_name = state) %>%
      left_join(fips_codes %>% transmute(location_name=state, location=unit),
                by = 'location_name'),
    model_weights %>% select(-state)
  ) %>%
    arrange(location)
  model_weights[is.na(model_weights)] <- 0.0


  model_eligibility <- model_eligibility %>%
    left_join(fips_codes, by = 'unit') %>%
    select(
      location = unit,
      location_name = state,
      model_abbreviation = model_abbreviation,
      eligibility = eligibility
    ) %>%
    arrange(model_abbreviation, location)

  # visualize
  observed <- all_truths %>%
    filter(target == paste0('1 wk ahead ', gsub('_', ' ', response_var))) %>%
    mutate(
      time = calc_target_week_end_date(
        timezero,
        as.integer(substr(target, 1, 1))) %>%
        lubridate::ymd()
    ) %>%
    left_join(fips_codes %>% select(unit=unit, location_name=state), by='unit')

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


  all_locations <- unique(model_weights$location_name)
  all_preds <- NULL

  for(location in all_locations) {
    location_models <- model_weights %>%
      dplyr::filter(location_name == UQ(location)) %>%
      dplyr::select(-location_name, -location) %>%
      tidyr::pivot_longer(
        cols = seq_len(ncol(.)),
        names_to = 'model_abbreviation',
        values_to = 'weight') %>%
      dplyr::filter(weight > .Machine$double.eps) %>%
      dplyr::pull(model_abbreviation)

    location_forecasts <- this_week_forecasts_and_observed %>%
      dplyr::filter(
        state == location,
        model_abbreviation %in% location_models) %>%
      dplyr::mutate(
        location_name = state,
        quantile = as.numeric(quantile)
      )

    all_preds <- bind_rows(
      all_preds,
      location_forecasts
    )
  }
}


temp <- all_preds %>%
  select(-horizon, -target_end_date) %>%
  filter(quantile == '0.5', model_abbreviation != 'Imperial-Ensemble2') %>%
  group_by(model_abbreviation, state) %>%
  pivot_wider(names_from=target, values_from = value) %>%
  summarize(diff = `4 wk ahead inc death` - `1 wk ahead inc death`) %>%
  ungroup() %>%
  group_by(state) %>%
  summarize(
    prop_dec = mean(diff < 0),
    mean_diff = mean(diff)
  )

