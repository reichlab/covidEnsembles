library(zoltr)
library(tidyverse)
library(covidEnsembles)
options(warn=2, error=recover)

zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

all_truths <- truth(zoltar_connection, 'https://www.zoltardata.com/api/project/44/') %>%
  transmute(
    timezero = timezero,
    unit = unit,
    target = target,
    observed = value
  )

fips_codes <- read_csv("data-raw/state_fips_codes.csv") %>%
  bind_rows(
    data.frame(
      state='US',
      state_code='US',
      state_name='United States',
      stringsAsFactors=FALSE)) %>%
  select(state = state, unit = state_code)

## from https://github.com/reichlab/covid19-forecast-hub/blob/master/code/ensemble-scripts/make_ewq_ensemble_script.R#L12
## models to exclude due to multiple models per team
## TODO: Maybe we should change this to a list of included models?
## For 2020-05-18:
## - JHU submitted only 1 model
## - UChicago requests CovidIL_10_increase
models_to_exclude <- c(
  ## Our ensemble
  'ensemble',
  ## CU
  "nointerv", "60-contact", "70-contact", "80-contact",
  "80-contact1x10p", "80-contact1x5p", "80-contactw10p", "80-contactw5p",
  ## Imperial
  "Ensemble1",# "Ensemble2",
  ## IowaStateLW
  "Spatiotemporal Epidemic Modeling Daily Recovery 8%",
  "Spatiotemporal Epidemic Modeling Daily Recover 15%",
  ## UChicago
  "CovidIL_40", "CovidIL_60", "CovidIL_80", "CovidIL_100", "CovidIL_10_increase", "CovidIL_30_increase",
  ## UMass
  "Expert consensus distributions")

# keep only quantile forecasts for requested quantiles
# that were in the last submission from a given model for each forecast week
# end date
required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

#for(response_var in c('cum_death', 'inc_death')) {
for(response_var in 'cum_death') {
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
#    model_eligibility$eligibility[
#      model_eligibility$model_abbreviation == "JHU_IDD-CovidSP"] <-
#        'baseline misalignment'
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


  for(root in c('code/application/weekly-ensemble/forecasts/', '../covid19-forecast-hub/')) {
    if(response_var == 'cum_death') {
      all_formatted_ensemble_predictions <- formatted_ensemble_predictions
    } else {
      all_formatted_ensemble_predictions <- bind_rows(
        all_formatted_ensemble_predictions,
        formatted_ensemble_predictions
      )
    }
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


  #system('alias chrome="/Applications/Google\\ \\Chrome.app/Contents/MacOS/Google\\ \\Chrome"')
  #system('chrome --allow-file-access-from-files ./code/application/plot_ensemble_forecasts.html')


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

  pdf(paste0('code/application/weekly-ensemble/prediction_plots_', response_var, '.pdf'),
      width=24, height=60)
  p <- ggplot() +
    geom_line(data=observed, mapping = aes(x = time, y = observed)) +
    geom_point(data=observed, mapping = aes(x = time, y = observed)) +
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



  all_locations <- unique(model_weights$location_name)
  #location <- 'AK'

  pdf(paste0('code/application/weekly-ensemble/prediction_plots_all_locations_', response_var, '.pdf'),
      width=24, height=12)
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
            model_abbreviation = 'ensemble'
          ) %>%
          dplyr::filter(location_name == UQ(location))
      )

    p <- ggplot() +
      geom_line(data=observed %>% filter(location_name == UQ(location)),
                mapping = aes(x = time, y = observed)) +
      geom_point(data=observed %>% filter(location_name == UQ(location)),
                 mapping = aes(x = time, y = observed)) +
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
      facet_wrap(~model_abbreviation, ncol=4, scales = 'free_y') +
      ggtitle(location) +
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

