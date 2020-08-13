library(zoltr)
library(tidyverse)
library(zeallot)
library(covidEnsembles)
library(covidData)
library(googledrive)
options(error=recover)

candidate_model_abbreviations_to_include <-
  c("Auquan-SEIR", "CAN-SEIR_CAN", "CDDEP-CDDEP_SEIR_MCMC", "Columbia_UNC-SurvCon",
    "Covid19Sim-Simulator", "CovidAnalytics-DELPHI", "COVIDhub-baseline",
    "CU-select", "epiforecasts-ensemble1", "Geneva-DeterministicGrowth",
    "GT_CHHS-COVID19", "GT-DeepCOVID", "IHME-CurveFit", "Imperial-Ensemble2",
    "IowaStateLW-STEM", "ISUandPKU-vSEIdR", "JHU_IDD-CovidSP", "LANL-GrowthRate",
    "MITCovAlliance-SIR",
    "MOBS-GLEAM_COVID", "NotreDame-FRED", "NotreDame-mobility", "OliverWyman-Navigator",
    "PSI-DRAFT", "QJHong-Encounter", "RobertWalraven-ESG", "STH-3PU",
    "SWC-TerminusCM", "UA-EpiCovDA", "UChicago-CovidIL",
    "UCLA-SuEIR", "UM_CFG-RidgeTfReg", "UMass-MechBayes", "USACE-ERDC_SEIR",
    "USC-SI_kJalpha", "UT-Mobility", "YYG-ParamSearch")




zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))
project_url <- 'https://www.zoltardata.com/api/project/44/'

required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
monday_dates <- lubridate::ymd('2020-07-06')

all_forecasts <-
  purrr::map_dfr(
    candidate_model_abbreviations_to_include,
    function(model_abbr) {
      covidEnsembles:::do_zoltar_query(
        model_abbr = model_abbr,
        last_timezero = monday_dates,
        timezero_window_size = 1,
        targets = c(paste0(1:4, ' wk ahead inc death'), paste0(1:4, ' wk ahead cum death')),
        zoltar_connection = zoltar_connection,
        project_url = project_url,
        verbose = TRUE
      ) %>%
        dplyr::mutate(unit = as.character(unit))
    }
  ) %>%
  dplyr::select(model, timezero, location=unit, target, quantile, value) %>%
  dplyr::filter(
    format(quantile, digits=3, nsmall=3) %in%
      format(required_quantiles, digits=3, nsmall=3)) %>%
  tidyr::pivot_wider(names_from = quantile, values_from = value) %>%
  dplyr::mutate(
    horizon = as.integer(substr(target, 1, 1)),
    forecast_week_end_date = calc_forecast_week_end_date(timezero),
    target_end_date = calc_target_week_end_date(timezero, horizon)
  ) %>%
  dplyr::group_by(
    location, target, forecast_week_end_date, model
  ) %>%
  dplyr::top_n(1, timezero) %>%
  tidyr::pivot_longer(
    cols = all_of(as.character(required_quantiles)),
    names_to = 'quantile',
    values_to = 'value') %>%
  ungroup() %>%
  left_join(fips_codes, by = 'location')


us_forecasts <- all_forecasts %>%
  dplyr::filter(location == 'US')

View(us_forecasts %>%
  dplyr::filter(grepl('cum', target), !is.na(value)) %>%
  dplyr::count(model))


cum_model_weights <- read_csv('https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/ensemble-metadata/2020-07-06-cum_death-model-weights.csv')
inc_model_weights <- read_csv('https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/ensemble-metadata/2020-07-06-inc_death-model-weights.csv')

cum_models <- cum_model_weights %>%
  dplyr::filter(location == 'US') %>%
  dplyr::select(-location, -location_abbreviation, -location_name) %>%
  tidyr::pivot_longer(cols = colnames(.), names_to = 'model', values_to = 'weight') %>%
  dplyr::filter(weight > 0) %>%
  dplyr::pull(model)

inc_models <- inc_model_weights %>%
  dplyr::filter(location == 'US') %>%
  dplyr::select(-location, -location_abbreviation, -location_name) %>%
  tidyr::pivot_longer(cols = colnames(.), names_to = 'model', values_to = 'weight') %>%
  dplyr::filter(weight > 0) %>%
  dplyr::pull(model)

both_models <- inc_models[inc_models %in% cum_models]
cum_models_not_in_both <- cum_models[!(cum_models %in% both_models)]
inc_models_not_in_both <- inc_models[!(inc_models %in% both_models)]


last_cum <- covidData::load_jhu_data(
  issue_date = '2020-07-05',
  spatial_resolution = 'national',
  temporal_resolution = 'weekly',
  measure = 'deaths') %>%
  tail(1) %>%
  pull(cum)
implied <- us_forecasts %>%
  dplyr::filter(
    model %in% both_models,
    quantile == '0.5',
    grepl('cum', target)) %>%
  dplyr::group_by(model) %>%
  dplyr::arrange(model, horizon) %>%
  dplyr::mutate(
    implied_median_inc =
      ifelse(
        is.na(dplyr::lag(value, 1)),
        value - last_cum,
        value - dplyr::lag(value, 1))
  ) %>%
  dplyr::select(model, horizon, implied_median_inc)

actual <- us_forecasts %>%
  dplyr::filter(
    model %in% both_models,
    quantile == '0.5',
    grepl('inc', target)) %>%
  dplyr::transmute(
    model = model,
    horizon = horizon,
    actual_median_inc = value
  )

implied_and_actual <- implied %>%
  dplyr::left_join(actual, by = c('model', 'horizon'))
# %>%
#   tidyr::pivot_longer(
#     cols = c('implied_median_inc', 'actual_median_inc'),
#     names_to = 'type',
#     values_to = 'value')

ggplot(
  data = implied_and_actual,
  mapping = aes(x = actual_median_inc,
                y = implied_median_inc,
                color = model,
                shape = factor(horizon))) +
  geom_point() +
  geom_line(mapping = aes(linetype = model, group = model)) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()



p1 <- ggplot(data = us_forecasts %>%
  dplyr::filter(
    model %in% cum_models,
    quantile == '0.5',
    grepl('cum', target)) %>%
  dplyr::mutate(
    cum_ensemble = (model %in% cum_models),
    inc_ensemble = (model %in% inc_models),
    both_ensembles = (cum_ensemble & inc_ensemble)
  )) +
  geom_point(mapping = aes(x = horizon, y = value, color = both_ensembles)) +
  ylab('median forecast: cumulative deaths') +
  theme_bw() +
  ggtitle("Cumulative Deaths")

p2 <- ggplot(data = us_forecasts %>%
               dplyr::filter(
                 model %in% inc_models,
                 quantile == '0.5',
                 grepl('inc', target)) %>%
               dplyr::mutate(
                 cum_ensemble = (model %in% cum_models),
                 inc_ensemble = (model %in% inc_models),
                 both_ensembles = (cum_ensemble & inc_ensemble)
               )) +
  geom_point(mapping = aes(x = horizon, y = value, color = both_ensembles)) +
  ylab('median forecast: incident deaths') +
  theme_bw() +
  ggtitle("Incident Deaths")

grid.arrange(p1, p2)



# Fit ensemble using only those models that were included in both ensembles
#

for(response_var in c('cum_death', 'inc_death')) {
  #for(response_var in 'cum_death') {
  #for(response_var in 'inc_death') {
  if(response_var == 'cum_death') {
    do_q10_check <- do_nondecreasing_quantile_check <- TRUE

    # adjustments based on plots
    if(forecast_date == '2020-06-08') {
      manual_eligibility_adjust <- c(
        "Auquan-SEIR", "CAN-SEIR_CAN", "CU-select", "UA-EpiCovDA", "SWC-TerminusCM"
      )
    } else if(forecast_date == '2020-06-15') {
      manual_eligibility_adjust <- "Auquan-SEIR"
    } else if(forecast_date == '2020-06-29') {
      manual_eligibility_adjust <- data.frame(
        model = c("epiforecasts-ensemble1", "NotreDame-mobility"),
        location = "34"
      )
    } else if(forecast_date == "2020-07-06") {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c("COVIDhub-baseline", "CU-select", "RobertWalraven-ESG", "USACE-ERDC_SEIR",
                  "MITCovAlliance-SIR"),
        location = fips_codes$location
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
    } else if(forecast_date == "2020-07-06") {
      manual_eligibility_adjust <- tidyr::expand_grid(
        model = c("COVIDhub-baseline", "CU-select", "RobertWalraven-ESG", "USACE-ERDC_SEIR",
                  "MITCovAlliance-SIR"),
        location = fips_codes$location
      )
    } else {
      manual_eligibility_adjust <- NULL
    }
  }

  c(model_eligibility, wide_model_eligibility, location_groups, component_forecasts) %<-%
    build_covid_ensemble_from_zoltar(
      candidate_model_abbreviations_to_include = both_models,
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
      do_baseline_check = FALSE,
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
}


both_quantiles <- all_formatted_ensemble_predictions %>%
  filter(location == 'US', quantile %in% c('0.025', '0.5', '0.975')) %>%
  select(location, target, quantile, value) %>%
  pivot_wider(names_from = 'quantile', names_prefix = 'both_q_') %>%
  arrange(target)

all_quantiles <- read_csv('https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-processed/COVIDhub-ensemble/2020-07-06-COVIDhub-ensemble.csv') %>%
  filter(location == 'US', quantile %in% c('0.025', '0.5', '0.975')) %>%
  select(location, target, quantile, value) %>%
  pivot_wider(names_from = 'quantile', names_prefix = 'separate_q_') %>%
  arrange(target)

both_quantiles %>%
  left_join(all_quantiles) %>%
  View()


