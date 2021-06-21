context("wis")
library(covidEnsembles)
library(dplyr)
library(tidyverse)
library(scoringutils)


test_that("wis works, median only", {
  y <- c(1, -15, 22)
  quantiles <- matrix(c(1, 2, 3), ncol = 1)
  quantile_probs <- 0.5

  attributes(quantiles) <- c(
    attributes(quantiles),
    list(
      row_index = data.frame(
        letters[1:3],
        stringsAsFactors = FALSE),
      col_index = data.frame(
        q_prob = as.character(quantile_probs),
        model = 'm1',
        stringsAsFactors = FALSE),
      model_col = 'model',
      quantile_name_col = 'q_prob',
      quantile_value_col = 'q_val'
    )
  )

  actual <- wis(y=y, qfm=quantiles)
  expected <- abs(y - quantiles[, 1])

  expect_equal(actual, expected)
})


test_that("wis works, 1 interval only", {
  y <- c(1, -15, 22)
  quantiles <- rbind(c(0, 2), c(1, 2), c(0, 3))
  quantile_probs <- c(0.25, 0.75)
  attributes(quantiles) <- c(
    attributes(quantiles),
    list(
      row_index = data.frame(
        letters[1:3],
        stringsAsFactors = FALSE),
      col_index = data.frame(
        q_prob = as.character(quantile_probs),
        model = 'm1',
        stringsAsFactors = FALSE),
      model_col = 'model',
      quantile_name_col = 'q_prob',
      quantile_value_col = 'q_val'
    )
  )

  alpha <- 0.5

  actual <- wis(y=y, qfm=quantiles)
  expected <- (quantiles[, 2] - quantiles[, 1])*(alpha/2) + c(0, 1-(-15), 22-3)

  expect_equal(actual, expected)
})


test_that("wis works, 1 interval and median", {
  y <- c(1, -15, 22)
  quantiles <- rbind(c(0, 1, 2), c(1, 2, 2), c(0, 3, 3))
  quantile_probs <- c(0.25, 0.5, 0.75)
  attributes(quantiles) <- c(
    attributes(quantiles),
    list(
      row_index = data.frame(
        letters[1:3],
        stringsAsFactors = FALSE),
      col_index = data.frame(
        q_prob = as.character(quantile_probs),
        model = 'm1',
        stringsAsFactors = FALSE),
      model_col = 'model',
      quantile_name_col = 'q_prob',
      quantile_value_col = 'q_val'
    )
  )

  alpha <- 0.5

  actual <- wis(y=y, qfm=quantiles)
  expected <- (1 / (1 + 0.5)) * (
    0.5 * abs(y - quantiles[, 2]) +
    (quantiles[, 3] - quantiles[, 1])*(alpha/2) + c(0, 1-(-15), 22-3)
  )

  expect_equal(actual, expected)
})


test_that("wis works, 2 intervals and median", {
  y <- c(1, -15, 22)
  quantiles <- rbind(c(-1, 0, 1, 2, 3), c(-2, 1, 2, 2, 4), c(-2, 0, 3, 3, 4))
  quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  attributes(quantiles) <- c(
    attributes(quantiles),
    list(
      row_index = data.frame(
        letters[1:3],
        stringsAsFactors = FALSE),
      col_index = data.frame(
        q_prob = as.character(quantile_probs),
        model = 'm1',
        stringsAsFactors = FALSE),
      model_col = 'model',
      quantile_name_col = 'q_prob',
      quantile_value_col = 'q_val'
    )
  )

  alpha1 <- 0.2
  alpha2 <- 0.5

  actual <- wis(y=y, qfm=quantiles)
  expected <- (1 / (2 + 0.5)) * (
    0.5 * abs(y - quantiles[, 3]) +
    (quantiles[, 5] - quantiles[, 1])*(alpha1/2) + c(0, (-2)-(-15), 22-4) +
    (quantiles[, 4] - quantiles[, 2])*(alpha2/2) + c(0, 1-(-15), 22-3)
  )

  expect_equal(actual, expected)
})



# test_that("relative wis agrees with scoringutils", {
#   # load some forecasts to score
#   monday_dates <- as.Date(c("2021-04-19", "2021-04-26"))

#   model_abbrs <- c("AIpert-pwllnod", "Auquan-SEIR", "BPagano-RtDriven", "Caltech-CS156",
#     "CDDEP-SEIR_MCMC", "CEID-Walk", "CMU-TimeSeries", "Columbia_UNC-SurvCon",
#     "Covid19Sim-Simulator", "CovidActNow-SEIR_CAN", "CovidAnalytics-DELPHI",
#     "COVIDhub-baseline", "COVIDhub-ensemble", "COVIDhub-trained_ensemble",
#     "CU-nochange", "CU-scenario_high", "CU-scenario_low", "CU-scenario_mid",
#     "CU-select", "DDS-NBDS", "epiforecasts-ensemble1", "FAIR-NRAR",
#     "FDANIHASU-Sweight", "FRBSF_Wilson-Econometric", "Geneva-DetGrowth",
#     "Google_Harvard-CPF", "GT_CHHS-COVID19", "GT-DeepCOVID", "IBF-TimeSeries",
#     "IEM_MED-CovidProject", "IHME-CurveFit", "Imperial-ensemble1",
#     "Imperial-ensemble2", "IowaStateLW-STEM", "IQVIA_ACOE-STAN",
#     "ISUandPKU-vSEIdR", "IUPUI-HkPrMobiDyR", "JCB-PRM", "JHU_CSSE-DECOM",
#     "JHU_IDD-CovidSP", "JHU_UNC_GAS-StatMechPool", "JHUAPL-Bucky",
#     "JHUAPL-Gecko", "JHUAPL-SLPHospEns", "Karlen-pypm", "KITmetricslab-select_ensemble",
#     "LANL-GrowthRate", "LNQ-ens1", "Microsoft-DeepSTIA", "MIT_CritData-GBCF",
#     "MIT_ISOLAT-Mixtures", "MIT-Cassandra", "MITCovAlliance-SIR",
#     "MOBS-GLEAM_COVID", "MSRA-DeepST", "NotreDame-FRED", "NotreDame-mobility",
#     "OliverWyman-Navigator", "OneQuietNight-ML", "PandemicCentral-COVIDForest",
#     "PandemicCentral-USCounty", "PSI-DRAFT", "QJHong-Encounter",
#     "Quantori-Multiagents", "RobertWalraven-ESG", "RPI_UW-Mob_Collision",
#     "SigSci-TS", "SteveMcConnell-CovidComplete", "STH-3PU", "SWC-TerminusCM",
#     "TTU-squider", "UA-EpiCovDA", "UCF-AEM", "UChicago-CovidIL",
#     "UChicago-CovidIL_10_+", "UChicago-CovidIL_100", "UChicago-CovidIL_30_+",
#     "UChicago-CovidIL_40", "UChicago-CovidIL_60", "UChicago-CovidIL_80",
#     "UChicagoCHATTOPADHYAY-UnIT", "UCLA-SuEIR", "UCM_MESALab-FoGSEIR",
#     "UCSB-ACTS", "UCSD_NEU-DeepGLEAM", "UMass-ExpertCrowd", "UMass-MechBayes",
#     "UMich-RidgeTfReg", "UpstateSU-GRU", "USACE-ERDC_SEIR", "USC-SI_kJalpha",
#     "USC-SI_kJalpha_RF", "UT-Mobility", "UVA-Ensemble", "Wadhwani_AI-BayesOpt",
#     "WalmartLabsML-LogForecasting", "Yu_Group-CLEP", "YYG-ParamSearch")

#   response_var <- "inc_hosp"
#   timezero_window_size <- 6L
#   horizon <- 28L
#   targets <- paste0(1:(horizon + 6), " day ahead ", gsub("_", " ", response_var))
#   required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

#   observed_by_location_target_end_date <-
#     get_observed_by_location_target_end_date(
#       as_of = as.character(Sys.Date()),
#       targets = targets,
#       spatial_resolution = "state"
#     )

#   forecasts <- load_covid_forecasts_relative_horizon(
#     hub = "US",
#     source = "zoltar",
#     monday_dates = monday_dates,
#     model_abbrs = model_abbrs,
#     timezero_window_size = timezero_window_size,
#     locations = unique(observed_by_location_target_end_date$location),
#     targets = targets,
#     max_horizon = horizon,
#     required_quantiles = required_quantiles
#   )

#   forecasts_qfm <- new_QuantileForecastMatrix_from_df(
#     forecasts,
#     model_col = "model",
#     id_cols = c("location", "forecast_week_end_date", "target"),
#     quantile_name_col = "quantile",
#     quantile_value_col = "value",
#     drop_missing_id_levels = FALSE
#   )

#   observed <- attr(forecasts_qfm, 'row_index') %>%
#     dplyr::mutate(
#       target_end_date = as.character(
#         lubridate::ymd(forecast_week_end_date) +
#           as.numeric(substr(target, 1, regexpr(" ", target, fixed = TRUE) - 1)) *
#             ifelse(grepl("day", target), 1, 7)
#       ),
#       base_target = substr(target, regexpr(" ", target, fixed = TRUE) + 1, nchar(target))
#     ) %>%
#     dplyr::left_join(
#       observed_by_location_target_end_date,
#       by = c('location', 'target_end_date', 'base_target')) %>%
#     dplyr::pull(observed)

#   to_keep <- !is.na(observed)
#   forecasts_qfm <- forecasts_qfm[to_keep, ]
#   observed <- observed[to_keep]

#   rel_wis <- covidEnsembles:::calc_relative_wis(y = observed, qfm = forecasts_qfm)

#   y <- observed
#   qfm <- forecasts_qfm

#   col_index <- attr(qfm, 'col_index')
#   model_col <- attr(qfm, 'model_col')
#   models <- unique(col_index[[model_col]])
#   wis_per_model <- matrix(NA, nrow = nrow(qfm), ncol = length(models))

#   for (m in seq_along(models)) {
#     model <- models[m]
#     model_qfm <- qfm[, col_index[[model_col]] == model]
#     wis_per_model[, m] <- wis(y, model_qfm)
#   }

#   pairwise_ratios <- diag(length(models))
#   for (m1 in seq_along(models)) {
#     for (m2 in seq_len(m1 - 1)) {
#       non_na_inds <- which(!is.na(wis_per_model[, m1]) & !is.na(wis_per_model[, m2]))
#       pairwise_ratios[m1, m2] <- mean(wis_per_model[non_na_inds, m1]) / mean(wis_per_model[non_na_inds, m2])
#       pairwise_ratios[m2, m1] <- 1 / pairwise_ratios[m1, m2]
#     }
#   }

#   rownames(pairwise_ratios) <- models
#   if ("COVIDhub-baseline" %in% models) {
#     ind_baseline <- which(rownames(pairwise_ratios) == "COVIDhub-baseline")
#   } else {
#     ind_baseline <- 1L
#   }
#   geom_mean_ratios <- exp(rowMeans(log(pairwise_ratios[, -ind_baseline]), na.rm = TRUE))
#   ratios_baseline2 <- geom_mean_ratios / geom_mean_ratios[ind_baseline]

#   tab <- data.frame(
#     model = names(geom_mean_ratios),
#     rel_wis = ratios_baseline2)

#   tab <- tab[order(tab$rel_wis), ]



#   forecasts_and_truth <- forecasts %>%
#     dplyr::mutate(
#       quantile = as.numeric(quantile),
#       prediction = value,
#       base_target = substr(target, regexpr(" ", target) + 1, nchar(target)),
#       target_end_date = as.character(target_end_date)
#     ) %>%
#     dplyr::left_join(
#       observed_by_location_target_end_date %>%
#         dplyr::rename(true_value = observed),
#         by = c("location", "base_target", "target_end_date")
#     ) %>%
#     dplyr::filter(!is.na(true_value))

#   observation_cols <- c("model", "location", "target", "target_end_date")
#   su_scores <- scoringutils::eval_forecasts(
#     data = forecasts_and_truth %>%
#       dplyr::select(model, location, target, target_end_date, quantile, prediction, true_value) %>%
#       dplyr::filter(!is.na(prediction)),
#     by = observation_cols,
#     summarise_by = c(observation_cols),
#     interval_score_arguments = list(weigh = TRUE, count_median_twice = FALSE))

#   su_rel_wis <- scoringutils::pairwise_comparison(su_scores, baseline = "Covid19Sim-Simulator")

#   su_rel_wis_matrix <- su_rel_wis %>%
#     dplyr::select(model, compare_against, mean_scores_ratio) %>%
#     dplyr::mutate(model = factor(model, levels = sort(unique(model))), compare_against = factor(compare_against, levels = sort(unique(compare_against)))) %>%
#     tidyr::pivot_wider(names_from = "compare_against", values_from = "mean_scores_ratio") %>%
#     as.data.frame()
#   rownames(su_rel_wis_matrix) <- su_rel_wis_matrix$model
#   su_rel_wis_matrix <- as.matrix(su_rel_wis_matrix %>% dplyr::select(-model))
#   su_rel_wis_matrix[order(rownames(su_rel_wis_matrix)), order(colnames(su_rel_wis_matrix))]

#   merged_scores <- su_rel_wis %>%
#     dplyr::distinct(model, scaled_rel_skill) %>%
#     dplyr::left_join(rel_wis, by = "model")
#   all(abs(merged_scores$scaled_rel_skill - merged_scores$rel_wis) < 1e-8)

#   expect_equal(actual, expected)
# })
