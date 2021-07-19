# load packages
library(covidData)
library(covidHubUtils)
library(covidEnsembles)
library(tidyverse)
library(gridExtra)
library(knitr)
library(here)

setwd(here())

spatial_scale <- "state_national"
response_var <- "inc_hosp"

all_scores <- readRDS(
  paste0("code/application/retrospective-qra-comparison/analyses/retrospective-eval-hosp-exclusions/retrospective_scores-",
    spatial_scale, "-",
    response_var, ".rds")
)

all_scores <- all_scores %>%
  dplyr::mutate(
      model_brief = do_sd_check,
      location_type = ifelse(location == "US", "national", "state")
  )

all_scores_base <- all_scores %>%
  dplyr::ungroup() %>%
  dplyr::filter(model_brief == "exclude_both") %>%
  dplyr::mutate(base_wis = wis) %>%
  dplyr::select(-wis, -model_brief)

all_scores_others <- all_scores %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, location, horizon, forecast_date, target_end_date, wis) %>%
  dplyr::left_join(all_scores_base, by = c("location", "horizon", "forecast_date", "target_end_date")) %>%
  dplyr::mutate(
    wis_diff = wis - base_wis,
  )

by_model_means <- all_scores %>%
  dplyr::group_by(model_brief, location_type) %>%
  dplyr::summarize(
    mae = mean(abs_error),
    mwis = mean(wis),
    coverage_50 = mean(coverage_50),
    coverage_95 = mean(coverage_95)
  )

by_model_means_base <- by_model_means %>%
  dplyr::ungroup() %>%
  dplyr::filter(model_brief == "exclude_both") %>%
  dplyr::transmute(location_type, base_mwis = mwis)

by_model_means_others <- by_model_means %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, location_type, mwis) %>%
  dplyr::left_join(by_model_means_base, by = c("location_type")) %>%
  dplyr::mutate(
    wis_diff = mwis - base_mwis,
  )


overall_means <- all_scores %>%
  dplyr::group_by(model_brief, location_type, horizon) %>%
  dplyr::summarize(
    mae = mean(abs_error),
    mwis = mean(wis),
    coverage_50 = mean(coverage_50),
    coverage_95 = mean(coverage_95)
  )

overall_means_base <- overall_means %>%
  dplyr::ungroup() %>%
  dplyr::filter(model_brief == "exclude_both") %>%
  dplyr::transmute(location_type, horizon, base_mwis = mwis)

overall_means_others <- overall_means %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, location_type, horizon, mwis) %>%
  dplyr::left_join(overall_means_base, by = c("location_type", "horizon")) %>%
  dplyr::mutate(
    wis_diff = mwis - base_mwis,
  )

summarized_scores <- all_scores %>%
  dplyr::group_by(model_brief, location_type, forecast_date, horizon) %>%
  dplyr::summarize(
    mae = mean(abs_error),
    mwis = mean(wis),
    coverage_50 = mean(coverage_50),
    coverage_95 = mean(coverage_95)
  )

scores_base <- summarized_scores %>%
  dplyr::ungroup() %>%
  dplyr::filter(model_brief == "exclude_both") %>%
  dplyr::transmute(forecast_date, location_type, horizon, base_mwis = mwis)

scores_others <- summarized_scores %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, forecast_date, location_type, horizon, mwis) %>%
  dplyr::left_join(scores_base, by = c("forecast_date", "location_type", "horizon")) %>%
  dplyr::mutate(
    wis_diff = mwis - base_mwis
  )

p_wis_boxplots <- ggplot(
    data = all_scores_others
  ) +
  geom_hline(yintercept = 0) +
  geom_boxplot(mapping = aes(
    x = model_brief,
    y = wis_diff)) +
  geom_point(
    data = by_model_means_others,
    mapping = aes(
      x = model_brief,
      y = wis_diff),
    shape = "+", size = 5, color = "orange"
  ) +
#      scale_y_log10() +
#      facet_wrap( ~ window_size, nrow = 1, scales = "free_x") +
  facet_wrap( ~ location_type, scales = "free") +
  ylab("Mean WIS for Method - Mean WIS for Exclude Both") +
  ggtitle(paste0(response_var, ", ", spatial_scale)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))
pdf(
  paste0(
    'code/application/retrospective-qra-comparison/analyses/retrospective-eval-hosp-exclusions/wis_boxplots',
    "_", response_var, "_", spatial_scale, '.pdf'),
  width=14, height=10)
print(p_wis_boxplots)
dev.off()


p_wis_boxplots <- ggplot(
    data = scores_others
  ) +
  geom_hline(yintercept = 0) +
  geom_boxplot(mapping = aes(
    x = model_brief,
    y = wis_diff)) +
  # geom_point(
  #   data = overall_means_others,
  #   mapping = aes(
  #     x = model_brief,
  #     y = wis_diff),
  #   shape = "+", size = 5
  # ) +
  geom_point(
    data = by_model_means_others,
    mapping = aes(
      x = model_brief,
      y = wis_diff),
    shape = "+", size = 5, color = "orange"
  ) +
#      scale_y_log10() +
#      facet_wrap( ~ window_size, nrow = 1, scales = "free_x") +
#      facet_grid(location_type ~ horizon, scales = "free") +
  facet_wrap( ~ location_type, scales = "free") +
  ylab("Mean WIS for Method - Mean WIS for Exclude Both") +
  ggtitle(paste0(response_var, ", ", spatial_scale)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))
pdf(
  paste0(
    'code/application/retrospective-qra-comparison/analyses/retrospective-eval-hosp-exclusions/wis_boxplots',
    "_", response_var, "_", spatial_scale, '_by_horizon.pdf'),
  width=14, height=10)
print(p_wis_boxplots)
dev.off()

scores_over_time <- all_scores %>%
  dplyr::group_by(model_brief, location_type, forecast_date, horizon) %>%
  dplyr::summarize(mwis = mean(wis), .groups="drop")



p_wis_over_time_national <- ggplot(data = scores_over_time %>%
  dplyr::filter(location_type == "national", horizon %in% c(1, 7, 14, 21, 28))) +
  geom_line(
    mapping = aes(x = forecast_date, y = mwis, color = model_brief, linetype = model_brief)
  ) +
  facet_wrap(~ horizon, ncol = 1) +
  ggtitle("national") +
  theme_bw()

p_wis_over_time_state <- ggplot(data = scores_over_time %>%
  dplyr::filter(location_type == "state", horizon %in% c(1, 7, 14, 21, 28))) +
  geom_line(
    mapping = aes(x = forecast_date, y = mwis, color = model_brief, linetype = model_brief)
  ) +
  facet_wrap(~ horizon, ncol = 1) +
  ggtitle("state") +
  theme_bw()

pdf(
  paste0(
    'code/application/retrospective-qra-comparison/analyses/retrospective-eval-hosp-exclusions/wis_lineplots',
    "_", response_var, "_", spatial_scale, '_over_time_by_horizon.pdf'),
  width=14, height=10)
print(p_wis_over_time_national)
print(p_wis_over_time_state)
dev.off()
