# load packages
library(covidData)
library(covidHubUtils)
library(covidEnsembles)
library(tidyverse)
library(gridExtra)
library(knitr)
library(here)

setwd(here())

knitr::opts_chunk$set(echo = FALSE, cache.lazy = FALSE)
options(width = 200)
options(error = recover)

# Dates of forecast submission for forecasts included in this analysis
first_forecast_date <- lubridate::ymd("2020-07-27")
last_forecast_date <- lubridate::ymd("2021-07-05")
num_forecast_weeks <- as.integer(last_forecast_date -
                         first_forecast_date) / 7 + 1

forecast_dates <- first_forecast_date +
  seq(from = 0, length = num_forecast_weeks) * 7

#for (spatial_scale in c("national", "state", "state_national", "county")) {
for (spatial_scale in c("state_national")) {
  if (spatial_scale %in% c("national", "state_national")) {
    response_vars <- "inc_hosp"
  } else if (spatial_scale == "state") {
#    response_vars <- c("cum_death", "inc_death", "inc_case", "inc_hosp")
    response_vars <- c("inc_death", "inc_case")
  } else if (spatial_scale == "county") {
    response_vars <- "inc_case"
  }
  for (response_var in response_vars) {
    all_scores <- calc_retrospective_ensemble_scores(
      submissions_root = "code/application/retrospective-qra-comparison/retrospective-forecasts/",
      forecast_dates = forecast_dates,
      spatial_scales = spatial_scale,
    #  spatial_scales = "state",
    #  spatial_scales = c("national", "state", "state_national", "county"),
      response_vars = response_var,
    #  response_vars = "inc_case",
    #  response_vars = NULL,
    #  response_vars = c("inc_case", "inc_death", "cum_death", "inc_hosp"),
      truth_as_of = Sys.Date() - 1
    )

    unique_models <- unique(all_scores$model)
    model_cases <- purrr::map_dfr(
      unique_models,
      function(model) {
        parse_model_case(model) %>% mutate(model = model)
      }
    ) %>%
      dplyr::mutate(
        top_models = ifelse(
          is.na(top_models) | as.character(top_models) %in% c("0", "all"),
          "all",
          paste0('top_', top_models)))

    all_scores <- all_scores %>%
      dplyr::filter(forecast_date >= "2020-07-27") %>%
      dplyr::left_join(model_cases, by = "model")

    saveRDS(
      all_scores,
      paste0("code/application/retrospective-qra-comparison/analyses/retrospective-eval-hosp-exclusions/retrospective_scores-",
        spatial_scale, "-",
        response_var, ".rds")
    )
  }
}


summarized_scores <- all_scores %>%
  dplyr::group_by(model, target_variable, forecast_date, horizon) %>%
  dplyr::summarize(
    mae = mean(abs_error),
    mwis = mean(wis),
    coverage_50 = mean(coverage_50),
    coverage_95 = mean(coverage_95)
  )

unique_models <- unique(summarized_scores$model)
model_cases <- purrr::map_dfr(
  unique_models,
  function(model) {
    parse_model_case(model) %>% mutate(model = model)
  }
) %>%
  dplyr::mutate(
    top_models = ifelse(
      is.na(top_models) | as.character(top_models) %in% c("0", "all"),
      "all",
      paste0('top_', top_models)))

summarized_scores <- summarized_scores %>%
  dplyr::left_join(model_cases, by = "model") %>%
  dplyr::mutate(
    model_brief = paste0(combine_method, "-", window_size, "-", top_models)
  )

all_scores %>%
#  dplyr::filter(target_variable == "inc case") %>%
  dplyr::filter(forecast_date >= "2020-07-27") %>%
#  dplyr::left_join(model_cases, by = "model") %>%
  dplyr::mutate(
    top_models = ifelse(top_models == "top_0", "all", top_models),
    horizon_group = ifelse(horizon_group == "all", "all", "by_horizon"),
    model_brief = paste0(combine_method, "-", quantile_groups, "-", window_size, "-", top_models, "-", horizon_group)
  ) %>% #pull(model_brief) %>% unique()
#  dplyr::filter(model_brief %in% models_to_keep) %>%
  dplyr::count(model_brief, target_variable) %>%
  tidyr::pivot_wider(names_from = "target_variable", values_from = "n") %>%
  as.data.frame()

all_scores %>%
  dplyr::filter(target_variable == "inc death") %>%
#  dplyr::left_join(model_cases, by = "model") %>%
  dplyr::mutate(
    model_brief = paste0(combine_method, "-", quantile_groups, "-", window_size, "-", top_models)
  ) %>%
#  dplyr::filter(model_brief %in% models_to_keep) %>%
  dplyr::count(model_brief, forecast_date) %>%
  tidyr::pivot_wider(names_from = "forecast_date", values_from = "n") %>%
  as.data.frame()
# for inc death, all problems are on or before 2020-07-20; can subset to 2020-07-27 or greater


# MAE
ggplot(data = summarized_scores %>% filter(target_variable == "inc case")) +
  geom_line(mapping = aes(x = forecast_date, y = mae, color = model_brief, linetype = model_brief)) +
  facet_wrap( ~ horizon) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  ggtitle("Inc Case") +
  theme_bw()

ggplot(data = summarized_scores %>% filter(target_variable == "inc hosp")) +
  geom_line(mapping = aes(x = forecast_date, y = mae, color = model_brief, linetype = model_brief)) +
  facet_wrap( ~ horizon) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  ggtitle("Inc Hosp") +
  theme_bw()

ggplot(data = summarized_scores %>% filter(target_variable == "inc death")) +
  geom_line(mapping = aes(x = forecast_date, y = mae, color = model_brief, linetype = model_brief)) +
  facet_wrap( ~ horizon) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  ggtitle("Inc Death") +
  theme_bw()

# WIS
models_to_keep <- c("median-0-all", "median-4-top_10", "median-4-top_5", "median-full_history-top_10", "median-full_history-top_5", "convex-full_history-all")
models_to_keep <- c("median-0-all",
  paste0("median-", rep(c("4", "8", "12", "full_history"), each = 3), "-top_", rep(c("5", "10", "15"), times = 4))
)
models_to_keep <- c("median-0-all",
  paste0("median-", rep(c("4", "8", "12", "full_history"), each = 1),
    "-top_", rep(c("5"), times = 4)),
  paste0("convex_median-", rep(c("4", "8", "12", "full_history"), each = 1),
    "-top_0")
)
models_to_keep <- c("median-0-all", "median-12-top_5", "rel_wis_weighted_median-12-top_5", "convex_median-12-top_5")

p_wis_case <- ggplot(data = summarized_scores %>%
    dplyr::filter(
      quantile_groups == "per_model",
      top_models != "top_15",
      !(combine_method == "rel_wis_weighted_median" & window_size %in% c("4", "full_history"))
    ) %>%
    filter(target_variable == "inc case", model_brief %in% models_to_keep)) +
  geom_line(mapping = aes(x = forecast_date, y = mwis, color = model_brief, linetype = model_brief)) +
  facet_grid(horizon ~ target_variable, scales = "free_y") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  ggtitle("Inc Case") +
  theme_bw()

# p_wis_hosp <- ggplot(data = summarized_scores %>%
#     filter(target_variable == "inc hosp", model_brief %in% models_to_keep)) +
#   geom_line(mapping = aes(x = forecast_date, y = mwis, color = model_brief, linetype = model_brief)) +
#   facet_grid(horizon ~ target_variable, scales = "free_y") +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
#   ggtitle("Inc Hosp") +
#   theme_bw()

# p_wis_hosp2 <- ggplot(data = summarized_scores %>%
#     filter(target_variable == "inc hosp", model_brief %in% models_to_keep)) +
#   geom_line(mapping = aes(x = forecast_date, y = mwis, color = model_brief, linetype = model_brief)) +
#   facet_wrap( ~ horizon, scales = "free_y") +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
#   ggtitle("Inc Hosp") +
#   theme_bw()


p_wis_death <- ggplot(data = summarized_scores %>%
    dplyr::filter(
      quantile_groups == "per_model",
      top_models != "top_15",
      !(combine_method == "rel_wis_weighted_median" & window_size %in% c("4", "full_history"))
    ) %>%
    filter(target_variable == "inc death", model_brief %in% models_to_keep)) +
  geom_line(mapping = aes(x = forecast_date, y = mwis, color = model_brief, linetype = model_brief)) +
  facet_grid(horizon ~ target_variable, scales = "free_y") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  ggtitle("Inc Death") +
  theme_bw()


#png('code/application/retrospective-qra-comparison/analyses/retrospective-eval-weighted-median-renormalization/wis_by_date_case.png', width=1000, height=500)
pdf('code/application/retrospective-qra-comparison/analyses/retrospective-eval-weighted-median-renormalization/wis_by_date_case.pdf', width=10, height=6)
p_wis_case
dev.off()

pdf('code/application/retrospective-qra-comparison/analyses/retrospective-eval-weighted-median-renormalization/wis_by_date_death.pdf', width=10, height=6)
p_wis_death
dev.off()

grid.arrange(
  p_wis_case,
#  p_wis_hosp,
  p_wis_death,
  nrow = 1)

summarized_scores_by_horizon_model <- all_scores %>%
  dplyr::group_by(model, target_variable, horizon) %>%
  dplyr::summarize(
    mae = mean(abs_error),
    mwis = mean(wis),
    coverage_50 = mean(coverage_50),
    coverage_95 = mean(coverage_95)
  ) %>%
  dplyr::left_join(model_cases, by = "model") %>%
  dplyr::mutate(
    model_brief = paste0(combine_method, "-", window_size, "-", top_models)
  ) %>%
  dplyr::filter(model_brief %in% models_to_keep)

ggplot(data = summarized_scores %>%
    dplyr::filter(!(model_brief == "convex_median-full_history" & target_variable == "inc death"),
      model_brief %in% models_to_keep)) +
  geom_boxplot(mapping = aes(x = model_brief, y = mwis)) +
  geom_point(
    data = summarized_scores_by_horizon_model %>%
      dplyr::filter(!(model_brief == "convex_median-full_history" & target_variable == "inc death")),
    mapping = aes(x = model_brief, y = mwis), shape = "+", size = 8) +
  facet_grid(target_variable ~ horizon, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


scores_base <- summarized_scores %>%
  dplyr::ungroup() %>%
  dplyr::filter(model_brief == "median-0-all") %>%
  dplyr::transmute(target_variable, forecast_date, horizon, base_mwis = mwis)

scores_others <- summarized_scores %>%
  dplyr::ungroup() %>%
  dplyr::filter(model_brief != "median-0-all", model_brief %in% models_to_keep) %>%
  dplyr::select(model_brief, target_variable, forecast_date, horizon, mwis) %>%
  dplyr::left_join(scores_base, by = c("target_variable", "forecast_date", "horizon")) %>%
  dplyr::mutate(wis_diff_unweighted_median = mwis - base_mwis)

p_case <- ggplot(data = scores_others %>% filter(target_variable == "inc case")) +
  geom_boxplot(mapping = aes(x = model_brief, y = wis_diff_unweighted_median)) +
  facet_grid(horizon ~ target_variable, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p_death <- ggplot(data = scores_others %>% filter(target_variable == "inc death")) +
  geom_boxplot(mapping = aes(x = model_brief, y = wis_diff_unweighted_median)) +
  facet_grid(horizon ~ target_variable, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(p_case, p_death, ncol = 2)

scores_others %>% filter(target_variable == "inc case") %>%
  group_by(model_brief)


models_to_keep <- c("median-0-all",
  paste0("median-", rep(c("4", "8", "12", "full_history"), each = 2),
    "-top_", rep(c("5", "10"#,
    # "15"
    ), times = 4)),
  paste0("convex_median-", rep(c("4", "8", "12", "full_history"), each = 3),
    rep(c("-all", "-top_5", "-top_10"#,
    # "-top_15"
    ), times = 4))
)

# investigate/correct missing forecasts for some location/date pairs for incident deaths


overall_mean_scores <- all_scores %>%
  dplyr::left_join(model_cases, by = "model") %>%
  dplyr::filter(
    quantile_groups == "per_model",
    !(combine_method == "rel_wis_weighted_median" & window_size %in% c("4", "full_history"))
  ) %>%
  dplyr::filter(target_variable != "inc hosp", forecast_date >= "2020-07-27") %>%
  dplyr::mutate(
    model_brief = paste0(combine_method, "-", window_size, "-", top_models)
  ) %>%
#  dplyr::filter(model_brief %in% models_to_keep) %>%
  dplyr::group_by(model_brief, target_variable) %>%
  dplyr::summarize(
    mae = mean(abs_error),
    mwis = mean(wis),
    coverage_50 = mean(coverage_50),
    coverage_95 = mean(coverage_95)
  ) %>%
  dplyr::arrange(mwis) %>%
  as.data.frame()

model_levels <- tidyr::expand_grid(
  window_size = c("4", "8", "12", "full_history"),
  combine_method = c("ew", "median", "rel_wis_weighted_median", "mean_weights_weighted_median"),
  top_models = c("top_5", "top_10", "all")
) %>%
  dplyr::mutate(model_brief = paste(combine_method, window_size, top_models, sep = "-")) %>%
  dplyr::pull(model_brief)

combine_method_and_top_model_levels <- tidyr::expand_grid(
  combine_method = rev(c("ew", "median", "convex", "convex_median", "rel_wis_weighted_median", "mean_weights_weighted_median")),
  top_models = c("top_5", "top_10", "all")
) %>%
  dplyr::mutate(model_brief = paste(combine_method, top_models, sep = "-")) %>%
  dplyr::pull(model_brief)

overall_mean_scores <- all_scores %>%
  dplyr::filter(
#    target_variable != "inc hosp",
    forecast_date >= "2020-07-27"#,
#    location != "39"
    ) %>%
  dplyr::left_join(model_cases, by = "model") %>%
  dplyr::filter(
    quantile_groups == "per_model",
    top_models != "top_15"#
#    !(combine_method == "rel_wis_weighted_median" & window_size %in% c("4", "full_history"))
  ) %>%
  dplyr::mutate(
    top_models = factor(
      ifelse(is.na(top_models) | top_models == "top_0", "all", top_models),
      levels = c("top_5", "top_10",
      # "top_15",
      "all")),
    model_brief = paste0(combine_method, "-", window_size, "-", top_models)
  ) %>%
#  dplyr::filter(model_brief %in% models_to_keep) %>%
  dplyr::group_by(combine_method, window_size, top_models, target_variable) %>%
  dplyr::summarize(
    mae = mean(abs_error),
    mwis = mean(wis),
    coverage_50 = mean(coverage_50),
    coverage_95 = mean(coverage_95)
  ) %>%
  dplyr::mutate(
    window_size = factor(window_size, levels = c("0", "4", "8", "12", "full_history")),
    combine_method_and_top_models = factor(
      paste0(combine_method, '-', top_models),
      levels = combine_method_and_top_model_levels
    )
  )# %>%
#  dplyr::filter(!is.na(combine_method_and_top_models))
#unique(overall_mean_scores$model_brief)

png('code/application/retrospective-qra-comparison/analyses/retrospective-eval-weighted-median-renormalization/wis_heatmap.png', width=1000, height=500)
p_case <- ggplot(overall_mean_scores %>% filter(target_variable == "inc case")) +
  geom_raster(mapping = aes(x = window_size, y = combine_method_and_top_models, fill = mwis)) +
  geom_label(mapping = aes(x = window_size, y = combine_method_and_top_models, label = round(mwis, 1))) +
  theme_bw() +
  ggtitle("inc case")

p_hosp <- ggplot(overall_mean_scores %>% filter(target_variable == "inc hosp")) +
  geom_raster(mapping = aes(x = window_size, y = combine_method_and_top_models, fill = mwis)) +
  geom_label(mapping = aes(x = window_size, y = combine_method_and_top_models, label = round(mwis, 1))) +
  theme_bw() +
  ggtitle("inc hosp")

p_death <- ggplot(overall_mean_scores %>% filter(target_variable == "inc death")) +
  geom_raster(mapping = aes(x = window_size, y = combine_method_and_top_models, fill = mwis)) +
  geom_label(mapping = aes(x = window_size, y = combine_method_and_top_models, label = round(mwis, 1))) +
  theme_bw() +
  ggtitle("inc death")

grid.arrange(
  p_case,
  p_death,
  nrow = 1)
dev.off()


png('code/application/retrospective-qra-comparison/analyses/retrospective-eval-weighted-median-renormalization/coverage_95_heatmap.png', width=1000, height=500)
p_case <- ggplot(overall_mean_scores %>% filter(target_variable == "inc case")) +
  geom_raster(mapping = aes(x = window_size, y = combine_method_and_top_models, fill = coverage_95)) +
  geom_label(mapping = aes(x = window_size, y = combine_method_and_top_models, label = round(coverage_95, 3))) +
  theme_bw() +
  ggtitle("inc case")

p_death <- ggplot(overall_mean_scores %>% filter(target_variable == "inc death")) +
  geom_raster(mapping = aes(x = window_size, y = combine_method_and_top_models, fill = coverage_95)) +
  geom_label(mapping = aes(x = window_size, y = combine_method_and_top_models, label = round(coverage_95, 3))) +
  theme_bw() +
  ggtitle("inc death")

grid.arrange(p_case, p_death, ncol = 2)
dev.off()

png('code/application/retrospective-qra-comparison/analyses/retrospective-eval-weighted-median-renormalization/mae_heatmap.png', width=1000, height=500)
p_case <- ggplot(overall_mean_scores %>% filter(target_variable == "inc case")) +
  geom_raster(mapping = aes(x = window_size, y = combine_method_and_top_models, fill = mae)) +
  geom_label(mapping = aes(x = window_size, y = combine_method_and_top_models, label = round(mae, 1))) +
  theme_bw() +
  ggtitle("inc case")

p_death <- ggplot(overall_mean_scores %>% filter(target_variable == "inc death")) +
  geom_raster(mapping = aes(x = window_size, y = combine_method_and_top_models, fill = mae)) +
  geom_label(mapping = aes(x = window_size, y = combine_method_and_top_models, label = round(mae, 1))) +
  theme_bw() +
  ggtitle("inc death")

grid.arrange(p_case, p_death, ncol = 2)
dev.off()
