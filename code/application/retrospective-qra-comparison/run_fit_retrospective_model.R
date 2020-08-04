library(tidyr)
library(dplyr)
library(lubridate)
library(doParallel)

registerDoParallel(cores = 6)

output_path <- 'code/application/retrospective-qra-comparison/log/'


# basic approaches
analysis_combinations <- tidyr::expand_grid(
  response_var = c('cum_death', 'inc_death'),
  forecast_week_end_date = as.character(
    lubridate::ymd('2020-05-09') + seq(from = 0, length = 11)*7),
  intercept = c('FALSE', 'TRUE'),
  constraint = c('ew', 'convex', 'positive'),
  quantile_group_str = c('per_model', '3_groups', 'per_quantile'),
  missingness = c('mean_impute', 'by_location_group'),
  window_size = c(0, 3:5),
  do_standard_checks = c('FALSE', 'TRUE'),
  do_baseline_check = c('FALSE', 'TRUE')
) %>%
  dplyr::filter(
    (intercept == 'FALSE' & constraint == 'ew' & quantile_group_str == 'per_model' & missingness == 'by_location_group') |
    (window_size > 0 & intercept == 'FALSE' & constraint == 'convex') |
    (window_size > 0 & intercept == 'TRUE' & constraint == 'positive'),
    !(do_standard_checks & do_baseline_check)
  )


# analysis_combinations <- analysis_combinations %>%
#   dplyr::filter(
#     do_baseline_check == 'TRUE',
#     (window_size %in% c(0, 3, 4) & constraint == 'ew' & do_standard_checks == 'FALSE') |
#     (window_size %in% c(3, 4) & intercept == 'FALSE' & constraint == 'convex' & quantile_group_str == '3_groups' & do_standard_checks == 'FALSE') |
#     (window_size %in% c(3, 4) & intercept == 'TRUE' & constraint == 'positive' & quantile_group_str == '3_groups' & do_standard_checks == 'FALSE')
#   )

foreach(row_ind = seq_len(nrow(analysis_combinations))) %dopar% {
#foreach(row_ind = seq_len(2)) %dopar% {
  response_var <- analysis_combinations$response_var[row_ind]
  forecast_week_end_date <- analysis_combinations$forecast_week_end_date[row_ind]
  intercept <- analysis_combinations$intercept[row_ind]
  constraint <- analysis_combinations$constraint[row_ind]
  quantile_group_str <- analysis_combinations$quantile_group_str[row_ind]
  missingness <- analysis_combinations$missingness[row_ind]
  window_size <- analysis_combinations$window_size[row_ind]
  do_standard_checks <- analysis_combinations$do_standard_checks[row_ind]
  do_baseline_check <- analysis_combinations$do_baseline_check[row_ind]

  run_cmd <- paste0(
    "R CMD BATCH --vanilla \'--args ",
    response_var, " ",
    forecast_week_end_date, " ",
    intercept, " ",
    constraint, " ",
    missingness, " ",
    quantile_group_str, " ",
    window_size, " ",
    do_standard_checks, " ",
    do_baseline_check,
    "\' code/application/retrospective-qra-comparison/fit_retrospective_model.R ",
    output_path, "output-", response_var, "-", forecast_week_end_date, '-',
    intercept, '-', constraint, '-', missingness, '-', quantile_group_str,
    '-', window_size, '-', do_standard_checks, '-', do_baseline_check, ".Rout")

  system(run_cmd)
}

#
# # rescaled convex approach, one weight per model
# analysis_combinations <- tidyr::expand_grid(
#   response_var = c('cum_death', 'inc_death'),
#   forecast_week_end_date = as.character(
#     lubridate::ymd("2020-04-18") + seq(from = 0, length = 8)*7),
#   ensemble_method = 'rescaled_convex',
#   window_size = seq(from = 1, to = 4)
# ) %>%
#   dplyr::filter((window_size == 0 & ensemble_method == 'ew') | window_size > 0)
#
#
# foreach(row_ind = seq_len(nrow(analysis_combinations))) %dopar% {
#   #foreach(row_ind = seq_len(2)) %dopar% {
#   response_var <- analysis_combinations$response_var[row_ind]
#   forecast_week_end_date <- analysis_combinations$forecast_week_end_date[row_ind]
#   ensemble_method <- analysis_combinations$ensemble_method[row_ind]
#   window_size <- analysis_combinations$window_size[row_ind]
#
#   run_cmd <- paste0(
#     "R CMD BATCH --vanilla \'--args ",
#     response_var, " ",
#     forecast_week_end_date, " ",
#     ensemble_method, " ",
#     window_size,
#     "\' code/application/retrospective-qra-comparison/fit_retrospective_model.R ",
#     output_path, "output-", response_var, "-", forecast_week_end_date, '-',
#     ensemble_method, '-', window_size, ".Rout")
#
#   system(run_cmd)
# }
#
