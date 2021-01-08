library(dplyr)
library(ggplot2)
library("pipeR")
library("data.table")
library(covidEnsembles)

## debug_weights_aheads_folder = "covid-19-iif-blog-post-data/debug_quantgen_weights/ensemble3_8times_cdc_matched_verification_verificationrun/jhu-csse_deaths_incidence_num/epiweek/state/200"
debug_weights_aheads_folder = "covid-19-iif-blog-post-data/debug_quantgen_weights/ensemble3_4times_cdc_matched_verification_verificationrun/jhu-csse_deaths_incidence_num/epiweek/state/200"
debug_weights_filenames = list.files(debug_weights_aheads_folder, recursive=TRUE)


# compare estimation inputs

## acquire estimation inputs for delphi
debug_qf = debug_weights_filenames %>>%
  setNames(paste0(dirname(.),"/",basename(.))) %>>%
  lapply(function(filename) {
    readRDS(file.path(debug_weights_aheads_folder, filename))
  }) %>>%
  lapply(function(info) {
    info[["extra_info"]][["qf"]]
  })

qarr_train_delphi <- debug_qf[[1]]$forecasts
forecasts_train_delphi <- purrr::map_dfr(
  dimnames(qarr_train_delphi)[[1]],
  function(dateloc) {
    dim1_ind <- which(dimnames(qarr_train_delphi)[[1]] == dateloc)
    qarr_train_delphi[dim1_ind, , ] %>%
      as.data.frame() %>%
      dplyr::mutate(
        model = rownames(.),
        dateloc = dateloc
      ) %>%
      tidyr::pivot_longer(
        cols = colnames(qarr_train_delphi[dim1_ind, , ]),
        names_to = "quantile",
        values_to = "value")
  }) %>%
  tidyr::separate(
    col = "dateloc",
    into = c("date", "location"),
    sep = ";"
  )

y_train_delphi <- data.frame(
  y = unname(debug_qf[[1]]$actual),
  dateloc = names(debug_qf[[1]]$actual)
) %>%
  tidyr::separate(
    col = "dateloc",
    into = c("date", "location"),
    sep = ";"
  )

## acquire estimation inputs for covidhub
model_fit <- "https://github.com/reichlab/covidEnsembles/blob/master/code/application/retrospective-qra-comparison/misc_variations/replicate_cmu/retrospective-fits/state/inc_death-forecast_week_2020-10-05-intercept_FALSE-combine_method_convex-missingness_impute-quantile_groups_3_groups-window_size_4-check_missingness_by_target_TRUE-do_standard_checks_FALSE-do_baseline_check_FALSE.rds?raw=true" %>%
  url("rb") %>%
  readRDS()

y_train_covidhub <- attr(model_fit$location_groups$qfm_train[[1]], "row_index") %>%
  transmute(
    location,
    date = as.character(forecast_week_end_date + 2),
    y = model_fit$location_groups$y_train[[1]],
  )

forecasts_train_covidhub <- model_fit$location_groups$qfm_train[[1]] %>%
  as.data.frame() %>%
  dplyr::transmute(
    model, date = as.character(forecast_week_end_date + 2),
    location, quantile = as.character(quantile),
    value
  )

### copied the following code from https://github.com/reichlab/covidEnsembles/blob/382f275fe5178ffae539bbb056e8fefddbd0f4f2/R/qra_fit.R#L738
### it converts the qfm_train object stored as part of the model fit to the 3d array used as input to quantgen::quantile_ensemble
  constraint <- "convex"
  qfm_train <- model_fit$location_groups$qfm_train[[1]]
  qfm_test <- model_fit$location_groups$qfm_test[[1]]

  # unpack and process arguments
  col_index <- attr(qfm_train, 'col_index')
  model_col <- attr(qfm_train, 'model_col')
  quantile_name_col <- attr(qfm_train, 'quantile_name_col')

  models <- unique(col_index[[model_col]])
  num_models <- length(models)

  quantiles <- sort(unique(col_index[[quantile_name_col]]))
  num_quantiles <- length(quantiles)

  quantgen_intercept <- intercept

  if(constraint == 'convex') {
    quantgen_nonneg = TRUE
    quantgen_unit_sum = TRUE
  } else if(constraint == 'positive') {
    quantgen_nonneg = TRUE
    quantgen_unit_sum = FALSE
  } else if(constraint == 'unconstrained') {
    quantgen_nonneg = FALSE
    quantgen_unit_sum = FALSE
  }

  # reformat training set predictive quantiles from component models as 3d
  # array in format required for quantgen package
  qarr_train <- unclass(qfm_train)
  dim(qarr_train) <- c(nrow(qarr_train), num_quantiles, num_models)
  qarr_train <- aperm(qarr_train, c(1, 3, 2))

  qarr_test <- unclass(qfm_test)
  dim(qarr_test) <- c(nrow(qarr_test), num_quantiles, num_models)
  qarr_test <- aperm(qarr_test, c(1, 3, 2))

  n_train <- dim(qarr_train)[1]
  n_test <- dim(qarr_test)[1]

  q0 <- array(dim = c(n_train + n_test, num_models, num_quantiles))
  q0[seq_len(n_train), , ] <- qarr_train
  q0[n_train + seq_len(n_test), , ] <- qarr_test

  qarr_train_covidhub <- qarr_train

  # manually assembled in case of errors in the array reshuffling above...
  new_qarr_train <- array(NA, dim = c(nrow(qfm_train), num_models, num_quantiles))
  col_index <- attr(qfm_train, "col_index")
  for (m in seq_len(num_models)) {
    qfm_cols <- which(col_index$model == unique(col_index$model)[m])
    new_qarr_train[, m, ] <- unclass(qfm_train)[, qfm_cols]
  }

## check equality of inputs
### observed responses match up
y_train_combined <- y_train_delphi %>%
  dplyr::full_join(y_train_covidhub, by = c("location", "date"))
all.equal(y_train_combined[["y.x"]], y_train_combined[["y.y"]])

### forecasts match up
forecasts_train_combined <- forecasts_train_delphi %>%
  dplyr::full_join(forecasts_train_covidhub, by = c("location", "date", "model", "quantile"))
all.equal(forecasts_train_combined[["value.x"]], forecasts_train_combined[["value.y"]])

all.equal(unname(qarr_train_delphi), unname(qarr_train_covidhub))
all.equal(
  which(is.na(qarr_train_delphi)),
  which(is.na(qarr_train_covidhub))
)
apply(qarr_train_delphi, 2, function(x) sum(is.na(x)))
apply(qarr_train_covidhub, 2, function(x) sum(is.na(x)))

delphi_reorder_inds <- names(debug_qf[[1]]$actual) %>% order()
for (m in seq_len(num_models)) {
  for (l in seq_len(num_quantiles)) {
    print(all.equal(unname(qarr_train_delphi[delphi_reorder_inds, m, l]), qarr_train_covidhub[, m, l]))
  }
}

# compare estimated weights
debug_weights_extract =
  debug_weights_filenames %>>%
  setNames(paste0(dirname(.),"/",basename(.))) %>>%
  lapply(function(filename) {
    readRDS(file.path(debug_weights_aheads_folder, filename))
  }) %>>%
  lapply(function(info) {
    ## data.table(info = list(info))
    setDT(info[["extra_info"]][["effective_weights_df"]])
  }) %>>%
  rbindlist(idcol="ahead/forecast_date") %>>%
  {(.
    [, ahead := as.integer(dirname(`ahead/forecast_date`))]
    [, forecast_date := as.Date(basename(`ahead/forecast_date`))]
    [, `ahead/forecast_date` := NULL]
    [, location := sub("^(.*);(.*)$", "\\2", `Date;Location`)]
    [, `Date;Location` := NULL]
    [, quantile := as.character(Quantile)]
    [] # (removes internal data.table invisible flag)
  )}

weights_covidhub <- "https://github.com/reichlab/covidEnsembles/blob/master/code/application/retrospective-qra-comparison/misc_variations/replicate_cmu/retrospective_weight_estimates.rds?raw=true" %>%
  url("rb") %>%
  readRDS() %>%
  ## dplyr::filter(window_size == 8, location == "01")
  dplyr::filter(window_size == 4, location == "01")

weights_delphi <- debug_weights_extract %>%
    tibble::as_tibble() %>%
    dplyr::filter(location == "01") %>>%
    dplyr::transmute(model = Forecaster, quantile, location, weight = `Fit Weight`)
    ## dplyr::transmute(model = Forecaster, quantile, location, weight = `Effective Weight`)

# weights_covidhub %>%
#   dplyr::filter(quantile %in% c("0.01", "0.025", "0.05", "0.1", "0.15"))

weights <- weights_delphi %>%
  dplyr::transmute(model, quantile, weight_delphi = weight) %>%
  dplyr::left_join(
    weights_covidhub %>%
      dplyr::transmute(model, quantile, weight_covidhub = weight),
    by = c("model", "quantile")
  ) %>%
  dplyr::mutate(
    weight_covidhub = ifelse(is.na(weight_covidhub), 0, weight_covidhub)
  )

weights %>%
  dplyr::filter(quantile %in% c("0.01", "0.5", "0.99")) %>%
  tidyr::pivot_longer(
    cols = c("weight_delphi", "weight_covidhub"), # starts_with("weight_"),
    names_to = "group",
    names_prefix = "weight_",
    values_to = "weight"
  ) %>%
  ggplot(aes(x = model, y = weight, color = group, group = group)) +
    geom_line() +
    geom_point() +
    facet_wrap( ~ quantile, ncol = 1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))




