#' Calculate weighted interval width for multiple intervals at a single value
#' of alpha
#'
#' @param y vector of observations from target distribution
#' @param l lower bound of predictive distribution
#' @param u upper bound of predictive distribution
#' @param alpha the interval [l,u] is a (1 - alpha)*100% predictive interval
#'
#' @return vector of interval scores separately for each element of y
weighted_interval_width <- function(y, l, u, alpha) {
  if(length(alpha) > 1 || alpha < 0.0 || alpha > 1.0) {
    stop("alpha must be a single number between 0 and 1.")
  }

  score = (alpha / 2.0) * (u - l)

  return(score)
}


#' Calculate quantile score, optionally including absolute error,
#' for a parametric distribution.
#'
#' @param y vector of observations from target distribution
#' @param qfm object of class QuantileForecastMatrix
#'
#' @return vector of interval scores separately for each element of x
#'
#' @export
wiw <- function(y, qfm) {
  col_index <- attr(qfm, 'col_index')
  quantile_levels <- as.numeric(col_index[[attr(qfm, 'quantile_name_col')]])

  if(length(unique(quantile_levels)) != length(quantile_levels)) {
    stop('Quantile levels must be unique.  Did you provide a quantile forecast matrix for a single model?')
  }

  # if median is one of the quantiles, keep track of that
  median_ind <- which(quantile_levels == 0.5)

  # determine number K of intervals to score, and
  # initialize score to absolute error if median provided or 0 otherwise
  if(length(median_ind) == 1) {
    K <- (length(quantile_levels) - 1)/2
    score <- abs(y - unclass(qfm)[, median_ind])
  } else {
    K <- length(quantile_levels) / 2
    score <- rep(0.0, length(y))
  }

  # interval scores for each interval
  for(k in seq_len(K)) {
    alpha <- 2*quantile_levels[k]
    score <- score + weighted_interval_width(y, unclass(qfm)[, k], unclass(qfm)[, ncol(qfm)+1-k], alpha)
  }

  score <- score / (K + length(median_ind))

  return(score)
}


#' Calculate interval scores for multiple intervals at a single value of alpha
#'
#' @param y vector of observations from target distribution
#' @param l lower bound of predictive distribution
#' @param u upper bound of predictive distribution
#' @param alpha the interval [l,u] is a (1 - alpha)*100% predictive interval
#'
#' @return vector of interval scores separately for each element of y
weighted_interval_penalty <- function(y, l, u, alpha) {
  if(length(alpha) > 1 || alpha < 0.0 || alpha > 1.0) {
    stop("alpha must be a single number between 0 and 1.")
  }

  score = ifelse(y < l, (l - y), 0.0) +
    ifelse(y > u, (y - u), 0.0)

  return(score)
}


#' Calculate quantile score, optionally including absolute error,
#' for a parametric distribution.
#'
#' @param y vector of observations from target distribution
#' @param qfm object of class QuantileForecastMatrix
#'
#' @return vector of interval scores separately for each element of x
#'
#' @export
wip <- function(y, qfm) {
  col_index <- attr(qfm, 'col_index')
  quantile_levels <- as.numeric(col_index[[attr(qfm, 'quantile_name_col')]])

  if(length(unique(quantile_levels)) != length(quantile_levels)) {
    stop('Quantile levels must be unique.  Did you provide a quantile forecast matrix for a single model?')
  }

  # if median is one of the quantiles, keep track of that
  median_ind <- which(quantile_levels == 0.5)

  # determine number K of intervals to score, and
  # initialize score to absolute error if median provided or 0 otherwise
  if(length(median_ind) == 1) {
    K <- (length(quantile_levels) - 1)/2
    score <- abs(y - unclass(qfm)[, median_ind])
  } else {
    K <- length(quantile_levels) / 2
    score <- rep(0.0, length(y))
  }

  # interval scores for each interval
  for(k in seq_len(K)) {
    alpha <- 2*quantile_levels[k]
    score <- score + weighted_interval_penalty(y, unclass(qfm)[, k], unclass(qfm)[, ncol(qfm)+1-k], alpha)
  }

  score <- score / (K + length(median_ind))

  return(score)
}


#' Calculate interval scores for multiple intervals at a single value of alpha
#'
#' @param y vector of observations from target distribution
#' @param l lower bound of predictive distribution
#' @param u upper bound of predictive distribution
#' @param alpha the interval [l,u] is a (1 - alpha)*100% predictive interval
#'
#' @return vector of interval scores separately for each element of y
interval_score <- function(y, l, u, alpha) {
  if(length(alpha) > 1 || alpha < 0.0 || alpha > 1.0) {
    stop("alpha must be a single number between 0 and 1.")
  }

  score = (alpha / 2.0) * (u - l) +
    ifelse(y < l, (l - y), 0.0) +
    ifelse(y > u, (y - u), 0.0)

  return(score)
}


#' Calculate quantile scores for multiple quantile predictions at a single
#' probability level tau
#'
#' @param y vector of observations from target distribution
#' @param q predictive quantile values
#' @param tau the probability level for the predictive quantile q
#'
#' @return vector of quantile scores separately for each element of q
quantile_score <- function(y, q, tau) {
  if(length(tau) > 1 || tau < 0.0 || tau > 1.0) {
    stop("tau must be a single number between 0 and 1.")
  }

  score <- (as.numeric(y < q) - tau) * (q - y)

  return(score)
}


#' Calculate quantile score, optionally including absolute error,
#' for a representation of a distribution for a single model
#'
#' @param y vector of observations from target distribution
#' @param qfm object of class QuantileForecastMatrix representing forecasts
#' from one model
#'
#' @return vector of interval scores separately for each element of x
#'
#' @export
wis <- function(y, qfm) {
  col_index <- attr(qfm, 'col_index')
  quantile_levels <- as.numeric(col_index[[attr(qfm, 'quantile_name_col')]])

  if(length(unique(quantile_levels)) != length(quantile_levels)) {
    stop('Quantile levels must be unique.  Did you provide a quantile forecast matrix for a single model?')
  }

  # # if median is one of the quantiles, keep track of that
  # median_ind <- which(quantile_levels == 0.5)

  # # determine number K of intervals to score, and
  # # initialize score to absolute error if median provided or 0 otherwise
  # if (length(median_ind) == 1) {
  #   K <- (length(quantile_levels) - 1)/2
  #   score <- 0.5 * abs(y - unclass(qfm)[, median_ind])
  # } else {
  #   K <- length(quantile_levels) / 2
    score <- rep(0.0, length(y))
  # }

  # interval scores for each interval
  # for(k in seq_len(K)) {
  #   alpha <- 2*quantile_levels[k]
  #   score <- score + interval_score(y, unclass(qfm)[, k], unclass(qfm)[, ncol(qfm)+1-k], alpha)
  # }

  # score <- score / (K + length(median_ind)/2)
  for(k in seq_along(quantile_levels)) {
    score <- score + 2 * quantile_score(y, unclass(qfm)[, k], quantile_levels[k])
  }

  score <- score / length(quantile_levels)

  return(score)
}


#' compute wis and contributions to wis from each interval level
#'
#' @param qfm QuantileForecastMatrix with predictions to score
#' @param observed_by_location_target_end_date data frame of observed values
#'
#' @return data frame with column per component of wis
#'
#' @export
get_all_wis_components <- function(
  qfm,
  observed_by_location_target_end_date) {

  row_index <- attr(qfm, 'row_index')

  y_test <- row_index %>%
    dplyr::mutate(
      horizon = as.integer(substr(target, 1, 1)),
      base_target = substr(target, 3, nchar(target)),
      target_end_date = as.character(lubridate::ymd(forecast_week_end_date) + 7*horizon)
    ) %>%
    dplyr::left_join(
      observed_by_location_target_end_date,
      by = c('location', 'target_end_date', 'base_target')
    ) %>%
    pull(observed)

  row_index$wis <- covidEnsembles::wis(y_test, qfm)

  col_index <- attr(qfm, 'col_index')
  for(i in seq_len((nrow(col_index) - 1)/2)) {
    wis_name <- paste0('wis_', format(as.numeric(col_index$quantile[i]) * 2,
                                      nsmall=2, digits=2))
    row_index[[wis_name]] <- covidEnsembles::wis(
      y_test, qfm[, c(i, nrow(col_index) + 1 - i)])
  }

  i <- i + 1
  row_index[['wis_1']] <- abs(y_test - unclass(qfm)[, i])

  return(row_index[!is.na(y_test), ])
}

calc_relative_wis <- function(y, qfm, baseline = "COVIDhub-baseline", agg_method=c("geom_mean", "mean")) {
  agg_method <- match.arg(agg_method)
  col_index <- attr(qfm, 'col_index')
  model_col <- attr(qfm, 'model_col')
  models <- unique(col_index[[model_col]])
  wis_per_model <- matrix(NA, nrow = nrow(qfm), ncol = length(models))

  for (m in seq_along(models)) {
    model <- models[m]
    model_qfm <- qfm[, col_index[[model_col]] == model]
    wis_per_model[, m] <- wis(y, model_qfm)
  }

  pairwise_ratios <- diag(length(models))
  for (m1 in seq_along(models)) {
    for (m2 in seq_len(m1 - 1)) {
      non_na_inds <- which(!is.na(wis_per_model[, m1]) & !is.na(wis_per_model[, m2]))
      pairwise_ratios[m1, m2] <- mean(wis_per_model[non_na_inds, m1]) / mean(wis_per_model[non_na_inds, m2])
      pairwise_ratios[m2, m1] <- 1 / pairwise_ratios[m1, m2]
    }
  }

  rownames(pairwise_ratios) <- models
  if (baseline %in% models) {
    ind_baseline <- which(rownames(pairwise_ratios) == baseline)
  } else {
    ind_baseline <- 1L
  }
  if (agg_method == "geom_mean") {
    geom_mean_ratios <- exp(rowMeans(log(pairwise_ratios[, -ind_baseline]), na.rm = TRUE))
    ratios_baseline2 <- geom_mean_ratios / geom_mean_ratios[ind_baseline]
    model_names <- names(geom_mean_ratios)
  } else if (agg_method == "mean") {
    mean_ratios <- rowMeans(pairwise_ratios[, -ind_baseline], na.rm = TRUE)
    ratios_baseline2 <- mean_ratios / mean_ratios[ind_baseline]
    model_names <- names(mean_ratios)
  }

  tab <- data.frame(
    model = model_names,
    rel_wis = ratios_baseline2)

  tab <- tab[order(tab$rel_wis), ]
  return(tab)
}
