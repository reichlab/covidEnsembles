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


#' Calculate quantile score, optionally including absolute error,
#' for a parametric distribution.
#'
#' @param y vector of observations from target distribution
#' @param alphas vector of alphas.  the score is based on several
#'   (1 - alpha)*100% predictive intervals for each alpha in alphas
#' @param ae logical.  If true, include absolute error in score
#' @param family family for distribution; e.g. 'norm' or 'nbinom'
#' @param args list of arguments for quantile function of parametric family,
#'   e.g. list(mean=0.0, sd=1.0)
#'
#' @return vector of interval scores separately for each element of x
#'
#' @export
wis <- function(y, qfm, quantile_probs) {
  if(length(unique(quantile_probs)) != length(quantile_probs)) {
    stop('quantile_probs must be unique')
  }

  # if median is one of the quantiles, keep track of that
  median_ind <- which(quantile_probs == 0.5)

  # determine number K of intervals to score, and
  # initialize score to absolute error if median provided or 0 otherwise
  if(length(median_ind) == 1) {
    K <- (length(quantile_probs) - 1)/2
    score <- abs(y - qfm[, median_ind])
  } else {
    K <- length(quantile_probs) / 2
    score <- rep(0.0, length(y))
  }

  # interval scores for each interval
  for(k in seq_len(K)) {
    alpha <- 2*quantile_probs[k]
    score <- score + interval_score(y, qfm[, k], qfm[, ncol(qfm)+1-k], alpha)
  }

  score <- score / (K + length(median_ind))

  return(score)
}
