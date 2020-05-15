## interface to R's C API for logspace arithmetic

#' Calculate log(exp(logx) - exp(logy)) in a somewhat numerically stable way.
#'
#' @param logx, logy log-scale numeric values to subtract
#'
#' @return log(exp(logx) - exp(logy)), but more numerically stable
#'
#' @export
logspace_sub <- function(logx, logy) {
  return(.Call("logspace_sub_C",
               as.numeric(logx),
               as.numeric(logy),
               PACKAGE = "covidEnsembles"))
}

#' Calculate log(exp(logx) + exp(logy)) in a somewhat numerically stable way.
#'
#' @param logx, logy log-scale numeric values to add
#'
#' @return log(exp(logx) + exp(logy)), but more numerically stable
#'
#' @export
logspace_add <- function(logx, logy) {
  return(.Call("logspace_add_C",
               as.numeric(logx),
               as.numeric(logy),
               PACKAGE = "covidEnsembles"))
}

#' Calculate log(sum(exp(logx))) in a somewhat numerically stable way.
#'
#' @param logx log-scale numeric vector of values to sum
#'
#' @return log(sum(exp(logx))), but more numerically stable
#'
#' @export
logspace_sum <- function(logx) {
  dim(logx) <- c(1, length(logx))
  return(logspace_sum_matrix_rows(logx))
}

#' Calculate logspace summation of matrix rows in a somewhat numerically stable
#' way.
#'
#' @param logX log-scale numeric matrix of values to sum.
#'
#' @return log(apply(exp(logX), 1, sum)), but more numerically stable
#'
#' @export
logspace_sum_matrix_rows <- function(logX) {
  return(.Call("logspace_sum_matrix_rows_C",
               as.numeric(logX),
               as.integer(nrow(logX)),
               as.integer(ncol(logX)),
               PACKAGE = "covidEnsembles"))
}

#' Calculate logspace difference of matrix rows in a somewhat numerically stable
#' way.
#'
#' @param logX log-scale numeric matrix of values to subtract.  logX must have
#'   exactly 2 columns.
#'
#' @return log(exp(logX)[, 1] - exp(logX)[, 2]), but more numerically stable
#'
#' @export
logspace_sub_matrix_rows <- function(logX) {
  if(!is.matrix(logX) || !identical(ncol(logX), 2L))
    stop("logX must be a matrix with 2 columns")

  return(.Call("logspace_sub_matrix_rows_C",
               as.numeric(logX),
               as.integer(nrow(logX)),
               PACKAGE = "covidEnsembles"))
}


#' Calculate softmax for each row of a matrix
#'
#' @param X a matrix
#'
#' @return softmax(X) by row; each row of result sums to 1
#'
#' @export
softmax_matrix_rows <- function(X) {
  log_denoms <- logspace_sum_matrix_rows(X)
  X <- sweep(X, 1, log_denoms)
  return(exp(X))
}
