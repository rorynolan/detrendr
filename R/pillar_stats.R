#' Get the means/medians/variances of pillars of a 3d array.
#'
#' For a 3-dimensional array `mat3d`, pillar `ij` is defined as
#' `mat3d[i, j, ]`. These functions compute the mean, median and variance
#' of each pillar.
#'
#' @param arr3d A 3-dimensional array.
#' @param parallel Do you want to parallelize the computation across multiple
#'   cores? If so, set this to the number of cores that you would like to use,
#'   or set it to `TRUE` to use all available cores.
#'
#' @return A matrix where element `i,j` is equal to
#'   `mean(mat3d[i, j, ])`, `median(mat3d[i, j, ])`, or
#'   `var(mat3d[i, j, ])`.
#'
#' @examples
#' aaa <- array(1:16, dim = c(2, 2, 4))
#' mean_pillars(aaa)
#' median_pillars(aaa)
#' var_pillars(aaa)
#'
#' @name pillar-stats
NULL

#' @rdname pillar-stats
#' @export
mean_pillars <- function(arr3d, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  mean_pillars_(arr3d)
}

#' @rdname pillar-stats
#' @export
median_pillars <- function(arr3d, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  median_pillars_(arr3d)
}

#' @rdname pillar-stats
#' @export
var_pillars <- function(arr3d, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  var_pillars_(arr3d)
}

#' Get the brightness of pillars of a 3d array.
#'
#' For a 3-dimensional array `mat3d`, pillar `ij` is defined as `mat3d[i, j, ]`.
#' This function computes the brightness of each pillar. Brightness is `variance
#' / mean`.
#'
#' @param arr3d A 3-dimensional array.
#' @param parallel Do you want to parallelize the computation across multiple
#'   cores? If so, set this to the number of cores that you would like to use,
#'   or set it to `TRUE` to use all available cores.
#'
#' @return A matrix where element `i,j` is equal to `var(mat3d[i, j, ]) /
#'   mean(mat3d[i, j, ])`.
#'
#' @examples
#' aaa <- array(1:16, dim = c(2, 2, 4))
#' brightness_pillars(aaa)
#'
#' @export
brightness_pillars <- function(arr3d, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  brightness_pillars_(arr3d)
}
