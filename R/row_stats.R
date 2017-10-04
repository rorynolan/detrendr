brightness_rows <- function(mat, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  brightness_rows_(mat)
}

mean_rows <- function(mat, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  mean_rows_(mat)
}
