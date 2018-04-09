mean_frames <- function(arr3d, parallel = FALSE) {
  checkmate::assert_numeric(arr3d)
  checkmate::assert_array(arr3d, d = 3)
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  mean_frames_(arr3d)
}

sum_frames <- function(arr3d, parallel = FALSE) {
  checkmate::assert_numeric(arr3d)
  checkmate::assert_array(arr3d, d = 3)
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  sum_frames_(arr3d)
}
