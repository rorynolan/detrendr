brightness_vec <- function(vec) {
  var(vec) / mean(vec)
}

brightness_rows <- function(mat, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  brightness_rows_(mat)
}

brightness_pillars <- function(arr3d) {
  var_pillars(arr3d) / mean_pillars(arr3d)
}
