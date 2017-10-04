myrpois <- function(means, seed = NULL, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  if (is.null(seed)) seed <- rand_seed()
  myrpois_(means, seed)
}

myrpois_frames <- function(means, frame_length, seed = NULL, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  if (is.null(seed)) seed <- rand_seed()
  myrpois_frames_(means, frame_length, seed)
}

myrbern <- function(p, seed = NULL, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  if (is.null(seed)) seed <- rand_seed()
  myrbernoulli_(p, seed)
}
