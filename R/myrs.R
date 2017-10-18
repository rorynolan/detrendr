myrpois <- function(means, seed = NULL, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  if (is.null(seed)) seed <- rand_seed()
  myrpois_(means, seed)
}

## every column is a frame, every row is a pixel
myrpois_frames <- function(means, frame_length, seed = NULL, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  if (is.null(seed)) seed <- rand_seed()
  myrpois_frames_(means, frame_length, seed)
}

## every column is a pixel, every row is a frame
myrpois_frames_t <- function(means, frame_length, seed = NULL, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  if (is.null(seed)) seed <- rand_seed()
  myrpois_frames_t_(means, frame_length, seed)
}

myrbern <- function(p, seed = NULL, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  if (is.null(seed)) seed <- rand_seed()
  myrbernoulli_(p, seed)
}
