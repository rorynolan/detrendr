boxcar_smooth_rows <- function(extended, extended_both_sides_by, l,
                               parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  boxcar_smooth_rows_(extended, extended_both_sides_by, l)
}

boxcar_smooth_pillars <- function(extended, extended_both_sides_by, l,
                                  parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  stopifnot(l <= extended_both_sides_by)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  boxcar_smooth_pillars_(extended, extended_both_sides_by, l)
}

exp_smooth_rows <- function(extended, extended_both_sides_by, tau, l,
                            parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  exp_smooth_rows_(extended, extended_both_sides_by, tau, l)
}

exp_smooth_pillars <- function(extended, extended_both_sides_by, tau, l,
                               parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  exp_smooth_pillars_(extended, extended_both_sides_by, tau, l)
}
