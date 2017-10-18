poly_calc_coef_cols <- function(x, coefs, parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  poly_calc_coef_cols_(x, coefs)
}
