med_reflect_extend <- function(vec, extend_both_sides_by, original = vec,
                               preserve_mean = TRUE, smooth = TRUE) {
  med_reflect_extend_(vec, original, extend_both_sides_by,
                      preserve_mean, smooth)
}

med_reflect_extend_rows <- function(mat, extend_both_sides_by, original = mat,
                                    preserve_mean = TRUE, smooth = TRUE,
                                    parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  med_reflect_extend_rows_(mat, original, extend_both_sides_by,
                           preserve_mean, smooth)
}

med_reflect_extend_pillars <- function(arr3d, extend_both_sides_by,
                                       original = arr3d,
                                       preserve_mean = TRUE, smooth = TRUE,
                                       parallel = FALSE) {
  n_cores <- translate_parallel(parallel)
  RcppParallel::setThreadOptions(n_cores)
  on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
  med_reflect_extend_pillars_(arr3d, original, extend_both_sides_by,
                              preserve_mean, smooth)
}
