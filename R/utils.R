translate_parallel <- function(parallel) {
  parallel <- parallel[1]
  if (BBmisc::isFALSE(parallel)) {
    n_cores <- 1
  } else if (isTRUE(parallel)) {
    n_cores <- parallel::detectCores()
  } else if (is.numeric(parallel)) {
    n_cores <- floor(parallel)
    if (n_cores > parallel::detectCores()) n_cores <- parallel::detectCores()
  } else {
    stop("If parallel is not TRUE or FALSE, then it must be numeric.")
  }
  n_cores
}

signif_to_l <- function(tau, signif) {
  stopifnot(signif > 0, signif < 1)
  floor(- tau * log(signif))
}

mat_to_col_list <- function(mat) {
  lapply(seq_len(ncol(mat)), function(i) mat[, i])
}

rand_seed <- function() sample.int(2 ^ 30, 1)
