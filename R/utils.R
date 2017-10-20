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

can_be_integer <- function(x) {
  isTRUE(all.equal(x, floor(x), check.attributes = FALSE))
}

frames_to_list <- function(img) {
  if (is.matrix(img)) return(list(img))
  d <- dim(img)
  stopifnot(length(d) == 3)
  purrr::map(seq_len(d[3]), ~ img[, , .])
}

#' Apply a function to each pillar of a 3-dimensional array.
#'
#' Define a 'pillar' of a 3-dimensional array as pillar `i,j` off array
#' `arr` being `arr[i, j, ]`. This function applies a specified
#' function to each pillar.
#'
#' @param arr3d A 3-dimensional array.
#' @param FUN A function which takes a vector as input and, for a given input
#'   length, outputs a vector of constant length (can be 1).
#'
#' @return If `FUN` is returning length 1 vectors, a matrix whereby
#'   `mat[i, j] = FUN(arr3d[i, j, ])`. If FUN is returning vectors of
#'   length `l > 1`, a 3-dimensional array whereby \code{arr[i, j, ] =
#'   FUN(arr3d[i, j, ])}.
#' @export
apply_on_pillars <- function(arr3d, FUN) {
  apply(arr3d, c(1, 2), FUN) %>% {
    if (length(dim(.)) == 3) {
      aperm(., c(2, 3, 1))
    } else {
      .
    }
  }
}
