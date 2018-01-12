#' Get the means/medians/variances of pillars of an
#' [ijtiff_img][ijtiff::ijtiff_img]-style array.
#'
#' For an [ijtiff_img][ijtiff::ijtiff_img]-style array `img` (indexed as `img[y,
#' x, channel, frame]`), pillar `xy` of channel
#' `ch` is defined as `img[y, x, ch, ]`. These functions compute the mean,
#' median and variance of each pillar for each channel.
#'
#' @inheritParams detrending
#'
#' @return An [ijtiff_img][ijtiff::ijtiff_img]-style array `arr` with one frame.
#'   `arr[y, x, ch, 1]` is equal to `mean(img[y, x, ch, ])`, `median(img[y, x,
#'   ch, ])`, or `var(img[y, x, ch, ])`.
#'
#' @examples
#' aaa <- array(seq_len(2 ^ 4), dim = rep(2, 4))  # a 2-channel, 2-frame array
#' mean_pillars(aaa)
#' median_pillars(aaa)
#' var_pillars(aaa)
#'
#' @name pillar-stats
NULL

#' @rdname pillar-stats
#' @export
mean_pillars <- function(img, parallel = FALSE) {
  checkmate::assert_numeric(img)
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  d <- dim(img)
  if (length(d) == 3) {
    n_cores <- translate_parallel(parallel)
    RcppParallel::setThreadOptions(n_cores)
    on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
    mean_pillars_(img)
  } else {
    out <- purrr::map(seq_len(d[3]), ~ mean_pillars_(img[, , ., ])) %>%
      unlist()
    d[4] <- 1
    dim(out) <- d
    out
  }
}

#' @rdname pillar-stats
#' @export
median_pillars <- function(img, parallel = FALSE) {
  checkmate::assert_numeric(img)
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  d <- dim(img)
  if (length(d) == 3) {
    n_cores <- translate_parallel(parallel)
    RcppParallel::setThreadOptions(n_cores)
    on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
    median_pillars_(img)
  } else {
    out <- purrr::map(seq_len(d[3]), ~ median_pillars_(img[, , ., ])) %>%
      unlist()
    d[4] <- 1
    dim(out) <- d
    out
  }
}

#' @rdname pillar-stats
#' @export
var_pillars <- function(img, parallel = FALSE) {
  checkmate::assert_numeric(img)
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  d <- dim(img)
  if (length(d) == 3) {
    n_cores <- translate_parallel(parallel)
    RcppParallel::setThreadOptions(n_cores)
    on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
    var_pillars_(img)
  } else {
    out <- purrr::map(seq_len(d[3]), ~ var_pillars_(img[, , ., ])) %>%
      unlist()
    d[4] <- 1
    dim(out) <- d
    out
  }
}

#' Get the brightness of pillars of a 3d array.
#'
#' For an [ijtiff_img][ijtiff::ijtiff_img]-style array `img` (indexed as `img[y,
#' x, channel, frame]`),  3-dimensional array `mat3d`, pillar `xy`  of channel
#' `ch` is defined as `img[y, x, ch, ]`. This function computes the brightness,
#' of each pillar.
#'
#' @inheritParams detrending
#'
#' @return An [ijtiff_img][ijtiff::ijtiff_img]-style array `arr` with
#'   one frame. `arr[y, x, ch, 1]` is equal to `var(img[y, x, ch, ]) /
#'   mean(img[y, x, ch, ])`.
#'
#' @examples
#' aaa <- array(1:16, dim = c(2, 2, 4))
#' brightness_pillars(aaa)
#'
#' @export
brightness_pillars <- function(img, parallel = FALSE) {
  checkmate::assert_numeric(img)
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  d <- dim(img)
  if (length(d) == 3) {
    n_cores <- translate_parallel(parallel)
    RcppParallel::setThreadOptions(n_cores)
    on.exit(RcppParallel::setThreadOptions(RcppParallel::defaultNumThreads()))
    brightness_pillars_(img)
  } else {
    out <- purrr::map(seq_len(d[3]), ~ brightness_pillars_(img[, , ., ])) %>%
      unlist()
    d[4] <- 1
    dim(out) <- d
    out
  }
}
