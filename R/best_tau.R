rows_detrend_tau_specified <- function(mat, tau, l, seed, parallel) {
  smoothed <- exp_smooth_rows(mat, tau, l, parallel = parallel)
  rows_detrend_smoothed(mat, smoothed, seed, parallel)
}

rows_detrend_tau_specified_mean_b <- function(mat, tau, l, seed, parallel) {
  rows_detrend_tau_specified(mat, tau, l, seed, parallel) %>%
    brightness_rows(parallel = parallel) %>%
    mean(na.rm = TRUE)
}

#' Find the best tau parameter for exponential smoothing detrending.
#'
#' Use Nolan's algorithm to find the ideal tau parameter for exponential
#' smoothing detrending.
#'
#' @inheritParams detrending
#'
#' @return If no detrend is necessary, this function returns `NA`. If a detrend
#'   is required, this function returns a natural number which is the ideal
#'   `tau` parameter for exponential smoothing detrending. If there are multiple
#'   channels, the function returns a vector, one `tau` parameter for each
#'   channel.
#'
#' @references Rory Nolan, Luis A. J. Alvarez, Jonathan Elegheert, Maro
#'   Iliopoulou, G. Maria Jakobsdottir, Marina Rodriguez-Muñoz, A. Radu
#'   Aricescu, Sergi Padilla-Parra; nandb—number and brightness in R with a
#'   novel automatic detrending algorithm, Bioinformatics,
#'   https://doi.org/10.1093/bioinformatics/btx434.
#' @examples
#' \dontrun{
#' ## These examples are not run on CRAN because they take too long.
#' ## You should still try them for yourself.
#' img <- ijtiff::read_tif(system.file('extdata', 'bleached.tif',
#'                                     package = 'detrendr'))[, , 1, ]
#' best_tau(img, seed = 0, parallel = 2)
#' }
#' @export
best_tau <- function(img, cutoff = 0.05, seed = NULL, parallel = FALSE) {
  checkmate::assert_numeric(img, lower = 0)
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  d <- dim(img)
  if (length(d) == 3) {
    if (is.null(seed)) seed <- rand_seed()
    frame_length <- sum(!is.na(img[, , 1]))
    frame_means <- apply(img, 3, mean, na.rm = TRUE)
    sim_mat <- myrpois_frames(frame_means, frame_length, seed, parallel)
    sim_brightness <- brightness_rows(sim_mat, parallel = parallel) %>%
      mean(na.rm = TRUE)
    if (sim_brightness <= 1) return(NA)
    big_tau <- 100
    big_tau_old <- big_tau
    max_l <- ncol(sim_mat) %>% {(. - 1) + (. - 2)}
    l <- min(floor(- big_tau * log(cutoff)), max_l)
    mean_brightness_big_tau <- rows_detrend_tau_specified_mean_b(
      sim_mat, big_tau, l, seed, parallel = parallel)
    mean_brightness_big_tau_old <- mean_brightness_big_tau
    while (mean_brightness_big_tau <= 1) {
      big_tau_old <- big_tau
      big_tau <- 2 * big_tau
      l <- min(floor(- big_tau * log(cutoff)), max_l)
      mean_brightness_big_tau <- rows_detrend_tau_specified_mean_b(
        sim_mat, big_tau, l, seed, parallel = parallel)
    }
    if (big_tau_old == big_tau) {
      while (mean_brightness_big_tau_old > 1) {
        big_tau_old <- big_tau_old / 2
        l <- min(floor(- big_tau_old * log(cutoff)), max_l)
        mean_brightness_big_tau_old <- rows_detrend_tau_specified_mean_b(
          sim_mat, big_tau_old, l, seed, parallel = parallel)
      }
    }
    if (mean_brightness_big_tau_old == 1) return(big_tau_old)
    tau_upper <- big_tau
    tau_lower <- big_tau_old
    mean_brightness_tau_upper <- mean_brightness_big_tau
    mean_brightness_tau_lower <- mean_brightness_big_tau_old
    while (tau_upper - tau_lower > 1) {
      middle_tau <- mean(c(tau_lower, tau_upper))
      l <- min(floor(- middle_tau * log(cutoff)), max_l)
      middle_brightness_mean <- rows_detrend_tau_specified_mean_b(
        sim_mat, middle_tau, l, seed, parallel = parallel)
      if (middle_brightness_mean < 1) {
        tau_lower <- middle_tau
        mean_brightness_tau_lower <- middle_brightness_mean
      } else if (middle_brightness_mean > 1) {
        tau_upper <- middle_tau
        mean_brightness_tau_upper <- middle_brightness_mean
      } else {
        return(round(middle_tau))
      }
    }
    upper_closer <- abs(mean_brightness_tau_upper - 1) >
      abs(mean_brightness_tau_lower - 1)
    dplyr::if_else(upper_closer, tau_upper, tau_lower)
  } else {
    purrr::map_dbl(seq_len(d[3]),
                   ~ best_tau(img[, , ., ], cutoff = cutoff,
                              seed = seed, parallel = parallel))
  }
}
