rows_detrend_tau_specified_extended <- function(mat, mat_extended,
                                                tau, l, seed, parallel) {
  extended_both_sides_by <- (ncol(mat_extended) - ncol(mat)) / 2
  smoothed <- exp_smooth_rows(mat_extended, extended_both_sides_by,
                              tau, l, parallel = parallel)
  rows_detrend_smoothed(mat, smoothed, seed, parallel)
}

rows_detrend_tau_specified_extended_mean_b <- function(mat, mat_extended,
                                                       tau, l, seed, parallel) {
  rows_detrend_tau_specified_extended(mat, mat_extended, tau, l, seed,
                                      parallel) %>%
    brightness_rows(parallel = parallel) %>%
    mean()
}

#' Find the best tau parameter for exponential smoothing detrending.
#'
#' Use Nolan's algorithm to find the ideal tau parameter for exponential
#' smoothing detrending.
#'
#' @param img A 3-dimensional array of integers representing the image series.
#'   `img[i, j, k]` is the pixel at `x = i`, `y = j` in frame `k` of the image
#'   series.
#' @param cutoff When exponential smoothing, neglect weights which are less than
#'   `cutoff * central weight`. It is fine to leave this parameter at the
#'   default level.
#' @param seed A seed for the random number generation needed in this function.
#'   If you do not set a seed, one will be chosen randomly.
#' @param parallel Do you want this function to run on multiple cores? If not,
#'   use `parallel = FALSE` (the default). To use `n` cores, use `parallel = n`
#'   or to use all available cores use `parallel = TRUE`.
#'
#' @return If no detrend is necessary, this function returns `NA`. If a detrend
#'   is required, this function returns a natural number which is the ideal
#'   `tau` parameter for exponential smoothing detrending.
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
#' img <- read_tif(system.file('extdata', 'bleached.tif', package = 'detrendr'),
#'                 n_ch = 1)
#' best_tau(img, seed = 0, parallel = 2)
#' }
#' @export
best_tau <- function(img, cutoff = 0.05, seed = NULL, parallel = FALSE) {
  if (is.null(seed)) seed <- rand_seed()
  d <- dim(img)
  frame_length <- sum(!is.na(img[, , 1]))
  frame_means <- apply(img, 3, mean, na.rm = TRUE)
  sim_mat <- myrpois_frames(frame_means, frame_length, seed, parallel)
  sim_brightness <- brightness_rows(sim_mat, parallel = parallel) %>%
    mean()
  if (sim_brightness <= 1) return(NA)
  max_possible_extension_both_sides <- d[3] - 2
  extend_both_sides_by <- min(250, max_possible_extension_both_sides)
  sim_extended <- med_reflect_extend_rows(sim_mat, extend_both_sides_by,
                                          parallel = parallel)
  current_extension_both_sides <- extend_both_sides_by
  big_tau <- - current_extension_both_sides / log(cutoff)
  big_tau_old <- big_tau
  max_l <- ncol(sim_mat) %>% {(. - 1) + (. - 2)}
  l <- min(floor(- big_tau * log(cutoff)), max_l)
  mean_brightness_big_tau <- rows_detrend_tau_specified_extended_mean_b(
    sim_mat, sim_extended, big_tau, l, seed, parallel = parallel)
  mean_brightness_big_tau_old <- mean_brightness_big_tau
  while (mean_brightness_big_tau <= 1) {
    if (current_extension_both_sides < max_possible_extension_both_sides) {
      extend_both_sides_by <- min(
        max_possible_extension_both_sides - current_extension_both_sides,
        2 * extend_both_sides_by)
      sim_extended %<>% med_reflect_extend_rows(extend_both_sides_by,
                                                sim_mat)
      current_extension_both_sides <- current_extension_both_sides +
                                        extend_both_sides_by
    }
    big_tau_old <- big_tau
    big_tau <- 2 * big_tau
    l <- min(floor(- big_tau * log(cutoff)), max_l)
    mean_brightness_big_tau <- rows_detrend_tau_specified_extended_mean_b(
      sim_mat, sim_extended, big_tau, l, seed, parallel = parallel)
  }
  if (big_tau_old == big_tau) {
    while (mean_brightness_big_tau_old > 1) {
      big_tau_old <- big_tau_old / 2
      l <- min(floor(- big_tau_old * log(cutoff)), max_l)
      mean_brightness_big_tau_old <- rows_detrend_tau_specified_extended_mean_b(
        sim_mat, sim_extended, big_tau_old, l, seed, parallel = parallel)
    }
  }
  if (mean_brightness_big_tau_old == 1) return(round(big_tau_old))
  tau_upper <- big_tau
  tau_lower <- big_tau_old
  mean_brightness_tau_upper <- mean_brightness_big_tau
  mean_brightness_tau_lower <- mean_brightness_big_tau_old
  while (tau_upper - tau_lower > 1) {
    middle_tau <- mean(c(tau_lower, tau_upper))
    l <- min(floor(- middle_tau * log(cutoff)), max_l)
    middle_brightness_mean <- rows_detrend_tau_specified_extended_mean_b(
      sim_mat, sim_extended, middle_tau, l, seed, parallel = parallel)
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
}
