cols_detrend_smoothed <- function(mat, mat_smoothed, seed, parallel) {
  t(mat) %>%
    rows_detrend_smoothed(t(mat_smoothed), seed, parallel = parallel) %>%
    t()
}

cols_detrend_degree_specified <- function(mat, degree, seed, parallel) {
  smoothed <- poly_fit_cols(mat, degree, parallel = parallel)
  cols_detrend_smoothed(mat, smoothed, seed, parallel)
}

cols_detrend_degree_specified_mean_b <- function(mat, degree, seed, parallel) {
  cols_detrend_degree_specified(mat, degree, seed, parallel) %>%
    brightness_cols(parallel = parallel) %>%
    mean(na.rm = TRUE)
}

#' Find the best polynomial degree for polynomial detrending.
#'
#' Use Nolan's algorithm to find the ideal polynomial degree for polynomial
#' detrending.
#'
#' @inheritParams detrending
#'
#' @return If no detrend is necessary, this function returns `NA`. If a detrend
#'   is required, this function returns a natural number which is the ideal
#'   polynomial degree for polynomial detrending. If there are multiple
#'   channels, the function returns a vector, one `degree` parameter for each
#'   channel.
#'
#' @references Rory Nolan, Luis A. J. Alvarez, Jonathan Elegheert, Maro
#'   Iliopoulou, G. Maria Jakobsdottir, Marina Rodriguez-Muñoz, A. Radu
#'   Aricescu, Sergi Padilla-Parra; nandb—number and brightness in R with a
#'   novel automatic detrending algorithm, Bioinformatics,
#'   https://doi.org/10.1093/bioinformatics/btx434.
#'
#' @examples
#' \dontrun{
#' ## These examples are not run on CRAN because they take too long.
#' ## You should still try them for yourself.
#' img <- ijtiff::read_tif(system.file('extdata', 'bleached.tif',
#'                                     package = 'detrendr'))
#' best_degree(img, seed = 0, parallel = 2)
#' }
#' @export
best_degree <- function(img, seed = NULL, parallel = FALSE) {
  checkmate::assert_numeric(img, lower = 0)
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  d <- dim(img)
  if (length(d) == 3) {
    if (is.null(seed)) seed <- rand_seed()
    frame_length <- sum(!is.na(img[, , 1]))
    frame_means <- apply(img, 3, mean, na.rm = TRUE)
    sim_mat <- myrpois_frames_t(frame_means, frame_length, seed, parallel)
    sim_brightness <- brightness_cols(sim_mat, parallel = parallel) %>%
      mean(na.rm = TRUE)
    if (sim_brightness <= 1) return(NA)
    lower_degree <- 0
    lower_degree_brightness <- sim_brightness
    upper_degree <- 1
    upper_degree_brightness <- cols_detrend_degree_specified_mean_b(
      sim_mat, upper_degree, seed,
      parallel = parallel)
    if (upper_degree_brightness < 1) {
      return(ifelse(1 - upper_degree_brightness < lower_degree_brightness - 1,
                    upper_degree, NA))
    }
    while (upper_degree_brightness > 1) {
      lower_degree <- lower_degree + 1
      lower_degree_brightness <- upper_degree_brightness
      upper_degree <- upper_degree + 1
      upper_degree_brightness <- cols_detrend_degree_specified_mean_b(
        sim_mat, upper_degree, seed,
        parallel = parallel)
    }
    out <- ifelse(1 - upper_degree_brightness < lower_degree_brightness - 1,
                  upper_degree, lower_degree)
    if (out > 3) {
      warning("The polynomial degree found for your detrend was ", out, ". ",
              "Degrees above 3 are not recommended as they usually indicate ",
              "eccentric fits. It would be wise to use another detrending ",
              "method (exponential or boxcar).")
    }
    out
  } else {
    purrr::map_int(seq_len(d[3]),
                   ~ best_degree(img[, , ., ],
                                 seed = seed, parallel = parallel))
  }
}
