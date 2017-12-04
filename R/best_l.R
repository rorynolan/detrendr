rows_detrend_smoothed <- function(mat, mat_smoothed, seed, parallel) {
  deviations_from_smoothed <- mat - mat_smoothed
  row_means <- mean_rows(mat, parallel = parallel)
  variance_correction_factors <- square_root(row_means / mat_smoothed,
                                             parallel = parallel) %T>% {
    .[!is.finite(.)] <- 1
  }
  deviations_from_smoothed <- deviations_from_smoothed *
    variance_correction_factors
  rm(variance_correction_factors)
  out_real <- row_means + deviations_from_smoothed
  rm(deviations_from_smoothed)
  out_int <- floor(out_real) %>%
    {. + myrbern(out_real - ., seed = seed, parallel = parallel)} %T>%
    {.[. < 0] <- 0}
  dim(out_int) <- dim(mat)
  out_int
}

rows_detrend_l_specified <- function(mat, l, seed, parallel) {
  smoothed <- boxcar_smooth_rows(mat, l, parallel = parallel)
  rows_detrend_smoothed(mat, smoothed, seed, parallel)
}

rows_detrend_l_specified_mean_b <- function(mat, l, seed, parallel) {
  rows_detrend_l_specified(mat, l, seed, parallel) %>%
    brightness_rows(parallel = parallel) %>%
    mean(na.rm = TRUE)
}


#' Find the best length parameter for boxcar detrending.
#'
#' Use Nolan's algorithm to find the ideal length parameter for boxcar
#' detrending. Boxcar detrending is also referred to as 'running average'.
#'
#' @inheritParams detrending
#'
#' @return If no detrend is necessary, this function returns `NA`. If a detrend
#'   is required, this function returns a natural number which is the ideal
#'   length parameter for boxcar detrending. If there are multiple
#'   channels, the function returns a vector, one `l` parameter for each
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
#'                                     package = 'detrendr'))
#' best_l(img, seed = 0, parallel = 2)
#' }
#' @export
best_l <- function(img, seed = NULL, parallel = FALSE) {
  checkmate::assert_numeric(img, lower = 0)
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  d <- dim(img)
  if (length(d) == 3) {
    if (is.null(seed)) seed <- rand_seed()
    frame_length <- sum(!is.na(img[, , 1]))
    frame_means <- apply(img, 3, mean, na.rm = TRUE)
    sim_mat <- myrpois_frames(frame_means, frame_length, seed, parallel)
    sim_brightness <- brightness_rows(sim_mat, parallel = parallel) %>% mean()
    if (sim_brightness <= 1) return(NA)
    maxl <- d[3] - 1
    big_l <- min(10, maxl)
    big_l_old <- big_l
    max_l <- ncol(sim_mat) %>% {(. - 1) + (. - 2)}
    mean_brightness_big_l <- rows_detrend_l_specified_mean_b(
      sim_mat, big_l, seed, parallel = parallel)
    mean_brightness_big_l_old <- mean_brightness_big_l
    while (mean_brightness_big_l <= 1) {
      big_l_old <- big_l
      big_l <- min(maxl, 2 * big_l)
      mean_brightness_big_l <- rows_detrend_l_specified_mean_b(
        sim_mat, big_l, seed, parallel = parallel)
    }
    if (big_l_old == big_l) {
      while (mean_brightness_big_l_old > 1) {
        big_l_old <- big_l_old %/% 2
        if (big_l_old == 0) {
          stop("Even with box size l = 1 (the most severe detrend)",
               "the brightness B was still above 1. ",
               "There is probably something wrong with your data.")
        }
        mean_brightness_big_l_old <- rows_detrend_l_specified_mean_b(
          sim_mat, big_l_old, seed, parallel = parallel)
      }
    }
    if (mean_brightness_big_l_old == 1) return(round(big_l_old))
    l_upper <- big_l
    l_lower <- big_l_old
    mean_brightness_l_upper <- mean_brightness_big_l
    mean_brightness_l_lower <- mean_brightness_big_l_old
    while (l_upper - l_lower > 1) {
      middle_l <- round(mean(c(l_lower, l_upper)))
      middle_brightness_mean <- rows_detrend_l_specified_mean_b(
        sim_mat, middle_l, seed, parallel = parallel)
      if (middle_brightness_mean < 1) {
        l_lower <- middle_l
        mean_brightness_l_lower <- middle_brightness_mean
      } else if (middle_brightness_mean > 1) {
        l_upper <- middle_l
        mean_brightness_l_upper <- middle_brightness_mean
      } else {
        return(middle_l)
      }
    }
    upper_closer <- abs(mean_brightness_l_upper - 1) >
      abs(mean_brightness_l_lower - 1)
    dplyr::if_else(upper_closer, l_upper, l_lower)
  } else {
    purrr::map_int(seq_len(d[3]),
                   ~ best_l(img[, , ., ], seed = seed, parallel = parallel))
  }
}
