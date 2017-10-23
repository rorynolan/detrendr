img_detrend_smoothed <- function(arr3d, arr3d_smoothed, seed, parallel) {
  deviations_from_smoothed <- arr3d - arr3d_smoothed
  pillar_means <- as.vector(mean_pillars(arr3d, parallel = parallel))
  variance_correction_factors <- square_root(pillar_means / arr3d_smoothed,
                                             parallel = parallel)
  deviations_from_smoothed <- deviations_from_smoothed *
    variance_correction_factors
  rm(variance_correction_factors)
  out_real <- pillar_means + deviations_from_smoothed
  rm(deviations_from_smoothed)
  out_int <- floor(out_real) %>% {. + myrbern(out_real - ., seed = seed,
                                              parallel = parallel)}
  dim(out_int) <- dim(arr3d)
  out_int
}

img_detrend_tau_specified <- function(arr3d, tau, cutoff, seed, parallel) {
  d <- dim(arr3d)
  l <- min(floor(- tau * log(cutoff)), d[3] %>% {(. - 1) + (. - 2)})
  extend_both_sides_by <- min(d[3] - 2, l)
  extended <- med_reflect_extend_pillars(arr3d, extend_both_sides_by,
                                         parallel = parallel)
  smoothed <- exp_smooth_pillars(extended, extend_both_sides_by,
                                 tau, l, parallel = parallel)
  img_detrend_smoothed(arr3d, smoothed, seed, parallel)
}

img_detrend_l_specified <- function(arr3d, l, seed, parallel) {
  d <- dim(arr3d)
  l <- min(l, d[3] %>% {(. - 1) + (. - 2)})
  extend_both_sides_by <- min(d[3] - 2, l)
  extended <- med_reflect_extend_pillars(arr3d, extend_both_sides_by,
                                         parallel = parallel)
  smoothed <- boxcar_smooth_pillars(extended, extend_both_sides_by,
                                    l, parallel = parallel)
  img_detrend_smoothed(arr3d, smoothed, seed, parallel)
}

img_detrend_degree_specified <- function(arr3d, degree, seed, parallel) {
  d <- dim(arr3d)
  smoothed <- poly_fit_pillars(arr3d, degree, parallel = parallel)
  img_detrend_smoothed(arr3d, smoothed, seed, parallel = parallel)
}


#' Detrend images.
#'
#' Correct images for bleaching (or any other effect that introduces an unwanted
#' trend) by *detrending*.
#'
#' There are 3 detrending methods available: *boxcar*, *exponential filtering*
#' and *polynomial*. These are described in detail in Nolan et al., 2017.
#' \itemize{\item *Boxcar* detrending with parameter \eqn{l} is a moving average
#' detrending method using a sliding window of size \eqn{2l + 1}. \item
#' *Exponential filtering* detrending is a moving weighted average method where
#' for parameter \eqn{tau} the weights are calculated as exp\eqn{(- t / tau)}
#' where \eqn{t} is the distance from the point of interest. \item *Polynomial*
#' detrending works by fitting a polynomial line to a series of points and then
#' correcting the series to remove the trend detailed by this polynomial fit.}
#'
#' @param img The image series to be detrended. This must be a 3-dimensional
#'   array of non-negative integers, with the matrix defined as `img[, , 1]`
#'   being the first frame of the image series.
#' @param l The length parameter for *boxcar* detrending. The size of the
#'   sliding window will be `2 * l + 1`. This must be a positive integer. Set
#'   this to "auto" to use Nolan's algorithm to automatically find a suitable
#'   value for this parameter (recommended).
#' @param tau The \eqn{tau} parameter for *exponential filtering* detrending.
#'   This must be a positive number. Set this to "auto" to use Nolan's algorithm
#'   to automatically find a suitable value for this parameter (recommended).
#' @param cutoff In *exponential filtering* detrending, for the weighted
#'   average, every point gets a weight. This can slow down the computation
#'   massively. However, many of the weights will be approximately zero. With
#'   cutoff, we say that any point with weight less than or equal to `cutoff`
#'   times the maximum weight may be ignored; so with `cutoff = 0.05`, any
#'   weight less than 5\% of the maximum weight may be ignored. The default
#'   value of this parameter is sensible and its value should not be set to
#'   anything else without good reason.
#' @param degree The degree of the polynomial to use for the polynomial
#'   detrending. This must be a positive integer. Set this to "auto" to use
#'   Nolan's algorithm to automatically find a suitable value for this
#'   parameter (recommended).
#' @param seed Random numbers may be generated during the detrending process.
#'   For reproducibility, you can set a seed for this random number generation
#'   here.
#' @param parallel Would you like to use multiple cores to speed up this
#'   function? If so, set the number of cores here, or to use all available
#'   cores, use `parallel = TRUE`.
#'
#' @return The detrended image, an object of class [detrended_img].
#'
#' @name detrending

#' @rdname detrending
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
#' img <- read_tif(system.file('extdata', 'bleached.tif', package = 'detrendr'),
#'                 n_ch = 1)
#' corrected <- img_detrend_boxcar(img, "auto", seed = 0, parallel = 2)
#' corrected10 <- img_detrend_boxcar(img, 10, seed = 0, parallel = 2)
#' corrected50 <- img_detrend_boxcar(img, 50, seed = 0, parallel = 2)
#' corrected100 <- img_detrend_boxcar(img, 100, seed = 0, parallel = 2)
#' corrected300 <- img_detrend_boxcar(img, 300, seed = 0, parallel = 2)
#' corrected <- img_detrend_exp(img, "auto", seed = 0, parallel = 2)
#' corrected10 <- img_detrend_exp(img, 10, seed = 0, parallel = 2)
#' corrected50 <- img_detrend_exp(img, 50, seed = 0, parallel = 2)
#' corrected100 <- img_detrend_exp(img, 100, seed = 0, parallel = 2)
#' corrected1000 <- img_detrend_exp(img, 1000, seed = 0, parallel = 2)
#' corrected <- img_detrend_polynom(img, "auto", seed = 0, parallel = 2)
#' corrected1 <- img_detrend_polynom(img, 1, seed = 0, parallel = 2)
#' corrected2 <- img_detrend_polynom(img, 2, seed = 0, parallel = 2)
#' corrected4 <- img_detrend_polynom(img, 4, seed = 0, parallel = 2)
#' corrected8 <- img_detrend_polynom(img, 8, seed = 0, parallel = 2)
#' }
#' @export
img_detrend_boxcar <- function(img, l, seed = NULL, parallel = FALSE) {
  if (is.null(seed)) seed <- rand_seed()
  checkmate::check_int(l, na.ok = TRUE)
  if (is.na(l)) {
    detrended_img(img, "boxcar", l, NA)
  } else if (is.numeric(l)) {
    if (l <= 0) stop("l must be greater than zero.")
    l %<>% floor()
    img_detrend_l_specified(img, l, seed, parallel) %>%
      detrended_img("boxcar", l, FALSE)
  } else if (is.character(l)) {
    l <- tolower(l)
    if (startsWith("auto", l)) {
      l <- best_l(img, seed = seed, parallel = parallel)
      img_detrend_l_specified(img, l, seed, parallel) %>%
        detrended_img("boxcar", l, TRUE)
    } else {
      stop("If l is a string, the only permissible value is 'auto' whereas ",
           "you have used '", l, "'.")
    }
  } else {
    stop("l must be specified as a positive number or as 'auto'.")
  }
}

#' @rdname detrending
#' @export
img_detrend_exp <- function(img, tau, cutoff = 0.05,
                            seed = NULL, parallel = FALSE) {
  if (is.null(seed)) seed <- rand_seed()
  checkmate::check_scalar(tau, na.ok = TRUE)
  if (is.na(tau)) {
    detrended_img(img, "exponential", tau, NA)
  } else if (is.numeric(tau)) {
    if (tau <= 0) stop("tau must be greater than zero.")
    img_detrend_tau_specified(img, tau, cutoff, seed, parallel) %>%
      detrended_img("exponential", tau, TRUE)
  } else if (is.character(tau)) {
    tau <- tolower(tau)
    if (startsWith("auto", tau)) {
      tau <- best_tau(img, cutoff = cutoff, seed = seed, parallel = parallel)
      img_detrend_tau_specified(img, tau, cutoff, seed, parallel) %>%
        detrended_img("exponential", tau, FALSE)
    } else {
      stop("If tau is a string, the only permissible value is 'auto' whereas ",
           "you have used '", tau, "'.")
    }
  } else {
    stop("tau must be specified as a positive number or as 'auto'.")
  }
}

#' @rdname detrending
#' @export
img_detrend_polynom <- function(img, degree, seed = NULL, parallel = FALSE) {
  if (is.null(seed)) seed <- rand_seed()
  checkmate::check_int(degree, na.ok = TRUE)
  if (is.na(degree)) {
    detrended_img(img, "polynomial", degree, TRUE)
  } else if (is.numeric(degree)) {
    if (degree <= 0) stop("degree must be greater than zero.")
    img_detrend_degree_specified(img, degree, seed, parallel) %>%
      detrended_img("polynomial", degree, FALSE)
  } else if (is.character(degree)) {
    degree <- tolower(degree)
    if (startsWith("auto", degree)) {
      degree <- best_degree(img, seed = seed, parallel = parallel)
      img_detrend_degree_specified(img, degree, seed, parallel) %>%
        detrended_img("polynomial", degree, TRUE)
    } else {
      stop("If degree is a string, the only permissible value is 'auto' ",
           "whereas you have used '", degree, "'.")
    }
  } else {
    stop("degree must be specified as a positive number or as 'auto'.")
  }
}
