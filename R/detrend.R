img_detrend_smoothed <- function(arr3d, arr3d_smoothed, purpose,
                                 seed, parallel) {
  checkmate::assert_string(purpose)
  stopifnot(purpose %in% c("fcs", "ffs"))
  arr3d_smoothed[arr3d_smoothed < 0] <- 0
  deviations_from_smoothed <- arr3d - arr3d_smoothed
  pillar_means <- as.vector(mean_pillars(arr3d, parallel = parallel))
  if (purpose == "ffs") {
    variance_correction_factors <- square_root(pillar_means /
                                                 arr3d_smoothed,
                                               parallel = parallel) %T>% {
      .[!is.finite(.)] <- 1
    }
    deviations_from_smoothed <- deviations_from_smoothed *
      variance_correction_factors
    rm(variance_correction_factors)
  }
  out_real <- pillar_means + deviations_from_smoothed
  rm(deviations_from_smoothed)
  out_int <- floor(out_real) %>% {. + myrbern(out_real - ., seed = seed,
                                              parallel = parallel)}
  dim(out_int) <- dim(arr3d)
  out_int
}

img_detrend_tau_specified <- function(arr3d, tau, cutoff, purpose,
                                      seed, parallel) {
  if (is.na(tau)) return(arr3d)
  d <- dim(arr3d)
  l <- min(floor(- tau * log(cutoff)), d[3] %>% {(. - 1) + (. - 2)})
  smoothed <- exp_smooth_pillars(arr3d, tau, l, parallel = parallel)
  img_detrend_smoothed(arr3d, smoothed, purpose = purpose,
                       seed = seed, parallel = parallel)
}

img_detrend_l_specified <- function(arr3d, l, purpose, seed, parallel) {
  if (is.na(l)) return(arr3d)
  d <- dim(arr3d)
  l <- min(l, d[3] %>% {(. - 1) + (. - 2)})
  smoothed <- boxcar_smooth_pillars(arr3d, l, parallel = parallel)
  img_detrend_smoothed(arr3d, smoothed, purpose = purpose,
                       seed = seed, parallel = parallel)
}

img_detrend_degree_specified <- function(arr3d, degree, purpose,
                                         seed, parallel) {
  if (is.na(degree)) return(arr3d)
  d <- dim(arr3d)
  smoothed <- poly_fit_pillars(arr3d, degree, parallel = parallel)
  img_detrend_smoothed(arr3d, smoothed, purpose = purpose,
                       seed = seed, parallel = parallel)
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
#' @param img A 4-dimensional array in the style of an
#'   [ijtiff_img][ijtiff::ijtiff_img] (indexed by `img[y, x, channel, frame]`)
#'   or a 3-dimensional array which is a single channel of an
#'   [ijtiff_img][ijtiff::ijtiff_img] (indexed by `img[y, x, frame]`).
#' @param l The length parameter for *boxcar* detrending. The size of the
#'   sliding window will be `2 * l + 1`. This must be a positive integer. Set
#'   this to "auto" to use Nolan's algorithm to automatically find a suitable
#'   value for this parameter (recommended). For multi-channel images, it is
#'   possible to have a different `l` for each channel by specifying `l` as a
#'   vector or list.
#' @param tau The \eqn{tau} parameter for *exponential filtering* detrending.
#'   This must be a positive number. Set this to "auto" to use Nolan's algorithm
#'   to automatically find a suitable value for this parameter (recommended).
#'   For multi-channel images, it is possible to have a different `tau` for each
#'   channel by specifying `tau` as a vector or list.
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
#'   Nolan's algorithm to automatically find a suitable value for this parameter
#'   (recommended). For multi-channel images, it is possible to have a different
#'   `degree` for each channel by specifying `degree` as a vector or list.
#' @param purpose What type of calculation do you intend to perform on the
#'   detrended image? If it is an FFS (fluorescence fluctuation spectroscopy)
#'   calculation (like number and brightness), choose 'FFS'. If it is an FCS
#'   (fluorescence correlation spectroscopy) calculation (like cross-correlated
#'   number and brightness or autocorrelation), choose 'FCS'. The difference is
#'   that if `purpose` is 'FFS', the time series is corrected for non-stationary
#'   mean and variance, whereas if `purpose` is 'FCS', the time series is
#'   corrected for non-stationary mean only.
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
#' img <- ijtiff::read_tif(system.file('extdata', 'bleached.tif',
#'                                     package = 'detrendr'))
#' corrected <- img_detrend_boxcar(img, "auto", purpose = "fcs",
#'                                 seed = 0, parallel = 2)
#' corrected10 <- img_detrend_boxcar(img, 10, purpose = "fcs",
#'                                   seed = 0, parallel = 2)
#' corrected50 <- img_detrend_boxcar(img, 50, purpose = "fcs",
#'                                   seed = 0, parallel = 2)
#' corrected <- img_detrend_exp(img, "auto", purpose = "ffs",
#'                              seed = 0, parallel = 2)
#' corrected10 <- img_detrend_exp(img, 10, purpose = "ffs",
#'                                seed = 0, parallel = 2)
#' corrected50 <- img_detrend_exp(img, 50, purpose = "fcs",
#'                                seed = 0, parallel = 2)
#' corrected <- img_detrend_polynom(img, "auto", purpose = "ffs",
#'                                  seed = 0, parallel = 2)
#' corrected2 <- img_detrend_polynom(img, 2, purpose = "ffs",
#'                                   seed = 0, parallel = 2)}
#' @export
img_detrend_boxcar <- function(img, l, purpose = c("FCS", "FFS"),
                               seed = NULL, parallel = FALSE) {
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  if (filesstrings::all_equal(purpose, c("FCS", "FFS")))
    stop("You must choose *either* 'FCS' or 'FFS' for `purpose`.")
  checkmate::assert_string(purpose)
  purpose %<>% RSAGA::match.arg.ext(c("fcs", "ffs"), ignore.case = TRUE)
  if (length(dim(img)) == 3) dim(img) %<>% {c(.[1:2], 1, .[3])}
  n_ch <- dim(img)[3]
  out <- array(0, dim = dim(img))
  if (is.null(seed)) seed <- rand_seed()
  if (length(l) == 1) l %<>% rep(n_ch)
  if (length(l) != n_ch) {
    stop("Argument l must have length 1 or length equal to ",
         "the number of channels. \n",
         "    * Your l argument has length ", length(l), " and your ",
         "image has ", n_ch, " channels.")
  }
  auto <- rep(FALSE, n_ch)
  for (i in seq_len(n_ch)) {
    if (is.na(l[[i]])) {
      out[, , i, ] <- img[, , i, ]
    } else if (is.numeric(l[[i]])) {
      if (l[[i]] <= 0) {
        stop("`l` must be greater than zero.", "\n",
             "    * You have `l` equal to ", l[[i]], ".")
      }
      if (!isTRUE(checkmate::check_int(l[[i]]))) {
        stop("`l` must be an integer.", "\n",
             "    * You have `l` equal to ", l[[i]], ".")
      }
      out[, , i, ] <- img_detrend_l_specified(img[, , i, ], l[[i]],
                                              purpose = purpose,
                                              seed = seed, parallel = parallel)
    } else if (is.character(l[[i]])) {
      l[[i]] %<>% tolower()
      if (startsWith("auto", l[[i]])) {
        auto[[i]] <- TRUE
        l[[i]] <- best_l(img[, , i, ], seed = seed, parallel = parallel)
        out[, , i, ] <- img_detrend_l_specified(img[, , i, ],
                                                as.numeric(l[[i]]),
                                                purpose = purpose,
                                                seed = seed,
                                                parallel = parallel)
      } else {
        stop("If `l` is a string, the only permissible value is 'auto'.", "\n",
             "    * You have used '", l[[i]], "'.")
      }
    } else {
      stop("`l` must be specified as a positive number or as 'auto'.", "\n",
           "    * You have used '", l[[i]], "'.")
    }
  }
  detrended_img(out, "boxcar", as.integer(unlist(l)), auto, purpose = purpose)
}

#' @rdname detrending
#' @export
img_detrend_exp <- function(img, tau, cutoff = 0.05, purpose = c("FCS", "FFS"),
                            seed = NULL, parallel = FALSE) {
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  if (filesstrings::all_equal(purpose, c("FCS", "FFS")))
    stop("You must choose *either* 'FCS' or 'FFS' for `purpose`.")
  checkmate::assert_string(purpose)
  purpose %<>% RSAGA::match.arg.ext(c("fcs", "ffs"), ignore.case = TRUE)
  if (length(dim(img)) == 3) dim(img) %<>% {c(.[1:2], 1, .[3])}
  n_ch <- dim(img)[3]
  out <- array(0, dim = dim(img))
  if (is.null(seed)) seed <- rand_seed()
  if (length(tau) == 1) tau %<>% rep(n_ch)
  if (length(tau) != n_ch) {
    stop("Argument `tau` must have length 1 or length equal to ",
         "the number of channels. \n",
         "    * Your tau argument has length ", length(tau), " and your ",
         "image has ", n_ch, " channels.")
  }
  auto <- rep(FALSE, n_ch)
  for (i in seq_len(n_ch)) {
    if (is.na(tau[[i]])) {
      out[, , i, ] <- img[, , i, ]
    } else if (is.numeric(tau[[i]])) {
      if (tau[[i]] <= 0) {
        stop("`tau` must be greater than zero.", "\n",
             "    * You have `tau` equal to ", tau[[i]], ".")
      }
      out[, , i, ] <- img_detrend_tau_specified(img[, , i, ], tau[[i]], cutoff,
                                                purpose = purpose,
                                                seed = seed,
                                                parallel = parallel)
    } else if (is.character(tau[[i]])) {
      tau[[i]] %<>% tolower()
      if (startsWith("auto", tau[[i]])) {
        auto[[i]] <- TRUE
        tau[[i]] <- best_tau(img[, , i, ], cutoff = cutoff,
                             seed = seed, parallel = parallel)
        out[, , i, ] <- img_detrend_tau_specified(img[, , i, ],
                                                  as.numeric(tau[[i]]),
                                                  cutoff,
                                                  purpose = purpose,
                                                  seed = seed,
                                                  parallel = parallel)
      } else {
        stop("If `tau` is a string, the only permissible value is 'auto'.",
             "\n", "    * You have used '", tau[[i]], "'.")
      }
    } else {
      stop("`tau` must be specified as a positive number or as 'auto'.",
           "\n", "    * You have used '", tau[[i]], "'.")
    }
  }
  detrended_img(out, "exponential", as.numeric(unlist(tau)), auto,
                purpose = purpose)
}

#' @rdname detrending
#' @export
img_detrend_polynom <- function(img, degree, purpose = c("FCS", "FFS"),
                                seed = NULL, parallel = FALSE) {
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  if (filesstrings::all_equal(purpose, c("FCS", "FFS")))
    stop("You must choose *either* 'FCS' or 'FFS' for `purpose`.")
  checkmate::assert_string(purpose)
  purpose %<>% RSAGA::match.arg.ext(c("fcs", "ffs"), ignore.case = TRUE)
  if (length(dim(img)) == 3) dim(img) %<>% {c(.[1:2], 1, .[3])}
  n_ch <- dim(img)[3]
  out <- array(0, dim = dim(img))
  if (is.null(seed)) seed <- rand_seed()
  if (length(degree) == 1) degree %<>% rep(n_ch)
  if (length(degree) != n_ch) {
    stop("Argument degree must have length 1 or length equal to ",
         "the number of channels. \n",
         "    * Your degree argument has length ", length(degree), " and your ",
         "image has ", n_ch, " channels.")
  }
  auto <- rep(FALSE, n_ch)
  for (i in seq_len(n_ch)) {
    if (is.na(degree[[i]])) {
      out[, , i, ] <- img[, , i, ]
    } else if (is.numeric(degree[[i]])) {
      if (degree[[i]] <= 0) {
        stop("`degree` must be greater than zero.", "\n",
             "    * You have `degree` equal to ", degree[[i]], ".")
      }
      if (!isTRUE(checkmate::check_int(degree[[i]]))) {
        stop("`degree` must be an integer.", "\n",
             "    * You have `degree` equal to ", degree[[i]], ".")
      }
      out[, , i, ] <- img_detrend_degree_specified(img[, , i, ], degree[[i]],
                                                   purpose = purpose,
                                                   seed = seed,
                                                   parallel = parallel)
    } else if (is.character(degree[[i]])) {
      degree[[i]] %<>% tolower()
      if (startsWith("auto", degree[[i]])) {
        degree[[i]] <- best_degree(img[, , i, ],
                                   seed = seed, parallel = parallel)
        out[, , i, ] <- img_detrend_degree_specified(img[, , i, ],
                                                     as.numeric(degree[[i]]),
                                                     purpose = purpose,
                                                     seed = seed,
                                                     parallel = parallel)
      } else {
        stop("If `degree` is a string, the only permissible value is 'auto'.",
             "\n", "    * You have used '", degree[[i]], "'.")
      }
    } else {
      stop("`degree` must be specified as a positive number or as 'auto'.",
           "\n", "    * You have used '", degree[[i]], "'.")
    }
  }
  detrended_img(out, "polynomial", as.numeric(unlist(degree)), auto,
                purpose = purpose)
}
