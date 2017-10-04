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

img_detrend_exp <- function(arr3d, tau, cutoff = 0.05,
                            seed = NULL, parallel = FALSE) {
  if (is.null(seed)) seed <- rand_seed()
  checkmate::check_scalar(tau, na.ok = TRUE)
  if (is.na(tau)) {
    radiant.data::set_attr(arr3d, "tau", NA)
  } else if (is.numeric(tau)) {
    if (tau <= 0) stop("tau must be greater than zero.")
    img_detrend_tau_specified(arr3d, tau, cutoff, seed, parallel) %>%
      radiant.data::set_attr("tau", tau)
  } else if (is.character(tau)) {
    tau <- tolower(tau)
    if (startsWith("auto", tau)) {
      tau <- best_tau(arr3d, cutoff = cutoff, seed = seed, parallel = parallel)
      img_detrend_tau_specified(arr3d, tau, cutoff, seed, parallel) %>%
        radiant.data::set_attr("tau", paste("auto", tau))
    } else {
      stop("If tau is a string, the only permissible value is 'auto' whereas ",
           "you have used '", tau, "'.")
    }
  } else {
    stop("tau must be specified as a positive number or as 'auto'.")
  }
}

img_detrend_boxcar <- function(arr3d, l, seed = NULL, parallel = FALSE) {
  if (is.null(seed)) seed <- rand_seed()
  checkmate::check_int(l, na.ok = TRUE)
  if (is.na(l)) {
    radiant.data::set_attr(arr3d, "l", NA)
  } else if (is.numeric(tau)) {
    if (l <= 0) stop("l must be greater than zero.")
    img_detrend_l_specified(arr3d, l, seed, parallel) %>%
      radiant.data::set_attr("l", l)
  } else if (is.character(l)) {
    l <- tolower(l)
    if (startsWith("auto", l)) {
      l <- best_l(arr3d, seed = seed, parallel = parallel)
      img_detrend_tau_specified(arr3d, l, seed, parallel) %>%
        radiant.data::set_attr("l", paste("auto", l))
    } else {
      stop("If tau is a string, the only permissible value is 'auto' whereas ",
           "you have used '", l, "'.")
    }
  } else {
    stop("tau must be specified as a positive number or as 'auto'.")
  }
}
