#' Perform swaps on a single-point fcs trace.
#'
#' Before a swap is made:
#' * A point's probability of losing a count is proportional to its distance
#' from the mean intensity where points below mean intensity do not lose counts.
#' * A point's probability of gaining a count is proportional to its distance
#' from the mean intensity where points above mean intensity do not gain counts.
#'
#' @param trace A vector of non-negative integers.
#' @param swaps A positive integer.
#' @param losing_weights Relative probabilities of losing a count.
#' @param getting_weights Relative probabilities of gaining a count.
#'
#' @return A vector of non-negative integers. The detrended trace.
#'
#' @noRd
single_point_perform_swaps <- function(trace, swaps,
                                       losing_weights, getting_weights) {
  checkmate::assert_int(swaps)
  checkmate::assert_integerish(trace)
  l <- length(trace)
  checkmate::assert_numeric(losing_weights, lower = 0, len = l)
  losing <- rfromboxes(swaps, trace, losing_weights)
  capacities <- trace %>% {
      mean(.) - .
    } %>%
    ceiling(.) %>%
    sigmoid::relu()
  getting <- rtoboxes(swaps, l, getting_weights, capacities)
  trace - losing + getting
}

#' Calculate the ideal number of swaps to detrend a single-point FCS trace.
#'
#' Calculate a smooth cubic spline through the trace to get an idea of the local
#' mean at each point. Then simulate a new trace with the same expected local
#' means using the Poisson distribution. This trace is now simulated as having
#' come from immobile particles only. See how many swaps are needed on this
#' trace to detrend it to having brightness \eqn{B = 1}.
#'
#' @param trace A vector of non-negative integers.
#'
#' @return A non-negative integer. The number of swaps.
#'
#' @export
single_point_best_swaps <- function(trace) {
  checkmate::assert_integerish(trace)
  naives <- purrr::rerun(9, single_point_best_swaps_naive(trace)) %>%
    purrr::map_int(1)
  consts <- trace %>% {
    purrr::rerun(9, single_point_best_swaps_naive(rpois(length(.), mean(.))))
  } %>%
    purrr::map_int(1)
  sigmoid::relu(floor(median(naives)) - floor(median(consts)))
}

single_point_best_swaps_naive <- function(trace) {
  checkmate::assert_integerish(trace)
  has_autocorr <- cor.test(trace, dplyr::lag(trace))$p.value < 0.05
  smoothed <- stats::smooth.spline(trace)$y
  simulated <- myrpois(smoothed)
  while (length(unique(simulated)) == 1) {
    simulated <- myrpois(smoothed)
  }
  if (brightness_vec(simulated) <= 1) return(0L)
  sim_mean_diff <- simulated %>% {
    . - mean(.)
  }
  losing_weights <- sigmoid::relu(sim_mean_diff)
  getting_weights <- sigmoid::relu(-sim_mean_diff)
  max_swaps <- min(sum(losing_weights), sum(getting_weights)) %>% floor(.)
  less <- more <- simulated
  currently_swapping <- 1
  least_swapped <- most_swapped <- 0
  while (brightness_vec(more) > 1) {
    if (most_swapped >= max_swaps) {
      custom_stop("
        The maximum number of swaps have been made but more still need
        to be made.
        ", "
        This error should never occur, but it has.
        ", "
        Please file a bug report at
        https://www.github.com/rorynolan/detrendr/issues.
        ")
    }
    less <- more
    more %<>% single_point_perform_swaps(
      currently_swapping,
      losing_weights, getting_weights
    )
    least_swapped <- most_swapped
    most_swapped %<>% {
      . + currently_swapping
    }
    currently_swapping %<>% {
      min(2 * ., max_swaps - most_swapped)
    }
  }
  while (most_swapped - least_swapped > 1) {
    middle_swapped <- floor(mean(c(least_swapped, most_swapped)))
    middle <- single_point_perform_swaps(
      less, middle_swapped - least_swapped,
      losing_weights, getting_weights
    )
    if (brightness_vec(middle) < 1) {
      most_swapped <- middle_swapped
      more <- middle
    } else if (brightness_vec(middle) > 1) {
      least_swapped <- middle_swapped
      less <- middle
    } else {
      return(as.integer(middle_swapped)) # probably never happens
    }
  }
  if (abs(brightness_vec(less) - 1) <= abs(brightness_vec(more) - 1)) {
    return(as.integer(least_swapped))
  } else {
    return(as.integer(most_swapped))
  }
}

#' Detrend a single-point FCS trace with the Robin Hood method.
#'
#' Calculate a smooth cubic spline through the trace to get an idea of the local
#' mean at each point. Then simulate a new trace with the same expected local
#' means using the Poisson distribution. This trace is now simulated as having
#' come from immobile particles only. See how many swaps are needed on this
#' trace to detrend it to having brightness \eqn{B = 1}. Then apply this many
#' swaps to the original trace.
#'
#' @param trace A vector of non-negative integers.
#'
#' @return A vector of non-negative integers. The detrended trace.
#'
#' @export
single_point_detrend_rh <- function(trace) {
  swaps <- single_point_best_swaps(trace)
  weights <- trace %>% {
    . - mean(.)
  }
  losing_weights <- sigmoid::relu(weights)
  getting_weights <- sigmoid::relu(-weights)
  single_point_perform_swaps(trace, swaps, losing_weights, getting_weights)
}
