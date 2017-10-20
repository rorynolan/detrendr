#' Detrended image class.
#'
#' A [detrended_img] is a 3-dimensional array of integers which is the result of
#' a detrending routine. It has 3 attributes: \describe{\item{`method`}{The
#' detrending method used. This must be one of `"boxcar"`, `"exponential"` or
#' `"polynomial"`. } \item{`parameter`}{The value of the parameter used. This
#' will be the `l`, `tau` or `degree` parameter for the respective methods.}
#' \item{`auto`}{A boolean that is `TRUE` if the parameter was found
#' automatically or `FALSE` if it was manually selected. }}
#'
#' @param img The detrended image series. A 3-dimensional array of non-negative
#'   integers.
#' @param method The method used. One of `"boxcar"`, `"exponential"` or
#'   `"polynomial"`.
#' @param parameter A number. The detrend parameter used.
#' @param auto Logical. Was automatic detrending used? This is `NA` if the
#'   detrending parameter is `NA`.
#'
#' @return An object of class `detrended_img`.
#'
#' @export
detrended_img <- function(img, method, parameter, auto) {
  checkmate::check_array(img, d = 3)
  checkmate::check_int(img, lower = 0)
  method %<>% RSAGA::match.arg.ext(c("boxcar", "exponential", "polynomial"),
                                   ignore.case = TRUE)
  attr(img, "method") <- method
  attr(img, "parameter") <- parameter
  attr(img, "auto") <- auto
  class(img) %<>% c("detrended_img", .)
  img
}
