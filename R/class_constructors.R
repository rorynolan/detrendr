#' Detrended image class.
#'
#' A [detrended_img] is a 4-dimensional array of positive integers in the style
#' of an [ijtiff_img][ijtiff::ijtiff_img] (indexed by `img[y, x, channel,
#' frame]`) which is the result of a detrending routine. It has 3 attributes:
#' \describe{\item{`method`}{The detrending method used. This must be one of
#' `"boxcar"`, `"exponential"` or `"polynomial"`. } \item{`parameter`}{The value
#' of the parameter used. This will be the `l`, `tau` or `degree` parameter for
#' the respective methods.} \item{`auto`}{A boolean that is `TRUE` if the
#' parameter was found automatically or `FALSE` if it was manually selected. }}
#'
#' Sometimes when detrending, you can get slight negative values in the
#' detrended image. These values should really just be zero, so this constructor
#' function sets negative values of `img` to zero.
#'
#' @param img The detrended image series. A 4-dimensional array of non-negative
#'   integers in the style of an [ijtiff_img][ijtiff::ijtiff_img], or a
#'   3-dimensional array of non-negative integers which represents a single
#'   channel of an [ijtiff_img][ijtiff::ijtiff_img]-style array (indexed by
#'   `img[y, x, frame]`).
#' @param method The method used. One of `"boxcar"`, `"exponential"` or
#'   `"polynomial"`.
#' @param parameter A number. The detrend parameter used. One per channel.
#' @param auto Logical. Was automatic detrending used? One per channel.
#'
#' @return An object of class `detrended_img`.
#'
#' @export
detrended_img <- function(img, method, parameter, auto) {
  checkmate::assert_array(img, min.d = 3, max.d = 4)
  checkmate::assert_numeric(img)
  if (!all.equal(floor(img), img, check.attributes = FALSE)) {
    stop("Elements of a detrended_img must all be integers.")
  }
  if (length(dim(img)) == 3) dim(img) %<>% {c(.[1:2], 1, .[3])}
  img[img < 0] <- 0
  method %<>% RSAGA::match.arg.ext(c("boxcar", "exponential", "polynomial"),
                                   ignore.case = TRUE)
  attr(img, "method") <- method
  attr(img, "parameter") <- parameter
  attr(img, "auto") <- auto
  class(img) %<>% c("detrended_img", .)
  img
}
