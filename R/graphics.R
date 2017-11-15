#' Basic image display.
#'
#' Display an image that has been read in by [read_tif()] as it would look in
#' ImageJ. This function wraps [fields::image.plot()].
#'
#' @param img A matrix.
#' @param ... Arguments passed to [fields::image.plot()]. These arguments should
#'   be named.
#' @examples
#' img <- read_tif(system.file('extdata', 'bleached.tif', package = 'detrendr'))
#' display(img[, , 1])
#'
#' @export
display <- function(img, ...) {
  checkmate::assert_matrix(img)
  img %>% {
    .[rev(seq_len(nrow(.))), ]
  } %>% t() %>%
    fields::image.plot(..., axes = FALSE)
}
