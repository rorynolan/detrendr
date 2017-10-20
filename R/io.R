#' Read tiff image as array object.
#'
#' Read in a tiff image file from the disk as an array of pixel intensities.
#'
#' This function wraps [tiff::readTIFF()].
#'
#' Thinking of the read image as a matrix `mat`, the pixel at \eqn{x = }`i`,
#' \eqn{y = }`j` has colour based on the value of \code{mat[j, i]} where the
#' \eqn{x} axis points right and the \eqn{y} axis points down. However, when one
#' prints a matrix in a console (or views it in a program such as excel), the
#' value in position \eqn{x = }`i`, \eqn{y = }`j` is from `mat[j, i]`, so if
#' you're confused about a phantom transposition, this is why.
#'
#' If the result has
#'
#' @param path The path to the image file on disk.
#' @param n_ch The number of channels in the image.
#'
#' @return An array of integers representing the image.
#'
#' @examples
#' img <- read_tif(system.file('extdata', 'bleached.tif', package = 'detrendr'),
#'                 n_ch = 1)
#' display(img[, , 1])
#'
#' @export
read_tif <- function(path, n_ch = 1) {
  image_data <- suppressWarnings(tiff::readTIFF(path,
                                                as.is = TRUE, all = TRUE))
  d <- dim(image_data[[1]])
  required_dim <- c(d, length(image_data))
  image_data %<>%
    unlist() %>%
    radiant.data::set_attr("dim", required_dim)
  d <- dim(image_data)
  n_dim <- length(d)
  msg <- paste0("Could not recognise image '", path, "' as one with ",
                n_ch, " channels.")
  if (n_dim == 2 && n_ch != 1) stop(msg)
  if (n_dim == 3) {
    if (n_ch > d[3]) stop(msg)
    if (n_ch > 1 && n_ch < d[3]) {
      if (!d[3] %% n_ch == 0) {
        stop(msg)
      }
      seqs <- purrr::map(seq_len(n_ch), ~ seq(., d[3], by = n_ch))
      image_data <- purrr::map(seqs, ~ image_data[, , .]) %>%
        purrr::reduce(~ abind::abind(.x, .y, along = 4)) %>%
        aperm(c(1, 2, 4, 3))
    }
  }
  d <- dim(image_data)
  if (length(d) == 3 && d[3] == 1) image_data %<>% {.[, , 1]}
  image_data
}

#' Basic image display.
#'
#' Display an image that has been read in by [read_tif()] as it would look in
#' ImageJ. This function wraps [fields::image.plot()].
#'
#' @param img A matrix.
#' @param ... Arguments passed to [fields::image.plot()]. These arguments should
#'   be named.
#' @examples
#' img <- read_tif(system.file('extdata', 'bleached.tif', package = 'detrendr'),
#'                 n_ch = 1)
#' display(img[, , 1])
#'
#' @export
display <- function(img, ...) {
  checkmate::assert_matrix(img)
  img %>% {
    .[rev(seq_len(nrow(.))), ]
  } %>% t() %>%
    fields::image.plot(...)
}

#' Write an integer array to disk as a tiff image.
#'
#' This function allows you to write integer-valued arrays to disk as tiff files
#' as you would want, which is cumbersome with [tiff::writeTIFF()].
#'
#' @param img An integer array.
#' @param file_name The name of the tif file (with or without the .tif) that you
#'   wish to write.
#' @param na How do you want to treat `NA` values? R can only write integer
#'   values (and hence not `NA`s) to tiff pixels. `na = 'saturate'` sets them to
#'   saturated value. `na = 'zero'` sets them to zero, while `na = 'error'` will
#'   give an error if the image contains `NA`s. You can also specify directly
#'   here a natural number (must be between 0 and 2 ^ 32 - 1) to use in place of
#'   `NA`s.
#' @param rds In addition to writing a TIFF file, do you want to save an RDS (a
#'   single R object) file?
#'
#' @examples
#' img <- read_tif(system.file('extdata', 'bleached.tif', package = 'detrendr'),
#'                  n_ch = 1)
#' setwd(tempdir())
#' write_tif(img, 'bleached.tif')
#' suppressWarnings(file.remove(list.files()))  # cleanup
#'
#' @export
write_tif <- function(img, file_name, na = "error", rds = TRUE) {
  to_invisibly_return <- img
  if (!all(can_be_integer(img), na.rm = TRUE)) {
    stop("img must contain only integers")
  }
  if (!all(img >= 0, na.rm = TRUE)) {
    stop("img must not contain negative values")
  }
  stopifnot(length(na) == 1)
  if (is.numeric(na)) {
    stopifnot(na >= 0, na < 2 ^ 32)
    na <- floor(na)
  } else {
    na <- RSAGA::match.arg.ext(na, c("saturate", "zero", "error"),
                               ignore.case = TRUE)
  }
  any_nas <- anyNA(img)
  if (na == "error" && any_nas) stop("img contains NA values.")
  if (is.numeric(na)) img[is.na(img)] <- na
  mx <- max(img, na.rm = TRUE)
  if (mx >= 2 ^ 32) {
    stop("The maximum value in img must be less than 2^32.")
  } else if (mx >= 2 ^ 16) {
    bits_per_sample <- 32
  } else if (mx >= 2 ^ 8) {
    bits_per_sample <- 16
  } else {
    bits_per_sample <- 8
  }
  img <- img / (2 ^ bits_per_sample - 1)
  if (any_nas) {
    if (na == "saturate") img[is.na(img)] <- 1
    if (na == "zero") img[is.na(img)] <- 0
  }
  d <- dim(img)
  ndim <- length(d)
  if (ndim > 4) {
    stop("write_tif() can handle images of at most 4 dimensions. ",
         "Your image has ", ndim, "dimensions.")
  } else if (ndim == 4) {
    for (i in seq_along(d[3])) {
      file_name_i <- filesstrings::give_ext(paste0(file_name, "_ch", i), "tif")
      message("Now writing ", file_name_i, ".")
      tiff::writeTIFF(frames_to_list(img[, , i, ]), file_name_i,
                      bits.per.sample = bits_per_sample)
    }
  } else {
    file_name %<>% filesstrings::give_ext("tif")
    message("Now writing ", file_name, ".")
    tiff::writeTIFF(frames_to_list(img), file_name,
                    bits.per.sample = bits_per_sample)
  }
  if (rds) {
    saveRDS(to_invisibly_return, filesstrings::give_ext(file_name, "RDS",
                                                        replace = TRUE))
  }
  invisible(to_invisibly_return)
}
