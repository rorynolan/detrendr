#' Detrend all TIFF images in an entire folder.
#'
#' Batch processing. Apply any of the available detrending routines to detrend
#' all of the TIFF images in a folder, saving the detrended images as TIFF files
#' in the same folder.
#'
#' These functions include a thresholding option, unlike their non-batch
#' processing counterparts which they wrap (i.e. [img_detrend_boxcar],
#' [img_detrend_exp] and [img_detrend_polynom]). This is because, when working
#' interactively, it's easy to threshold and then detrend, but for batch
#' processing, it's not so easy to efficiently do one after the other, so it's
#' nice to have that taken care of should you want it.
#'
#' @param folder_path The path (relative or absolute) to the folder you wish to
#'   process.
#' @inheritParams detrending
#' @param thresh The threshold or thresholding method (see
#'   [autothresholdr::mean_stack_thresh()]) to use on the image prior to
#'   detrending.
#' @param msg Receive messages to tell you how the processing of the directory is
#'   going? Default is yes.
#'
#' @return Silently, a character vector of the paths to the detrended images.
#'
#' @name detrend-directory
#'
#' @examples
#' \dontrun{
#' setwd(tempdir())
#' file.copy(c(system.file("extdata", "bleached.tif", package = "detrendr"),
#'             system.file("img", "2ch_ij.tif", package = "ijtiff")),
#'           ".")
#' dir_detrend_boxcar(l = "auto", thresh = "tri", purpose = "FFS")
#' dir_detrend_exp(tau = "auto", thresh = "tri", purpose = "FCS")
#' dir_detrend_polynom(degree = "auto", thresh = "huang", purpose = "FFS")}
NULL

#' @rdname detrend-directory
#' @export
dir_detrend_boxcar <- function(folder_path = ".", l, purpose = c("FCS", "FFS"),
                               thresh = NULL,
                               seed = NULL, parallel = FALSE, msg = TRUE) {
  checkmate::assert_directory_exists(folder_path)
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(folder_path)
  tiffs <- list.files(pattern = "\\.tiff*")
  purrr::map_chr(tiffs, file_detrend, "box", parameter = l,
                 purpose = purpose, thresh = thresh,
                 seed = seed, parallel = parallel, msg = msg) %>%
    invisible()
}

#' @rdname detrend-directory
#' @export
dir_detrend_exp <- function(folder_path = ".", tau, purpose = c("FCS", "FFS"),
                            thresh = NULL,
                            seed = NULL, parallel = FALSE, msg = TRUE) {
  checkmate::assert_directory_exists(folder_path)
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(folder_path)
  tiffs <- list.files(pattern = "\\.tiff*")
  purrr::map_chr(tiffs, file_detrend, "exp", parameter = tau,
                 purpose = purpose, thresh = thresh,
                 seed = seed, parallel = parallel, msg = msg) %>%
    invisible()
}

#' @rdname detrend-directory
#' @export
dir_detrend_polynom <- function(folder_path = ".", degree,
                                purpose = c("FCS", "FFS"), thresh = NULL,
                                seed = NULL, parallel = FALSE, msg = TRUE) {
  checkmate::assert_directory_exists(folder_path)
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(folder_path)
  tiffs <- list.files(pattern = "\\.tiff*")
  purrr::map_chr(tiffs, file_detrend, "poly", parameter = degree,
                 purpose = purpose,
                 thresh = thresh, seed = seed, parallel = parallel,
                 msg = msg) %>%
    invisible()
}

file_detrend <- function(path, method, parameter,
                         purpose = c("FCS", "FFS"), thresh = NULL,
                         seed = NULL, parallel = FALSE, msg = TRUE) {
  checkmate::assert_file_exists(path)
  checkmate::assert_string(method)
  method %<>% RSAGA::match.arg.ext(c("boxcar", "exponential", "polynomial"),
                                   ignore.case = TRUE)
  need_to_change_dir <- stringr::str_detect(path, "/")
  if (need_to_change_dir) {
    dir <- filesstrings::str_before_last(path, "/")
    cwd <- getwd()
    on.exit(setwd(cwd))
    setwd(dir)
    path %<>% filesstrings::str_after_last("/")
  }
  img <- ijtiff::read_tif(path, msg = msg)
  if (msg) message("Detrending ", path, " . . .")
  if (!is.null(thresh)) {
    img %<>% autothresholdr::mean_stack_thresh(thresh)
    thresh <- attr(img, "thresh")
  }
  if (method == "boxcar")
    img %<>% img_detrend_boxcar(l = parameter, purpose = purpose,
                                seed = seed, parallel = parallel)
  if (method == "exponential")
    img %<>% img_detrend_exp(tau = parameter, purpose = purpose,
                             seed = seed, parallel = parallel)
  if (method == "polynomial") {
    img %<>% img_detrend_polynom(degree = parameter, purpose = purpose,
                                 seed = seed, parallel = parallel)
  }
  if (msg) message("\b Done.")
  if (!is.null(thresh)) attr(img, "thresh") <- thresh
  filename_start <- filesstrings::before_last_dot(path)
  filename_end <- make_detrended_filename_ending(img)
  suppressMessages(filesstrings::create_dir("detrended"))
  path <- paste0("detrended", "/", filename_start, filename_end, ".tif")
  ijtiff::write_tif(img, path, msg = msg)
  path
}



make_thresh_filename_part <- function(img) {
  stopifnot("thresh" %in% names(attributes(img)))
  thresh <- attr(img, "thresh")
  if (is.list(thresh)) {
    threshs <- unlist(thresh)
    methods <- purrr::map(thresh, attr, "autothresh_method") %T>% {
      for (i in seq_along(.)) if (is.null(.[[i]])) .[[i]] <- NA
    } %>%
      unlist()
    paste0("thresh=",
           paste0(ifelse(is.na(methods), "", paste0(methods, "=")), threshs) %>%
             paste(collapse = ","), "_")
  } else {
    make_thresh_filename_part(rlang::set_attrs(0, thresh = list(thresh)))
  }
}

make_detrended_filename_ending <- function(img) {
  checkmate::assert_class(img, "detrended_img")
  method <- attr(img, "method")
  parameter <- attr(img, "parameter")
  stopifnot(method %in% c("boxcar", "exponential", "polynomial"))
  symbol <- switch(method, boxcar = "l", exponential = "tau",
                   polynomial = "degree")
  checkmate::assert_string(symbol)
  auto <- attr(img, "auto")
  purpose <- attr(img, "purpose")
  stopifnot(purpose %in% c("FCS", "FFS"))
  thresh_part <- ""
  if ("thresh" %in% names(attributes(img)))
    thresh_part <- make_thresh_filename_part(img)
  paste0("_", method, "_detrended_for_", purpose, "_", thresh_part, symbol, "=",
         paste(parameter, collapse = ","))
}

