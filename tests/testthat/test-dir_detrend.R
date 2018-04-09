context("Detrend directories")

test_that("detrending entire derectories works", {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  orig_files <- c(system.file("img", "2ch_ij.tif", package = "ijtiff"),
                  system.file("extdata", "bleached.tif", package = "detrendr"))
  file.copy(orig_files, ".")
  orig_imgs <- purrr::map(orig_files, ijtiff::read_tif, msg = FALSE)
  detrendeds <- NA
  detrendeds <- try(stop("eee"), silent = TRUE)
  set.seed(1)
  while (class(detrendeds) == "try-error") {
    set.seed(get_seed())
    detrendeds <- tryCatch(purrr::map(orig_imgs,
                                      autothresholdr::mean_stack_thresh,
                                      method = "tri") %>%
                             purrr::map(img_detrend_boxcar, l = "auto",
                                        purpose = "ff"),
                           error = function(e) {
                             err_msg <- conditionMessage(e)
                             if (!stringr::str_detect(err_msg,
                                                      "Even .* most severe d"))
                               stop("unexpected error")
                           }
    )
  }
  eee <- try(stop("eee"), silent = TRUE)
  set.seed(1)
  while (class(eee) == "try-error") {
    set.seed(get_seed())
    eee <- tryCatch(dir_detrend_boxcar(l = "auto", thresh = "tri",
                                       purpose = "ff", msg = FALSE),
                    error = function(e) {
                      err_msg <- conditionMessage(e)
                      if (!stringr::str_detect(err_msg,
                                               "Even .* most severe detrend")) {
                        stop("unexpected error")
                      }
                    }
    )
  }
  detrendeds_dir <- dir(pattern = "detrended.*tif", recursive = TRUE) %>%
    purrr::map(ijtiff::read_tif, msg = FALSE)
  expect_equal(purrr::map(detrendeds, dim), purrr::map(detrendeds_dir, dim))
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir))
  filesstrings::dir.remove("detrended")
  set.seed(1)
  detrendeds <- purrr::map(orig_imgs, autothresholdr::mean_stack_thresh,
                           method = "tri") %>%
    purrr::map(img_detrend_exp, tau = "auto", purpose = "ff")
  set.seed(1)
  dir_detrend_exp(tau = "auto", thresh = "tri", purpose = "ff",
                              msg = FALSE)
  detrendeds_dir <- dir(pattern = "detrended.*tif", recursive = TRUE) %>%
    purrr::map(ijtiff::read_tif, msg = FALSE)
  expect_equal(purrr::map(detrendeds, dim), purrr::map(detrendeds_dir, dim))
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir))
  filesstrings::dir.remove("detrended")
  set.seed(1)
  detrendeds <- purrr::map(orig_imgs, autothresholdr::mean_stack_thresh,
                           method = "tri") %>%
    purrr::map(~ suppressWarnings(img_detrend_polynom(., degree = "auto",
                                                      purpose = "ff")))
  set.seed(1)
  suppressWarnings(dir_detrend_polynom(degree = "auto", thresh = "tri",
                                       purpose = "ff", msg = FALSE))
  detrendeds_dir <- dir(pattern = "detrended.*tif", recursive = TRUE) %>%
    purrr::map(ijtiff::read_tif, msg = FALSE)
  expect_equal(purrr::map(detrendeds, dim), purrr::map(detrendeds_dir, dim))
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir))
  filesstrings::dir.remove("detrended")
  file.remove(dir(pattern = "tif$"))
})

test_that("file_detrend() deals with other directories correctly", {
  setwd(tempdir())
  filesstrings::create_dir("tempwithintemp")
  file.copy(system.file("extdata", "bleached.tif", package = "detrendr"),
            "tempwithintemp")
  file_detrend("tempwithintemp/bleached.tif", method = "exp", parameter = 5,
               purpose = "ff")
})
