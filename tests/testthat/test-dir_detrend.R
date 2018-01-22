context("Detrend directories")

test_that("detrending entire derectories works", {
  library(magrittr)
  setwd(tempdir())
  orig_files <- c(system.file("img", "2ch_ij.tif", package = "ijtiff"),
                  system.file("extdata", "bleached.tif", package = "detrendr"))
  file.copy(orig_files, ".")
  orig_imgs <- purrr::map(orig_files, ijtiff::read_tif)
  detrendeds <- purrr::map(orig_imgs, autothresholdr::mean_stack_thresh,
                           method = "tri") %>%
    purrr::map(img_detrend_boxcar, l = "auto", seed = 0)
  dir_detrend_boxcar(l = "auto", thresh = "tri", seed = 0)
  detrendeds_dir <- dir(pattern = "detrended.*tif") %>%
    purrr::map(ijtiff::read_tif)
  expect_equal(purrr::map(detrendeds, dim), purrr::map(detrendeds_dir, dim))
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir))
  file.remove(dir(pattern = "detrended.*tif$"))
  detrendeds <- purrr::map(orig_imgs, autothresholdr::mean_stack_thresh,
                           method = "tri") %>%
    purrr::map(img_detrend_exp, tau = "auto", seed = 0)
  dir_detrend_exp(tau = "auto", thresh = "tri", seed = 0)
  detrendeds_dir <- dir(pattern = "detrended.*tif") %>%
    purrr::map(ijtiff::read_tif)
  expect_equal(purrr::map(detrendeds, dim), purrr::map(detrendeds_dir, dim))
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir))
  file.remove(dir(pattern = "detrended.*tif$"))
  detrendeds <- purrr::map(orig_imgs, autothresholdr::mean_stack_thresh,
                           method = "tri") %>%
    purrr::map(~ suppressWarnings(img_detrend_polynom(., degree = "auto",
                                                      seed = 0)))
  suppressWarnings(dir_detrend_polynom(degree = "auto", thresh = "tri",
                                       seed = 0))
  detrendeds_dir <- dir(pattern = "detrended.*tif") %>%
    purrr::map(ijtiff::read_tif)
  expect_equal(purrr::map(detrendeds, dim), purrr::map(detrendeds_dir, dim))
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir))
  file.remove(dir(pattern = "tif$"))
})
