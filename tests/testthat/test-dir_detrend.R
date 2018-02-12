context("Detrend directories")

test_that("detrending entire derectories works", {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  orig_files <- c(system.file("img", "2ch_ij.tif", package = "ijtiff"),
                  system.file("extdata", "bleached.tif", package = "detrendr"))
  file.copy(orig_files, ".")
  orig_imgs <- purrr::map(orig_files, ijtiff::read_tif)
  detrendeds <- purrr::map(orig_imgs, autothresholdr::mean_stack_thresh,
                           method = "tri") %>%
    purrr::map(img_detrend_boxcar, l = "auto", purpose = "ff", seed = 0)
  dir_detrend_boxcar(l = "auto", thresh = "tri", purpose = "ff", seed = 0)
  detrendeds_dir <- dir(pattern = "detrended.*tif", recursive = TRUE) %>%
    purrr::map(ijtiff::read_tif)
  expect_equal(purrr::map(detrendeds, dim), purrr::map(detrendeds_dir, dim))
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir))
  filesstrings::dir.remove("detrended")
  detrendeds <- purrr::map(orig_imgs, autothresholdr::mean_stack_thresh,
                           method = "tri") %>%
    purrr::map(img_detrend_exp, tau = "auto", purpose = "ff", seed = 0)
  dir_detrend_exp(tau = "auto", thresh = "tri", purpose = "ff", seed = 0)
  detrendeds_dir <- dir(pattern = "detrended.*tif", recursive = TRUE) %>%
    purrr::map(ijtiff::read_tif)
  expect_equal(purrr::map(detrendeds, dim), purrr::map(detrendeds_dir, dim))
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir))
  filesstrings::dir.remove("detrended")
  detrendeds <- purrr::map(orig_imgs, autothresholdr::mean_stack_thresh,
                           method = "tri") %>%
    purrr::map(~ suppressWarnings(img_detrend_polynom(., degree = "auto",
                                                      purpose = "ff",
                                                      seed = 0)))
  suppressWarnings(dir_detrend_polynom(degree = "auto", thresh = "tri",
                                       purpose = "ff", seed = 0))
  detrendeds_dir <- dir(pattern = "detrended.*tif", recursive = TRUE) %>%
    purrr::map(ijtiff::read_tif)
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
