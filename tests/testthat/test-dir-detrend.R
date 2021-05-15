test_that("detrending entire derectories works", {
  skip_if(getRversion() < "3.6")
  skip_on_cran()
  cwd <- setwd(tempdir(check = TRUE))
  on.exit(setwd(cwd))
  orig_files <- c(
    system.file("extdata", "2ch_ij.tif", package = "detrendr"),
    system.file("extdata", "bleached.tif", package = "detrendr")
  )
  file.copy(orig_files, ".")
  orig_imgs <- purrr::map(orig_files, ijtiff::read_tif, msg = FALSE) %>%
    magrittr::set_names(basename(orig_files))
  detrendeds <- try(stop("eee"), silent = TRUE)
  set.seed(1)
  while (class(detrendeds) == "try-error") {
    set.seed(get_seed())
    detrendeds <- tryCatch(purrr::map(orig_imgs,
      autothresholdr::mean_stack_thresh,
      method = "tri"
    ) %>%
      purrr::map(img_detrend_boxcar,
        l = "auto",
        purpose = "ff"
      ),
    error = function(e) {
      err_msg <- conditionMessage(e)
      if (!stringr::str_detect(
        err_msg,
        "Even.*most severe d"
      )) {
        stop(err_msg)
      } else {
        structure(err_msg, class = "try-error")
      }
    }
    )
  }
  eee <- try(stop("eee"), silent = TRUE)
  set.seed(1)
  while (class(eee) == "try-error") {
    set.seed(get_seed())
    eee <- tryCatch(dir_detrend_boxcar(
      l = "auto", thresh = "tri",
      purpose = "ff", msg = FALSE
    ),
    error = function(e) {
      err_msg <- conditionMessage(e)
      if (!stringr::str_detect(
        err_msg,
        "Even .* most severe detrend"
      )) {
        stop("unexpected error")
      } else {
        structure(err_msg, class = "try-error")
      }
    }
    )
  }
  detrendeds_dir <- dir(pattern = "detrended.*tif", recursive = TRUE) %>%
    purrr::map(ijtiff::read_tif, msg = FALSE)
  expect_equal(
    purrr::map(detrendeds, dim),
    purrr::map(detrendeds_dir, dim),
    ignore_attr = TRUE
  )
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir),
    tolerance = 1,
    ignore_attr = TRUE
  )
  expect_match(
    dir("detrended")[1],
    paste0(
      "2ch_ij_detrended_",
      "thresh=Triangle=\\d+\\.?\\d*,Triangle=\\d+\\.?\\d*_",
      "boxcar_for_FFS_l=auto=(NA|\\d+),auto=(NA|\\d+).tif"
    )
  )
  expect_match(
    dir("detrended")[2],
    paste0(
      "bleached_detrended_",
      "thresh=Triangle=\\d+\\.?\\d*_",
      "boxcar_for_FFS_l=auto=\\d+.tif"
    )
  )
  suppressWarnings(
    expect_warning(
      dir_detrend_boxcar("detrended", l = 9, purpose = "fcs"),
      "already detrended"
    )
  )
  filesstrings::dir.remove("detrended")
  set.seed(1)
  detrendeds <- purrr::map(orig_imgs, autothresholdr::mean_stack_thresh,
    method = "tri"
  ) %>%
    purrr::map(img_detrend_exp, tau = "auto", purpose = "ff")
  set.seed(1)
  dir_detrend_exp(
    tau = "auto", thresh = "tri", purpose = "ff",
    msg = FALSE
  )
  detrendeds_dir <- dir(pattern = "detrended.*tif", recursive = TRUE) %>%
    purrr::map(ijtiff::read_tif, msg = FALSE)
  expect_equal(
    purrr::map(detrendeds, dim),
    purrr::map(detrendeds_dir, dim),
    ignore_attr = TRUE
  )
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir),
    tolerance = 1, ignore_attr = TRUE
  )
  expect_match(
    dir("detrended")[1],
    paste0(
      "2ch_ij_detrended_",
      "thresh=Triangle=\\d+\\.?\\d*,Triangle=\\d+\\.?\\d*_",
      "exponential_for_FFS_tau=auto=(NA|\\d+\\.?\\d*),auto=(NA|\\d+\\.?\\d*)",
      ".tif"
    )
  )
  expect_match(
    dir("detrended")[2],
    paste0(
      "bleached_detrended_",
      "thresh=Triangle=\\d+\\.?\\d*_",
      "exponential_for_FFS_tau=auto=\\d+\\.?\\d*.tif"
    )
  )
  filesstrings::dir.remove("detrended")
  set.seed(1)
  detrendeds <- purrr::map(orig_imgs, autothresholdr::mean_stack_thresh,
    method = "tri"
  ) %>%
    purrr::map(img_detrend_rh, swaps = 222)
  set.seed(1)
  dir_detrend_rh(swaps = 222, thresh = "tri", msg = FALSE)
  detrendeds_dir <- dir(pattern = "detrended.*tif$", recursive = TRUE) %>%
    purrr::map(ijtiff::read_tif, msg = FALSE)
  expect_equal(
    purrr::map(detrendeds, dim),
    purrr::map(detrendeds_dir, dim),
    ignore_attr = TRUE
  )
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir), ignore_attr = TRUE)
  expect_match(
    dir("detrended")[1],
    paste0(
      "2ch_ij_detrended_",
      "thresh=Triangle=\\d+\\.?\\d*,Triangle=\\d+\\.?\\d*_",
      "robinhood_swaps=222,222.tif"
    )
  )
  expect_match(
    dir("detrended")[2],
    paste0(
      "bleached_detrended_",
      "thresh=Triangle=\\d+\\.?\\d*_",
      "robinhood_swaps=222.tif"
    )
  )
  filesstrings::dir.remove("detrended")
  set.seed(1)
  file.remove("2ch_ij.tif")
  detrendeds <- purrr::map(
    orig_imgs[list.files(pattern = ".tif$")],
    autothresholdr::mean_stack_thresh,
    method = "tri"
  ) %>%
    purrr::map(~ suppressWarnings(img_detrend_polynom(.,
      degree = 2,
      purpose = "ff"
    )))
  set.seed(1)
  suppressWarnings(dir_detrend_polynom(
    degree = 2, thresh = "tri",
    purpose = "ff", msg = FALSE
  ))
  detrendeds_dir <- dir(pattern = "detrended.*tif", recursive = TRUE) %>%
    purrr::map(ijtiff::read_tif, msg = FALSE)
  expect_equal(
    purrr::map(detrendeds, dim),
    purrr::map(detrendeds_dir, dim),
    ignore_attr = TRUE
  )
  expect_equal(unlist(detrendeds), unlist(detrendeds_dir),
    tolerance = 1, ignore_attr = TRUE
  )
  expect_match(
    dir("detrended"),
    paste0(
      "bleached_detrended_",
      "thresh=Triangle=\\d+\\.?\\d*_polynomial_for_FFS_degree=2.tif"
    )
  )
  filesstrings::dir.remove("detrended")
  file.remove(dir(pattern = "tif$"))
  setwd(cwd)
})

test_that("file_detrend() deals with other directories correctly", {
  skip_on_cran()
  setwd(tempdir())
  filesstrings::create_dir("tempwithintemp")
  file.copy(
    system.file("extdata", "bleached.tif", package = "detrendr"),
    "tempwithintemp"
  )
  file_detrend("tempwithintemp/bleached.tif",
    method = "exp", parameter = 5,
    purpose = "ff"
  )
})

test_that("`make_thresh_filename_part()` works", {
  expect_equal(
    make_thresh_filename_part(structure(0, thresh = 1)),
    "thresh=1_"
  )
})
