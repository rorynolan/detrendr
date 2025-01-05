test_that("best_tau works", {
  skip_on_cran()
  skip_if(getRversion() < "3.6.0")
  img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
    package = "detrendr"
  ), msg = FALSE)
  expect_error(
    best_tau(img),
    paste0(
      "You must choose \\*either\\* 'FCS' \\*or\\* 'FFS' for\\s?",
      "`purpose`."
    )
  )
  set.seed(1)
  expect_equal(best_tau(img, purpose = "ffs"),
    34,
    tolerance = 3
  )
  img_2ch <- abind::abind(img, img, along = 3)
  set.seed(1)
  t1 <- best_tau(img_2ch, purpose = "fcs")
  expect_equal(length(t1), 2)
  set.seed(1)
  t2 <- best_tau(img_2ch, purpose = "fcs")
  expect_equal(t1, t2, tolerance = 1)
  img <- array(0, dim = rep(99, 3))
  expect_error(
    best_tau(img, purpose = "fcs"),
    "all pixel values are equal to 0"
  )
})

test_that("best_l works", {
  skip_on_cran()
  img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
    package = "detrendr"
  ), msg = FALSE)
  expect_error(
    best_l(img),
    paste0(
      "You must choose \\*either\\* 'FCS' \\*or\\* 'FFS' for\\s?",
      "`purpose`."
    )
  )
  set.seed(1)
  expect_equal(round(best_l(img, purpose = "ffs")),
    17,
    tolerance = 2
  )
  img_2ch <- abind::abind(img, img, along = 3)
  set.seed(1)
  l1 <- best_l(img_2ch, purpose = "fcs")
  expect_equal(length(l1), 2)
  expect_equal(mean(l1), 17, tolerance = 2)
  set.seed(4)
  img <- array(rpois(99^3, 99), dim = rep(99, 3))
  bt <- round(best_tau(img, purpose = "ffs"))
  if (!is.na(bt)) {
    expect_equal(bt, 28372, tolerance = 28200)
  }
  img[] <- 0
  expect_error(best_l(img, purpose = "ffs"), "all pixel values are equal to 0")
  img <- array(round(seq(0, .Machine$integer.max, length.out = 2^3)),
    dim = rep(2, 3)
  )
  expect_error(
    best_l(img, purpose = "ffs"),
    "Even with.*the most severe detrend"
  )
  skip_if_not_installed("abind")
})

test_that("best_swaps() works", {
  skip_on_cran()
  img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
    package = "detrendr"
  ))
  set.seed(1)
  expect_equal(best_swaps(img, quick = TRUE), 1588366, tolerance = 4000)
  expect_error(
    best_swaps(array(7, dim = rep(1, 4))),
    paste(
      "Your image is constant: all pixel values are equal to 7.+",
      "This type of image is not detrendable."
    )
  )
  for (i in seq_len(77)) { # ensure covering brightness part of `best_swaps()`
    set.seed(i)
    sim_img <- array(rpois(6^4, 8), dim = rep(6, 4))
    expect_lt(
      sum(best_swaps(sim_img, quick = TRUE)),
      sum(sim_img) * dim(sim_img)[3]
    )
  }
})
