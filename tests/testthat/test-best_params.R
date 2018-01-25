context("Automatic parameter finding")

test_that("best_tau works", {
  img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
                                      package = "detrendr"), msg = FALSE)
  expect_equal(round(best_tau(img, seed = 0, parallel = 2)), 34,
               tolerance = 2)
  set.seed(3)
  img <- array(rpois(99 ^ 3, 99), dim = rep(99, 3))
  expect_equal(round(best_tau(img, seed = 7)), 6618, tolerance = 6400)
  img[] <- 0
  expect_error(best_tau(img), "all pixel values are equal to 0")
})

test_that("best_l works", {
  img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
                                      package = "detrendr"), msg = FALSE)
  expect_equal(round(best_l(img, seed = 0, parallel = 2)), 17,
               tolerance = 2)
  set.seed(3)
  img <- array(rpois(99 ^ 3, 99), dim = rep(99, 3))
  expect_equal(round(best_tau(img, seed = 7)), 28372, tolerance = 28200)
  img[] <- 0
  expect_error(best_l(img), "all pixel values are equal to 0")
  img <- array(round(seq(0, .Machine$integer.max, length.out = 2 ^ 3)),
               dim = rep(2, 3))
  expect_error(best_l(img), "Even with.*the most severe detrend")
})

test_that("best_degree works", {
  img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
                                      package = "detrendr"), msg = FALSE)
  best_degree <- suppressWarnings(round(best_degree(img, seed = 0)))
  expect_equal(best_degree, 17, tolerance = 2)
  set.seed(7)
  img <- array(rpois(99 ^ 3, 99), dim = rep(99, 3))
  best_degree <- suppressWarnings(round(best_degree(img, seed = 0)))
  expect_equal(best_degree, NA_real_)
  img[] <- 0
  expect_error(best_degree(img), "all pixel values are equal to 0")
})
