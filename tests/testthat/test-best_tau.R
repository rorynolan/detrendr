context("Automatic tau finding")

test_that("best_tau works", {
  img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
                                      package = "detrendr"), msg = FALSE)
  expect_equal(round(best_tau(img, seed = 0, parallel = 2)), 34,
               tolerance = 2)
  set.seed(3)
  img <- array(rpois(99 ^ 3, 99), dim = rep(99, 3))
  expect_equal(round(best_tau(img, seed = 7)), 6618, tolerance = 6000)
})
