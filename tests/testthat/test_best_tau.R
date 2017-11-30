context("Automatic tau finding")

test_that("best_tau works", {
  skip_on_os("windows")
  img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
                                      package = "detrendr"))
  expect_true(round(best_tau(img, seed = 0, parallel = 2)) %in% c(32, 33, 34))
})
