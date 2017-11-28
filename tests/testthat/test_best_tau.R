context("Automatic tau finding")

test_that("best_tau works", {
  img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
                                      package = "detrendr"))[, , 1, ]
  expect_true(round(best_tau(img, seed = 0, parallel = 2)) %in% c(32, 34))
})
