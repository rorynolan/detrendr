test_that("best_tau works", {
  img <- read_tif(system.file("extdata", "bleached.tif",
    package = "detrendr"), n_ch = 1)
  expect_equal(round(best_tau(img, seed = 0, parallel = 2)), 44)
})
