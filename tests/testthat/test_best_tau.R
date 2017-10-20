test_that("best_tau works", {
  skip_on_appveyor()
  skip_on_cran()
  skip_on_travis()
  img <- read_tif(system.file("extdata", "bleached.tif",
    package = "detrendr"), n_ch = 1)
  expect_true(round(best_tau(img, seed = 0, parallel = 2)) %in% 42:46)
})
