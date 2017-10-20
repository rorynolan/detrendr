img <- read_tif(system.file("extdata", "bleached.tif", package = "detrendr"),
                n_ch = 1)

test_that("detrending works", {
  skip_on_appveyor()
  skip_on_cran()
  skip_on_travis()
  corrected <- img_detrend_boxcar(img, "auto", seed = 0, parallel = 2)
  expect_true(round(mean(brightness_pillars(corrected)), 1) %in% c(1.8, 1.9))
  corrected10 <- img_detrend_boxcar(img, 10, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected10)), 1), 1.3)
  corrected50 <- img_detrend_boxcar(img, 50, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected50)), 1), 2.2)
  corrected100 <- img_detrend_boxcar(img, 100, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected100)), 1), 3.4)
  corrected300 <- img_detrend_boxcar(img, 300, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected300)), 1), 5.9)
  corrected <- img_detrend_exp(img, "auto", seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected)), 1), 2.4)
  corrected10 <- img_detrend_exp(img, 10, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected10)), 1), 1.3)
  corrected50 <- img_detrend_exp(img, 50, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected50)), 1), 2.6)
  corrected100 <- img_detrend_exp(img, 100, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected100)), 1), 3.6)
  corrected1000 <- img_detrend_exp(img, 1000, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected1000)), 1), 10.4)
  corrected <- img_detrend_polynom(img, "auto", seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected), na.rm = TRUE), 1),
               2.1)
  corrected1 <- img_detrend_polynom(img, 1, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected1), na.rm = TRUE), 1),
               5.0)
  corrected2 <- img_detrend_polynom(img, 2, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected2), na.rm = TRUE), 1),
               3.8)
  corrected4 <- img_detrend_polynom(img, 4, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected4), na.rm = TRUE), 1),
               2.7)
  corrected8 <- img_detrend_polynom(img, 8, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected8), na.rm = TRUE), 1),
               2.0)
})

test_that("detrending errors correctly", {
  expect_error(img_detrend_boxcar(img, "abc"), "auto")
  expect_error(img_detrend_exp(img, "abc"), "auto")
  expect_error(img_detrend_polynom(img, "abc"), "auto")
  expect_error(img_detrend_boxcar(img, FALSE), "positive number or")
  expect_error(img_detrend_exp(img, FALSE), "positive number or")
  expect_error(img_detrend_polynom(img, FALSE), "positive number or")
})
