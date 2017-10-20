img <- read_tif(system.file("extdata", "bleached.tif", package = "detrendr"),
                n_ch = 1)

test_that("detrending works", {
  skip_on_appveyor()
  skip_on_cran()
  skip_on_travis()
  corrected <- img_detrend_boxcar(img, "auto", seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected)), 4), 1.8061)
  corrected10 <- img_detrend_boxcar(img, 10, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected10)), 4), 1.2669)
  corrected50 <- img_detrend_boxcar(img, 50, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected50)), 4), 2.2282)
  corrected100 <- img_detrend_boxcar(img, 100, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected100)), 4), 3.409)
  corrected300 <- img_detrend_boxcar(img, 300, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected300)), 4), 5.9376)
  corrected <- img_detrend_exp(img, "auto", seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected)), 4), 2.3844)
  corrected10 <- img_detrend_exp(img, 10, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected10)), 4), 1.2922)
  corrected50 <- img_detrend_exp(img, 50, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected50)), 4), 2.5654)
  corrected100 <- img_detrend_exp(img, 100, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected100)), 4), 3.6463)
  corrected1000 <- img_detrend_exp(img, 1000, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected1000)), 4), 10.3631)
  corrected <- img_detrend_polynom(img, "auto", seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected), na.rm = TRUE), 4),
               2.1156)
  corrected1 <- img_detrend_polynom(img, 1, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected1), na.rm = TRUE), 4),
               5.0427)
  corrected2 <- img_detrend_polynom(img, 2, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected2), na.rm = TRUE), 4),
               3.7805)
  corrected4 <- img_detrend_polynom(img, 4, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected4), na.rm = TRUE), 4),
               2.7471)
  corrected8 <- img_detrend_polynom(img, 8, seed = 0, parallel = 2)
  expect_equal(round(mean(brightness_pillars(corrected8), na.rm = TRUE), 4),
               2.0063)
})

test_that("detrending errors correctly", {
  expect_error(img_detrend_boxcar(img, "abc"), "auto")
  expect_error(img_detrend_exp(img, "abc"), "auto")
  expect_error(img_detrend_polynom(img, "abc"), "auto")
  expect_error(img_detrend_boxcar(img, FALSE), "positive number or")
  expect_error(img_detrend_exp(img, FALSE), "positive number or")
  expect_error(img_detrend_polynom(img, FALSE), "positive number or")
})
