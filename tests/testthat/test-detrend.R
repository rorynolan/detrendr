img <- ijtiff::read_tif(system.file("extdata", "bleached.tif",
                                    package = "detrendr"), msg = FALSE)

test_that("detrending works", {
  context("Boxcar detrending")
  corrected <- img_detrend_boxcar(img[, , 1, ], "auto", seed = 0, parallel = 2,
                                  purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected[, , 1, ])), 2), 1.45,
               tolerance = 0.15)
  corrected10 <- img_detrend_boxcar(img, 10, seed = 0, parallel = 2,
                                    purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected10[, , 1, ])), 2), 1.27)
  corrected50 <- img_detrend_boxcar(img, 50, seed = 0, parallel = 2,
                                    purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected50[, , 1, ])), 2), 2.08)
  corrected100 <- img_detrend_boxcar(img, 100, seed = 0, parallel = 2,
                                     purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected100[, , 1, ])), 2), 3.04)
  corrected300 <- img_detrend_boxcar(img, 300, seed = 0, parallel = 2,
                                     purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected300[, , 1, ])), 2), 5.6)
  correctedNA <- img_detrend_boxcar(img, NA, purpose = "ff")
  expect_equal(list(dim(correctedNA), as.vector(correctedNA)),
               list(dim(img), as.vector(img)))
  context("Exponential filtering detrending")
  corrected <- img_detrend_exp(img[, , 1, ], "auto", seed = 0, parallel = 2,
                               purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected[, , 1, ])), 2), 1.64,
               tolerance = 0.1)
  corrected10 <- img_detrend_exp(img, 10, seed = 0, parallel = 2,
                                 purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected10[, , 1, ])), 2), 1.29)
  corrected50 <- img_detrend_exp(img, 50, seed = 0, parallel = 2,
                                 purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected50[, , 1, ])), 2), 2.38)
  corrected100 <- img_detrend_exp(img, 100, seed = 0, parallel = 2,
                                  purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected100[, , 1, ])), 2), 3.38)
  corrected1000 <- img_detrend_exp(img, 1000, seed = 0, parallel = 2,
                                   purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected1000[, , 1, ])), 2), 6.01)
  correctedNA <- img_detrend_exp(img, NA, purpose = "ff")
  expect_equal(list(dim(correctedNA), as.vector(correctedNA)),
               list(dim(img), as.vector(img)))
  context("Polynomial detrending")
  expect_warning(img_detrend_polynom(img, "auto", seed = 0, parallel = 2,
                                     purpose = "ff"),
                 "polynomial degree")
  corrected <- suppressWarnings(img_detrend_polynom(img, "auto", purpose = "ff",
                                                    seed = 0, parallel = 2))
  expect_equal(round(mean(brightness_pillars(corrected[, , 1, ]),
                         na.rm = TRUE), 2),
               1.6, tolerance = 0.15)
  corrected1 <- img_detrend_polynom(img[, , 1, ], 1, seed = 0, parallel = 2,
                                    purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected1[, , 1, ]),
                          na.rm = TRUE), 1), 4.5)
  corrected2 <- img_detrend_polynom(img, 2, seed = 0, parallel = 2,
                                    purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected2[, , 1, ]),
                          na.rm = TRUE), 1), 3.5)
  corrected4 <- img_detrend_polynom(img, 4, seed = 0, parallel = 2,
                                    purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected4[, , 1, ]),
                          na.rm = TRUE), 1), 2.6)
  corrected8 <- img_detrend_polynom(img, 8, seed = 0, parallel = 2,
                                    purpose = "ff")
  expect_equal(round(mean(brightness_pillars(corrected8[, , 1, ]),
                          na.rm = TRUE), 1), 1.9)
  correctedNA <- img_detrend_polynom(img, NA, purpose = "ff")
  expect_equal(list(dim(correctedNA), as.vector(correctedNA)),
               list(dim(img), as.vector(img)))
})

context("Detrending errors")
test_that("detrending errors correctly", {
  expect_error(img_detrend_boxcar(img, "abc", purpose = "ff"), "auto")
  expect_error(img_detrend_exp(img, "abc", purpose = "ff"), "auto")
  expect_error(img_detrend_polynom(img, "abc", purpose = "ff"), "auto")
  expect_error(img_detrend_boxcar(img, FALSE, purpose = "ff"),
               "positive number or")
  expect_error(img_detrend_exp(img, FALSE, purpose = "ff"),
               "positive number or")
  expect_error(img_detrend_polynom(img, FALSE, purpose = "ff"),
               "positive number or")
  img <- img[, , 1, ]
  expect_error(img_detrend_polynom(img, degree = 1:7, purpose = "ff"),
               "length 1 or length equal to the number of channels.+7.+1")
  expect_error(img_detrend_boxcar(img, l = 1:7, purpose = "ff"),
               "length 1 or length equal to the number of channels.+7.+1")
  expect_error(img_detrend_exp(img, tau = 1:7, purpose = "ff"),
               "length 1 or length equal to the number of channels.+7.+1")
  expect_error(img_detrend_polynom(img, degree = -1, purpose = "ff"),
               "must be greater than zero")
  expect_error(img_detrend_polynom(img, degree = 1.5, purpose = "ff"),
               "must be an integer")
  expect_error(img_detrend_boxcar(img, l = -1, purpose = "ff"),
               "must be greater than zero")
  expect_error(img_detrend_boxcar(img, l = 1.5, purpose = "ff"),
               "must be an integer")
  expect_error(img_detrend_exp(img, tau = -1, purpose = "ff"),
               "must be greater than zero")
})
