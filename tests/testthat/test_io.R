img_path <- system.file("extdata", "bleached.tif", package = "detrendr")
img <- read_tif(img_path, n_ch = 1)

test_that("display works", {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  expect_null(display(img[, , 1]))
})

test_that("write_tif works", {
  skip_on_appveyor()
  skip_on_cran()
  skip_on_travis()
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  write_tif(img, "bleached.tif")
  expect_equal(read_tif("bleached.tif"), img)
  suppressWarnings(file.remove(list.files()))
  expect_error(write_tif(img %T>% {.[1] <- 0.5}, "abc.tif"), "only integers")
  expect_error(write_tif(img %T>% {.[1] <- -1L}, "abc.tif"), "negative")
  expect_error(write_tif(img %T>% {.[1] <- 0.5}, "abc.tif", na = -1))
  expect_equal(write_tif(img, "abc.tif", na = 0), img)
  expect_error(write_tif(img %T>% {.[1] <- 2^33}, "abc.tif"), "2\\^32")
  img0 <- img %T>% {.[] <- 2 ^ 31}
  expect_equal(write_tif(img0, "abc.tif"), img0)
  img0[] <- 2 ^ 15
  expect_equal(write_tif(img0, "abc.tif"), img0)
  img0[] <- 2 ^ 7
  expect_equal(write_tif(img0, "abc.tif"), img0)
  expect_error(write_tif(array(1, dim = rep(2, 5)), "abc.tif"), "at most 4 dim")
  img0 <- array(1, dim = rep(4, 4))
  expect_equal(write_tif(img0, "abc"), img0)
})

test_that("read_tif works", {
  expect_equal(read_tif(img_path, n_ch = 2)[, , 1, ],
               img[, , seq(1, dim(img)[3], by = 2)])
  expect_error(read_tif(img_path, n_ch = 3), "Could not recognise")
})
