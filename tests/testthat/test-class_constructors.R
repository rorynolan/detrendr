context("class_constructors")

test_that("detrended_img works", {
  library(magrittr)
  three_d <- array(runif(8), dim = rep(2, 3))
  expect_error(detrended_img(array(runif(8), dim = rep(2, 3)), "box", 5, TRUE,
                             purpose = "ff"),
               "Elements of a detrended_img must all be integers.")
  three_d[] %<>% ceiling()
  four_d <- three_d
  dim(four_d) <- c(dim(three_d)[1:2], 1, dim(three_d)[3])
  expect_equal(detrended_img(three_d, "exp", 9, FALSE, purpose = "ff"),
               detrended_img(four_d, "exp", 9, FALSE, purpose = "ff"))
})
