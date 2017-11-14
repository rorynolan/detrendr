context("Utilities")

test_that("translate_parallel works", {
  expect_equal(detrendr:::translate_parallel(FALSE), 1)
  expect_equal(detrendr:::translate_parallel(2), 2)
  expect_true(detrendr:::translate_parallel(TRUE) >= 2)
})

test_that("apply_on_pillars works", {
  a <- array(runif(3^3), dim = rep(3, 3))
  expect_equal(apply_on_pillars(a, identity), a)
})
