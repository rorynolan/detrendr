context("Utilities")

test_that("translate_parallel works", {
  expect_equal(translate_parallel(FALSE), 1)
  max_cores <- parallel::detectCores()
  expect_equal(translate_parallel(2), min(2, max_cores))
  expect_equal(translate_parallel(TRUE), max_cores)
})

test_that("apply_on_pillars works", {
  a <- array(runif(3^3), dim = rep(3, 3))
  expect_equal(apply_on_pillars(a, identity), a)
})
