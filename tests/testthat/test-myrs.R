context("myrs")

test_that("myrpois works", {
  expect_equal(myrpois(-5:5, 0), c(-6, -6, -5, -1, -2, 0, 0, 1, 2, 4, 2))
})
