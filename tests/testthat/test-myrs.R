context("myrs")

test_that("myrpois works", {
  expect_equal(myrpois(-5:5, 10), c(-6, -2, -3, -1, 0, 0, 1, 3, 3, 9, 6))
})

test_that("myrbern works", {
  expect_equal(myrbern(seq(0.01, 0.9, length.out = 7), 12),
               c(0, 0, 1, 1, 0, 1, 1))
})
