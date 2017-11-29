context("myrs")

test_that("myrpois works", {
  expect_true(list(myrpois(-5:5, 10)) %in%
                list(c(-2, -5, -6, -2, 0, 0, 1, 0, 4, 6, 5),
                     c(-6, -3, -6, -2, -1, 0, 0, 1, 0, 5, 6),  # travis
                     c(-5, -2, -1, -6, -2, 0, 0, 0, 3, 4, 5)))  # appveyor
})

test_that("myrbern works", {
  expect_true(list(myrbern(seq(0.01, 0.9, length.out = 7), 12)) %in%
              list(c(0, 0, 1, 0, 0, 1, 1),
                   c(0, 0, 0, 1, 1, 1, 1),  # travis
                   c(0, 0, 0, 1, 1, 1, 1)))
})
