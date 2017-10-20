context("myrs")

test_that("myrpois works", {
  skip_on_appveyor()
  skip_on_cran()
  skip_on_travis()
  expect_equal(myrpois(-5:5, 0), c(-6, -6, -5, -1, -2, 0, 0, 1, 2, 4, 2))
})
