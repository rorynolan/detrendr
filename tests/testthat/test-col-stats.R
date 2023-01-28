test_that("mean_cols works", {
  m <- matrix(sample.int(100), ncol = 20)
  expect_equal(mean_cols(m), apply(m, 2, mean))
})

test_that("brightness_cols edge case works", {
  expect_equal(
    brightness_cols(matrix(numeric(0), ncol = 7)),
    rep(NaN, 7)
  )
})
