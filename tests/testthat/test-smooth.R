test_that("cpp smoothing functions work", {
  v <- runif(10)
  expect_equal(boxcar_smooth(v, 1), weighted_smooth(v, rep(1, 3)))
  expect_equal(boxcar_smooth(v, 3), weighted_smooth(v, rep(1, 7)))
  exp5weights <- exp(-abs(-1:1) / 5)
  expect_equal(
    exp_smooth(v, 5, 1),
    weighted_smooth(v, exp5weights)
  )
  v <- seq_len(3)
  expect_equal(weighted_smooth(v, rep(1, 21)), rep(2, 3))
  if (get_os() == "mac") {
    skip_on_cran()
  } else {
    expect_error(weighted_smooth(v, rep(1, 20)), "must be odd",
      class = "C++Error"
    )
  }
})

test_that("parallel smoothing functions work", {
  # Test boxcar_smooth_rows
  mat <- matrix(runif(100), nrow = 10)
  expected_rows <- t(apply(mat, 1, boxcar_smooth, l = 2))
  result_rows <- boxcar_smooth_rows(mat, 2)
  expect_equal(result_rows, expected_rows)

  # Test boxcar_smooth_pillars
  arr <- array(runif(60), dim = c(3, 4, 5))
  expected_pillars <- arr
  for (i in 1:3) {
    for (j in 1:4) {
      pillar <- arr[i, j, ]
      expected_pillars[i, j, ] <- boxcar_smooth(pillar, l = 1)
    }
  }
  result_pillars <- boxcar_smooth_pillars(arr, 1)
  expect_equal(result_pillars, expected_pillars)

  # Test exp_smooth_rows
  tau <- 2
  l <- 2
  expected_rows <- t(apply(mat, 1, exp_smooth, tau = tau, l = l))
  result_rows <- exp_smooth_rows(mat, tau, l)
  expect_equal(result_rows, expected_rows)

  # Test exp_smooth_pillars
  expected_pillars <- arr
  for (i in 1:3) {
    for (j in 1:4) {
      pillar <- arr[i, j, ]
      expected_pillars[i, j, ] <- exp_smooth(pillar, tau = tau, l = l)
    }
  }
  result_pillars <- exp_smooth_pillars(arr, tau, l)
  expect_equal(result_pillars, expected_pillars)

  # Test edge cases
  # Empty matrix
  empty_mat <- matrix(numeric(0), nrow = 0, ncol = 0)
  expect_equal(dim(boxcar_smooth_rows(empty_mat, 1)), c(0, 0))
  expect_equal(dim(exp_smooth_rows(empty_mat, 1, 1)), c(0, 0))

  # Single row/column
  single_row <- matrix(1:5, nrow = 1)
  expect_equal(dim(boxcar_smooth_rows(single_row, 1)), c(1, 5))
  expect_equal(dim(exp_smooth_rows(single_row, 1, 1)), c(1, 5))

  # Single pillar
  single_pillar <- array(1:5, dim = c(1, 1, 5))
  expect_equal(dim(boxcar_smooth_pillars(single_pillar, 1)), c(1, 1, 5))
  expect_equal(dim(exp_smooth_pillars(single_pillar, 1, 1)), c(1, 1, 5))
})
