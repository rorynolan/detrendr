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

test_that("parallel column stats work", {
  skip_if_not(get_os() == "linux")
  
  # Create test matrix
  set.seed(42)
  mat <- matrix(rpois(100, lambda=10), ncol=10)
  
  # Test sum_cols_
  expected_sums <- colSums(mat)
  result_sums <- sum_cols_(mat)
  expect_equal(result_sums, expected_sums)
  
  # Test mean_cols_
  expected_means <- colMeans(mat)
  result_means <- mean_cols_(mat)
  expect_equal(result_means, expected_means)
  
  # Test var_cols_given_mean_
  expected_vars <- apply(mat, 2, var)
  means <- colMeans(mat)
  result_vars <- var_cols_given_mean_(mat, means)
  expect_equal(result_vars, expected_vars)
  
  # Test brightness_cols_
  expected_brightness <- apply(mat, 2, brightness)
  result_brightness <- brightness_cols_(mat)
  expect_equal(result_brightness, expected_brightness)
  
  # Test brightness_cols_given_mean_
  result_brightness_given_mean <- brightness_cols_given_mean_(mat, means)
  expect_equal(result_brightness_given_mean, expected_brightness)
  
  # Test edge cases
  # Empty matrix
  empty_mat <- matrix(integer(0), nrow=0, ncol=0)
  expect_equal(length(sum_cols_(empty_mat)), 0)
  expect_equal(length(mean_cols_(empty_mat)), 0)
  expect_equal(length(brightness_cols_(empty_mat)), 0)
  
  # Single column
  single_col <- matrix(1:5, ncol=1)
  expect_equal(length(sum_cols_(single_col)), 1)
  expect_equal(length(mean_cols_(single_col)), 1)
  expect_equal(length(brightness_cols_(single_col)), 1)
  expect_equal(sum_cols_(single_col), sum(1:5))
  expect_equal(mean_cols_(single_col), mean(1:5))
  
  # All constant values
  const_mat <- matrix(1, nrow=3, ncol=5)
  const_means <- rep(1, 5)
  expect_equal(var_cols_given_mean_(const_mat, const_means), rep(0, 5))
  expect_equal(mean_cols_(const_mat), rep(1, 5))
  expect_equal(sum_cols_(const_mat), rep(3, 5))
  
  # Test error cases
  # Mismatched means length
  expect_error(var_cols_given_mean_(mat, means[1:5]))
  expect_error(brightness_cols_given_mean_(mat, means[1:5]))
  
  # Test reproducibility
  mat1 <- matrix(rpois(100, lambda=10), ncol=10)
  result1 <- mean_cols_(mat1)
  result2 <- mean_cols_(mat1)
  expect_equal(result1, result2)
})
