test_that("parallel row stats work", {
  skip_if_not(get_os() == "linux")
  
  # Create test matrix
  set.seed(42)
  mat <- matrix(rpois(100, lambda=10), nrow=10)
  
  # Test sum_rows_
  expected_sums <- rowSums(mat)
  result_sums <- sum_rows_(mat)
  expect_equal(result_sums, expected_sums)
  
  # Test mean_rows_
  expected_means <- rowMeans(mat)
  result_means <- mean_rows_(mat)
  expect_equal(result_means, expected_means)
  
  # Test var_rows_given_mean_
  expected_vars <- apply(mat, 1, var)
  means <- rowMeans(mat)
  result_vars <- var_rows_given_mean_(mat, means)
  expect_equal(result_vars, expected_vars)
  
  # Test brightness_rows_
  expected_brightness <- apply(mat, 1, brightness)
  result_brightness <- brightness_rows_(mat)
  expect_equal(result_brightness, expected_brightness)
  
  # Test brightness_rows_given_mean_
  result_brightness_given_mean <- brightness_rows_given_mean_(mat, means)
  expect_equal(result_brightness_given_mean, expected_brightness)
  
  # Test edge cases
  # Empty matrix
  empty_mat <- matrix(integer(0), nrow=0, ncol=0)
  expect_equal(length(sum_rows_(empty_mat)), 0)
  expect_equal(length(mean_rows_(empty_mat)), 0)
  expect_equal(length(brightness_rows_(empty_mat)), 0)
  
  # Single row
  single_row <- matrix(1:5, nrow=1)
  expect_equal(length(sum_rows_(single_row)), 1)
  expect_equal(length(mean_rows_(single_row)), 1)
  expect_equal(length(brightness_rows_(single_row)), 1)
  expect_equal(sum_rows_(single_row), sum(1:5))
  expect_equal(mean_rows_(single_row), mean(1:5))
  
  # All constant values
  const_mat <- matrix(1, nrow=5, ncol=3)
  const_means <- rep(1, 5)
  expect_equal(var_rows_given_mean_(const_mat, const_means), rep(0, 5))
  expect_equal(mean_rows_(const_mat), rep(1, 5))
  expect_equal(sum_rows_(const_mat), rep(3, 5))
  
  # Test error cases
  # Mismatched means length
  expect_error(var_rows_given_mean_(mat, means[1:5]))
  expect_error(brightness_rows_given_mean_(mat, means[1:5]))
  
  # Test reproducibility
  mat1 <- matrix(rpois(100, lambda=10), nrow=10)
  result1 <- mean_rows_(mat1)
  result2 <- mean_rows_(mat1)
  expect_equal(result1, result2)
})
