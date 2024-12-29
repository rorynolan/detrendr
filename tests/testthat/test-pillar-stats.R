test_that("brightness_pillars works", {
  d <- 2:4
  aaa <- array(seq_len(prod(d)), dim = d)
  expect_equal(
    ijtiff::ijtiff_img(brightness_pillars(aaa)),
    ijtiff::ijtiff_img(apply_on_pillars(aaa, brightness_vec))
  )
})

test_that("pillar-stats works", {
  d <- 2:4
  aaa <- array(seq_len(prod(d)), dim = d)
  expect_equal(
    ijtiff::ijtiff_img(mean_pillars(aaa)),
    ijtiff::ijtiff_img(apply_on_pillars(aaa, mean))
  )
  expect_equal(
    ijtiff::ijtiff_img(median_pillars(aaa)),
    ijtiff::ijtiff_img(apply_on_pillars(aaa, median))
  )
  expect_equal(
    ijtiff::ijtiff_img(var_pillars(aaa)),
    ijtiff::ijtiff_img(apply_on_pillars(aaa, var))
  )
  d <- 2:5
  aaaa <- array(sample.int(prod(d)), dim = d)
  skip_if_not_installed("abind")
  ans <- purrr::map(seq_len(dim(aaaa)[3]), ~ mean_pillars(aaaa[, , ., ])) %>%
    purrr::reduce(~ abind::abind(.x, .y, along = 3)) %>%
    structure(dimnames = NULL)
  expect_equal(
    ijtiff::ijtiff_img(mean_pillars(aaaa)),
    ijtiff::ijtiff_img(ans)
  )
  ans <- purrr::map(seq_len(dim(aaaa)[3]), ~ median_pillars(aaaa[, , ., ])) %>%
    purrr::reduce(~ abind::abind(.x, .y, along = 3)) %>%
    structure(dimnames = NULL)
  expect_equal(
    ijtiff::ijtiff_img(median_pillars(aaaa)),
    ijtiff::ijtiff_img(ans)
  )
  ans <- purrr::map(seq_len(dim(aaaa)[3]), ~ var_pillars(aaaa[, , ., ])) %>%
    purrr::reduce(~ abind::abind(.x, .y, along = 3)) %>%
    structure(dimnames = NULL)
  expect_equal(
    ijtiff::ijtiff_img(var_pillars(aaaa)),
    ijtiff::ijtiff_img(ans)
  )
  ans <- purrr::map(
    seq_len(dim(aaaa)[3]),
    ~ brightness_pillars(aaaa[, , ., ])
  ) %>%
    purrr::reduce(~ abind::abind(.x, .y, along = 3)) %>%
    structure(dimnames = NULL)
  expect_equal(
    ijtiff::ijtiff_img(brightness_pillars(aaaa)),
    ijtiff::ijtiff_img(ans)
  )
})

test_that("parallel pillar stats work", {
  skip_if_not(get_os() == "linux")
  
  # Create test array
  arr <- array(runif(2*3*4), dim=c(2,3,4))
  
  # Test sum_pillars_
  expected_sums <- apply(arr, c(1,2), sum)
  result_sums <- sum_pillars_(arr)
  expect_equal(result_sums, expected_sums)
  
  # Test mean_pillars_
  expected_means <- apply(arr, c(1,2), mean)
  result_means <- mean_pillars_(arr)
  expect_equal(result_means, expected_means)
  
  # Test var_pillars_
  expected_vars <- apply(arr, c(1,2), var)
  result_vars <- var_pillars_(arr)
  expect_equal(result_vars, expected_vars)
  
  # Test median_pillars_
  expected_medians <- apply(arr, c(1,2), median)
  result_medians <- median_pillars_(arr)
  expect_equal(result_medians, expected_medians)
  
  # Test brightness_pillars_
  expected_brightness <- apply(arr, c(1,2), brightness)
  result_brightness <- brightness_pillars_(arr)
  expect_equal(result_brightness, expected_brightness)
  
  # Test edge cases
  # Empty array
  empty_arr <- array(numeric(0), dim=c(0,0,0))
  expect_error(sum_pillars_(empty_arr))
  expect_error(mean_pillars_(empty_arr))
  expect_error(var_pillars_(empty_arr))
  expect_error(median_pillars_(empty_arr))
  expect_error(brightness_pillars_(empty_arr))
  
  # Single pillar
  single_arr <- array(1:3, dim=c(1,1,3))
  expect_equal(dim(sum_pillars_(single_arr)), c(1,1))
  expect_equal(dim(mean_pillars_(single_arr)), c(1,1))
  expect_equal(dim(var_pillars_(single_arr)), c(1,1))
  expect_equal(dim(median_pillars_(single_arr)), c(1,1))
  expect_equal(dim(brightness_pillars_(single_arr)), c(1,1))
  
  # All constant values
  const_arr <- array(rep(1, 24), dim=c(2,3,4))
  expect_equal(var_pillars_(const_arr), matrix(0, nrow=2, ncol=3))
  expect_equal(median_pillars_(const_arr), matrix(1, nrow=2, ncol=3))
  expect_equal(mean_pillars_(const_arr), matrix(1, nrow=2, ncol=3))
  
  # Test reproducibility
  arr1 <- array(rnorm(24), dim=c(2,3,4))
  result1 <- mean_pillars_(arr1)
  result2 <- mean_pillars_(arr1)
  expect_equal(result1, result2)
})
