test_that("NA handling in frame stats works", {
  # Create test array with all NAs
  all_na_arr <- array(NA_real_, dim = c(2, 2, 2))
  
  # Test sum_frames_na_omit
  expect_true(all(is.na(dbl_sum_frames_na_omit(all_na_arr))))
  expect_true(all(is.na(int_sum_frames_na_omit(array(NA_integer_, dim = c(2, 2, 2))))))
  
  # Test mean_frames_na_omit  
  expect_true(all(is.na(dbl_mean_frames_na_omit(all_na_arr))))
  expect_true(all(is.na(int_mean_frames_na_omit(array(NA_integer_, dim = c(2, 2, 2))))))
  
  # Create test array with mix of values and NAs
  mixed_arr <- array(c(1, NA, 3, NA, 5, NA, 7, NA), dim = c(2, 2, 2))
  
  # Test sum_frames_na_omit with mixed data
  sums <- dbl_sum_frames_na_omit(mixed_arr)
  expect_equal(sums[1], 4)  # 1 + 3
  expect_equal(sums[2], 12) # 5 + 7
  
  # Test mean_frames_na_omit with mixed data
  means <- dbl_mean_frames_na_omit(mixed_arr)
  expect_equal(means[1], 2)  # (1 + 3) / 2
  expect_equal(means[2], 6)  # (5 + 7) / 2
})
