test_that("brightness_pillars works", {
  aaa <- array(1:16, dim = c(2, 2, 4))
  expect_equal(brightness_pillars(aaa), apply_on_pillars(aaa, brightness_vec))
})

test_that("pillar-stats works", {
  aaa <- array(1:16, dim = c(2, 2, 4))
  expect_equal(mean_pillars(aaa), apply_on_pillars(aaa, mean))
  expect_equal(median_pillars(aaa), apply_on_pillars(aaa, median))
  expect_equal(var_pillars(aaa), apply_on_pillars(aaa, var))
})
