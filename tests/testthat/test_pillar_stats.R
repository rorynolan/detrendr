context("Pillar calculations")

test_that("brightness_pillars works", {
  d <- 2:4
  aaa <- array(seq_len(prod(d)), dim = d)
  expect_equal(brightness_pillars(aaa), apply_on_pillars(aaa, brightness_vec))
})

test_that("pillar-stats works", {
  d <- 2:4
  aaa <- array(seq_len(prod(d)), dim = d)
  expect_equal(mean_pillars(aaa), apply_on_pillars(aaa, mean))
  expect_equal(median_pillars(aaa), apply_on_pillars(aaa, median))
  expect_equal(var_pillars(aaa), apply_on_pillars(aaa, var))
})
