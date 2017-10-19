context("smooth")

test_that("cpp smoothing functions work", {
  v <- runif(10)
  ev <- med_reflect_extend(v, 5)
  expect_equal(boxcar_smooth(ev, 5, 1), weighted_smooth(ev, 5, rep(1, 3), 3))
  expect_equal(boxcar_smooth(ev, 5, 3), weighted_smooth(ev, 5, rep(1, 7), 7))
  exp5weights <- exp(- abs(-1:1) / 5)
  expect_equal(exp_smooth(ev, 5, 5, 1),
               weighted_smooth(ev, 5, exp5weights, sum(exp5weights)))
  v <- seq_len(3)
  ev <- med_reflect_extend(v, 1)
  expect_equal(weighted_smooth(ev, 1, rep(1, 21), 21), rep(2, 3))
})
