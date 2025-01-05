test_that("parallel bernoulli sampling works", {
  # Test basic functionality with fixed seed
  n <- 1000
  p <- runif(n)
  set.seed(42)
  result <- myrbernoulli_(p, 42)

  # Check output type and length
  expect_type(result, "integer")
  expect_length(result, n)

  # Check values are binary
  expect_true(all(result %in% c(0, 1)))

  # Check mean is approximately p
  expect_equal(mean(result), mean(p), tolerance = 0.1)

  # Test reproducibility
  set.seed(42)
  result2 <- myrbernoulli_(p, 42)
  expect_equal(result, result2)

  # Test different seeds give different results
  set.seed(43)
  result3 <- myrbernoulli_(p, 43)
  expect_false(identical(result, result3))

  # Test edge cases
  # Empty vector
  expect_equal(length(myrbernoulli_(numeric(0), 1)), 0)

  # All zeros
  zeros <- rep(0, 100)
  expect_equal(sum(myrbernoulli_(zeros, 1)), 0)

  # All ones
  ones <- rep(1, 100)
  expect_equal(sum(myrbernoulli_(ones, 1)), 100)

  # Test error cases
  expect_error(myrbernoulli_(c(-1, 0.5), 1), "probability")
  expect_error(myrbernoulli_(c(2, 0.5), 1), "probability")
})
