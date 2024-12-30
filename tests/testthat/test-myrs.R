test_that("myrpois asymptotic properties", {
  set.seed(1)
  # ----- Test positive lambda ------------------------------------------------
  n <- 10000
  lambda <- 5
  x <- myrpois(rep(lambda, n))
  expect_equal(mean(x), lambda, tolerance = 0.1)
  expect_equal(var(x), lambda, tolerance = 0.2)
  # ----- Test negative lambda -------------------------------------------------
  neg_lambda <- -3
  x_neg <- myrpois(rep(neg_lambda, n))
  expect_equal(mean(x_neg), neg_lambda, tolerance = 0.1)
  expect_equal(var(x_neg), abs(neg_lambda), tolerance = 0.2)
  # ----- Test vector input ----------------------------------------------------
  lambdas <- c(1, 3, 5, 7)
  x_vec <- myrpois(rep(lambdas, each = n))
  means <- as.vector(tapply(x_vec, rep(seq_along(lambdas), each = n), mean))
  vars <- as.vector(tapply(x_vec, rep(seq_along(lambdas), each = n), var))
  expect_equal(means, lambdas, tolerance = 0.1)
  expect_equal(vars, abs(lambdas), tolerance = 0.2)
})

test_that("myrbern asymptotic properties", {
  set.seed(1)
  # ----- Test p=0.3 -----------------------------------------------------------
  n <- 100000
  p <- 0.3
  x <- myrbern(rep(p, n))
  expect_equal(mean(x), p, tolerance = 0.01)
  expect_equal(var(x), p * (1 - p), tolerance = 0.01)
  # ----- Test vector input ----------------------------------------------------
  ps <- seq(0.1, 0.9, by = 0.2)
  x_vec <- myrbern(rep(ps, each = n))
  means <- as.vector(tapply(x_vec, rep(seq_along(ps), each = n), mean))
  vars <- as.vector(tapply(x_vec, rep(seq_along(ps), each = n), var))
  expect_equal(means, ps, tolerance = 0.01)
  expect_equal(vars, ps * (1 - ps), tolerance = 0.01)
})

test_that("rtoboxes asymptotic properties", {
  set.seed(1)
  # ----- Setup ----------------------------------------------------------------
  n <- 10000
  num_boxes <- 4
  total_balls <- 100
  # ----- Test uniform distribution --------------------------------------------
  samples <- replicate(n, rtoboxes(total_balls, num_boxes))
  means <- rowMeans(samples)
  expect_equal(means, rep(total_balls / num_boxes, num_boxes), tolerance = 0.01)
  # ----- Test with weights ----------------------------------------------------
  weights <- c(1, 2, 3, 4)
  norm_weights <- weights / sum(weights)
  samples_weighted <- replicate(n, rtoboxes(total_balls, num_boxes, weights))
  means_weighted <- rowMeans(samples_weighted)
  expect_equal(means_weighted, total_balls * norm_weights, tolerance = 0.01)
})

test_that("rfromboxes sequential sampling behavior", {
  set.seed(1)
  # ----- Test total number of balls drawn is correct --------------------------
  balls <- c(10, 20, 30, 40)
  num_draws <- 5
  result <- rfromboxes(num_draws, balls)
  expect_equal(sum(result), num_draws)
  # ----- Test that we can't draw more balls than available --------------------
  expect_error(
    rfromboxes(101, balls),
    "must be less than or equal to the total number of balls"
  )
  # ----- Test that we can't draw from empty boxes -----------------------------
  small_balls <- c(1, 2, 0, 3)
  result <- rfromboxes(3, small_balls)
  expect_equal(result[3], 0) # Can't draw from empty box
  # ----- Test weighted sampling ---------------------------------------------
  balls <- c(10, 10, 10, 10)
  weights <- c(0, 1, 1, 0)
  n_trials <- 10000
  results <- replicate(n_trials, {
    balls_copy <- balls
    weights_copy <- weights
    rfromboxes(1, balls_copy, weights_copy)
  })
  # ----- Should only draw from boxes 2 and 3 ----------------------------------
  expect_equal(sum(results[c(1, 4), ]), 0)
  # ----- Over many trials, should draw from both boxes 2 and 3 ----------------
  expect_true(sum(results[2, ]) > 0)
  expect_true(sum(results[3, ]) > 0)
  # ----- Test that weights are properly updated when boxes become empty -------
  balls <- c(1, 1, 10, 10)
  weights <- c(1, 1, 0, 0)
  result <- rfromboxes(4, balls, weights)
  # ----- 1st 2 draws empty boxes 1 and 2, forcing remaining draws to 3 and 4
  expect_equal(sum(result[1:2]), 2)
  expect_equal(sum(result[3:4]), 2)
})

test_that("rfromboxes asymptotic properties", {
  set.seed(1)
  n <- 100000
  # Test without weights - each box should be equally likely to be drawn from
  # until it runs out of balls
  balls <- c(10, 20, 30, 40)
  num_draws <- 1 # Only draw one ball at a time to avoid sequential effects
  results <- replicate(n, {
    # Need to copy balls since it's modified in place
    balls_copy <- balls
    rfromboxes(num_draws, balls_copy)
  })
  mean_draws <- rowMeans(results)
  # Each non-empty box has equal probability of being drawn
  expect_equal(mean_draws / num_draws, rep(1 / length(balls), length(balls)),
    tolerance = 0.01
  )
  # Test with weights: probability proportional to weights if box is not empty
  weights <- c(1, 2, 3, 4)
  norm_weights <- weights / sum(weights)
  results_weighted <- replicate(n, {
    # Need to copy balls and weights since they're modified in place
    balls_copy <- c(10, 10, 10, 10) # Equal number of balls in each box
    weights_copy <- weights
    rfromboxes(num_draws, balls_copy, weights_copy)
  })
  mean_draws_weighted <- rowMeans(results_weighted)
  # Distribution should match the weights
  expect_equal(mean_draws_weighted / num_draws, norm_weights, tolerance = 0.01)
  # Test that empty boxes get zero probability
  balls_with_empty <- c(0, 20, 30, 40)
  weights <- c(1, 2, 3, 4)
  norm_weights <- weights[-1] / sum(weights[-1]) # Exclude weight for empty box
  results_weighted <- replicate(n, {
    balls_copy <- balls_with_empty
    weights_copy <- weights
    rfromboxes(num_draws, balls_copy, weights_copy)
  })
  mean_draws_weighted <- rowMeans(results_weighted)
  expect_equal(mean_draws_weighted[1], 0) # Empty box should never be drawn
  expect_equal(mean_draws_weighted[-1] / num_draws, norm_weights,
    tolerance = 0.01
  )
  # Test that all balls are drawn
  balls <- c(2, 3, 4)
  result <- rfromboxes(sum(balls), balls)
  expect_equal(result, balls)
})

test_that("rfromboxes errors correctly", {
  expect_error(
    rfromboxes(10, 1:3),
    paste0(
      "`n` must be less than or equal to the total ",
      "number of balls.*",
      "You have.*n = ", 10, ".*6 balls"
    )
  )
  expect_error(
    rfromboxes(10, 1:5, 1:11),
    paste0(
      "The length of `weights` must be equal to ",
      "the length of `balls`.", ".*",
      "You have 5 elements in `balls` and ",
      "11 elements in `weights`."
    )
  )
})

test_that("rtoboxes errors correctly", {
  expect_error(
    rtoboxes(10, 4, capacities = 1:5),
    paste0(
      "The length of `capacities` must be equal to ",
      "the number of boxes.*",
      "You have 4 boxes and 5 .*capacities"
    )
  )
  expect_error(
    rtoboxes(10, 4, 1:11),
    paste0(
      "The length of `weights` must be equal to ",
      "the number of boxes.", ".*",
      "You have 4 boxes and ",
      "11 elements in `weights`."
    )
  )
  expect_error(
    rtoboxes(20, 5, capacities = 1:5),
    paste(
      "You have `n = 20` balls but your 5 boxes only have the",
      "capacity for a total of 15 balls"
    )
  )
})

test_that("rfromboxes and rtoboxes doesn't hang with a non-integer n", {
  skip_if_not_installed("R.utils")
  set.seed(1)
  boxes <- R.utils::withTimeout(
    rtoboxes(5.5, 3),
    timeout = 5
  )
  expect_equal(sum(boxes), 5)
  from <- R.utils::withTimeout(
    rfromboxes(2.5, boxes, boxes),
    timeout = 5
  )
  expect_equal(sum(from), 2)
})
