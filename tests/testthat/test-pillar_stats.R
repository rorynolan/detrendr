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
  d <- 2:5
  aaaa <- array(sample.int(prod(d)), dim = d)
  skip_if_not_installed("abind")
  ans <- purrr::map(seq_len(dim(aaaa)[3]), ~ mean_pillars(aaaa[, , ., ])) %>%
    purrr::reduce(~ abind::abind(.x, .y, along = 3)) %T>% {
      dim(.) <- c(dim(.), 1)
    }
  expect_equal(mean_pillars(aaaa), ans)
  ans <- purrr::map(seq_len(dim(aaaa)[3]), ~ median_pillars(aaaa[, , ., ])) %>%
    purrr::reduce(~ abind::abind(.x, .y, along = 3)) %T>% {
      dim(.) <- c(dim(.), 1)
    }
  expect_equal(median_pillars(aaaa), ans)
  ans <- purrr::map(seq_len(dim(aaaa)[3]), ~ var_pillars(aaaa[, , ., ])) %>%
    purrr::reduce(~ abind::abind(.x, .y, along = 3)) %T>% {
      dim(.) <- c(dim(.), 1)
    }
  expect_equal(var_pillars(aaaa), ans)
  ans <- purrr::map(seq_len(dim(aaaa)[3]),
                    ~ brightness_pillars(aaaa[, , ., ])) %>%
    purrr::reduce(~ abind::abind(.x, .y, along = 3)) %T>% {
      dim(.) <- c(dim(.), 1)
    }
  expect_equal(brightness_pillars(aaaa), ans)
})
