context("med_reflect")

test_that("med_reflect_extend", {
  expect_equal(med_reflect_extend(2:8, 2,
                                  preserve_mean = TRUE, smooth = FALSE),
               0:10)
})
