poly_fit_pillars <- function(arr3d, degree) {
  d <- dim(arr3d)
  if (degree >= d[3]) {
    stop(
      "Your pillars are of length ", d[3], ". This is too short. ",
      "To fit a polynomial of degree ", degree, ", your pillars must be ",
      "of length at least ", d[3] + 1, "."
    )
  }
  pillars_to_cols(arr3d) %>%
    poly_fit_cols(degree) %>%
    cols_to_pillars(dim(arr3d))
}

poly_fit_cols <- function(mat, degree) {
  degree <- floor(degree)
  nr <- nrow(mat)
  if (degree >= nr) {
    custom_stop(
      "Your columns are of length {nr}. This is too short. ",
      "
      To fit a polynomial of degree {degree}, your pillars must be
      of length at least {degree + 1}.
      "
    )
  }
  x1 <- seq_len(nr)
  x <- stats::poly(x1, degree, raw = TRUE)
  na_cols <- apply(mat, 2, anyNA)
  any_na_cols <- any(na_cols)
  out <- mat %T>% {
    .[] <- NA
  }
  if (any_na_cols) {
    non_na_cols <- !na_cols
    mat <- mat[, non_na_cols, drop = FALSE]
  }
  fits <- stats::fitted(stats::lm(mat ~ x))
  if (any_na_cols) {
    out[, non_na_cols] <- fits
    out
  } else {
    fits
  }
}
