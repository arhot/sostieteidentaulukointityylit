# .format_corr_upper is not exported
test_that(".format_corr_upper returns a tibble", {
  r <- as.matrix(corr_obj$r)
  p <- as.matrix(corr_obj$p)
  out <- sostieteidentaulukointityylit:::.format_corr_upper(r, p, digits = 2)
  expect_s3_class(out, "tbl_df")
})

test_that(".format_corr_upper has Muuttuja column from rownames", {
  r <- as.matrix(corr_obj$r)
  p <- as.matrix(corr_obj$p)
  out <- sostieteidentaulukointityylit:::.format_corr_upper(r, p, digits = 2)
  expect_true("Muuttuja" %in% names(out))
})

test_that(".format_corr_upper dimensions: nrow = n-1, ncol = n (Muuttuja + (n-1) vars)", {
  r <- as.matrix(corr_obj$r)
  p <- as.matrix(corr_obj$p)
  n <- nrow(r)   # 3 variables
  out <- sostieteidentaulukointityylit:::.format_corr_upper(r, p, digits = 2)
  expect_equal(nrow(out), n - 1L)
  expect_equal(ncol(out), n)  # Muuttuja + (n-1) value columns
})

test_that(".format_corr_upper upper triangle: lower triangle + diagonal are NA", {
  r <- as.matrix(corr_obj$r)
  p <- as.matrix(corr_obj$p)
  out <- sostieteidentaulukointityylit:::.format_corr_upper(r, p, digits = 2)
  # All non-Muuttuja cells in the lower-left should be NA
  # The trimmed matrix is (n-1) rows x (n-1) cols (value cols only).
  # The lower triangle of that sub-matrix should be all NA.
  val_mat <- as.matrix(out[, -1])
  # lower.tri includes diagonal on the trimmed matrix
  expect_true(all(is.na(val_mat[lower.tri(val_mat, diag = TRUE)])))
})

test_that(".format_corr_upper adds stars for significant correlations", {
  # Build a matrix where r[1,2] is very high and p[1,2] is very small
  r2 <- matrix(c(1, 0.99, 0.99, 1), nrow = 2,
               dimnames = list(c("a", "b"), c("a", "b")))
  p2 <- matrix(c(0, 0.0001, 0.0001, 0), nrow = 2,
               dimnames = list(c("a", "b"), c("a", "b")))
  out <- sostieteidentaulukointityylit:::.format_corr_upper(r2, p2, digits = 2)
  # The single upper-triangle cell should contain "***"
  expect_true(grepl("\\*", out[[2]][[1]]))
})

test_that(".format_corr_upper stops on mismatched dimensions", {
  r <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  p <- matrix(c(0, 0.05, 0.05, 0, 0.1), nrow = 1)  # wrong dim
  expect_error(sostieteidentaulukointityylit:::.format_corr_upper(r, p))
})
