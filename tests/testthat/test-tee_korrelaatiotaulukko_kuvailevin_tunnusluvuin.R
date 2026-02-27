test_that("tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin stops on bad korrelaatiomatriisi", {
  expect_error(
    tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin(NULL, corr_vars),
    "psych::corr.test"
  )
})

test_that("tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin stops on non-data.frame aineisto", {
  expect_error(
    tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin(corr_obj, "not_a_df"),
    "data.frame"
  )
})

test_that("tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin returns a tibble", {
  out <- tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin(corr_obj, corr_vars)
  expect_s3_class(out, "tbl_df")
})

test_that("tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin has n, ka, kh columns (Finnish)", {
  out <- tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin(corr_obj, corr_vars, kieli = "suomi")
  expect_true(all(c("n", "ka", "kh") %in% names(out)))
})

test_that("tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin translates descriptive columns (English)", {
  out <- tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin(corr_obj, corr_vars, kieli = "eng")
  expect_true(all(c("n", "mean", "sd") %in% names(out)))
  expect_false("ka" %in% names(out))
})

test_that("tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin row count matches n variables", {
  n_vars <- ncol(corr_obj$r)
  out <- tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin(corr_obj, corr_vars)
  # left_join preserves all rows from stats_df (one per variable),
  # so the output has n_vars rows (not trimmed like the plain correlation table)
  expect_equal(nrow(out), n_vars)
})

test_that("tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin n column excludes NAs", {
  # x2 has no NAs, full n = 100
  out <- tee_korrelaatiotaulukko_kuvailevin_tunnusluvuin(corr_obj, corr_vars)
  n_col <- out[["n"]]
  expect_true(all(n_col == nrow(corr_vars)))
})
