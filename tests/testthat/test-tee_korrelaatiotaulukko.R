test_that("tee_korrelaatiotaulukko stops on NULL input", {
  expect_error(tee_korrelaatiotaulukko(NULL),
               "psych::corr.test")
})

test_that("tee_korrelaatiotaulukko stops on input missing r or p", {
  bad <- list(r = corr_obj$r)   # no p
  expect_error(tee_korrelaatiotaulukko(bad), "psych::corr.test")
  bad2 <- list(p = corr_obj$p)  # no r
  expect_error(tee_korrelaatiotaulukko(bad2), "psych::corr.test")
})

test_that("tee_korrelaatiotaulukko returns a tibble", {
  out <- tee_korrelaatiotaulukko(corr_obj)
  expect_s3_class(out, "tbl_df")
})

test_that("tee_korrelaatiotaulukko has Muuttuja column in Finnish output", {
  out <- tee_korrelaatiotaulukko(corr_obj, kieli = "suomi")
  expect_true("Muuttuja" %in% names(out))
})

test_that("tee_korrelaatiotaulukko translates Muuttuja to Variable in English", {
  out <- tee_korrelaatiotaulukko(corr_obj, kieli = "eng")
  expect_true("Variable" %in% names(out))
  expect_false("Muuttuja" %in% names(out))
})

test_that("tee_korrelaatiotaulukko lower triangle cells are NA", {
  out <- tee_korrelaatiotaulukko(corr_obj)
  val_mat <- as.matrix(out[, -1])
  # Strict lower triangle is NA (same trimming logic as .format_corr_upper)
  expect_true(all(is.na(val_mat[lower.tri(val_mat, diag = FALSE)])))
})

test_that("tee_korrelaatiotaulukko dimension: n_vars - 1 rows", {
  n_vars <- ncol(corr_obj$r)
  out <- tee_korrelaatiotaulukko(corr_obj)
  expect_equal(nrow(out), n_vars - 1L)
})

test_that("tee_korrelaatiotaulukko desimaalierotin = 'pilkku' uses commas", {
  out <- tee_korrelaatiotaulukko(corr_obj, desimaalierotin = "pilkku")
  char_vals <- unlist(out[, -1])
  char_vals <- char_vals[!is.na(char_vals)]
  expect_false(any(grepl("\\.", char_vals)))
  expect_true(any(grepl(",", char_vals)))
})

test_that("tee_korrelaatiotaulukko desimaalierotin = 'piste' keeps dots (default)", {
  out <- tee_korrelaatiotaulukko(corr_obj)
  char_vals <- unlist(out[, -1])
  char_vals <- char_vals[!is.na(char_vals)]
  expect_false(any(grepl(",", char_vals)))
})
