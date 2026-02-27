test_that("tee_regressiotaulukko stops on non-lm input", {
  expect_error(tee_regressiotaulukko(glm_logit),
               "regressioanalyysin tulosobjekti")
  expect_error(tee_regressiotaulukko("teksti"),
               "regressioanalyysin tulosobjekti")
})

test_that("tee_regressiotaulukko returns a tibble", {
  out <- tee_regressiotaulukko(lm_simple)
  expect_s3_class(out, "tbl_df")
})

test_that("tee_regressiotaulukko excludes intercept row", {
  out <- tee_regressiotaulukko(lm_simple)
  muuttuja <- out[[1]]
  expect_false("(Intercept)" %in% muuttuja)
})

test_that("tee_regressiotaulukko last row is R2 (Finnish)", {
  out <- tee_regressiotaulukko(lm_simple, kieli = "suomi")
  last_row_label <- out[[1]][nrow(out)]
  expect_equal(last_row_label, "R2, korjattu")
})

test_that("tee_regressiotaulukko last row is R2 (English)", {
  out <- tee_regressiotaulukko(lm_simple, kieli = "eng")
  last_row_label <- out[[1]][nrow(out)]
  expect_equal(last_row_label, "R2, adjusted")
})

test_that("tee_regressiotaulukko expected columns exist (Finnish)", {
  out <- tee_regressiotaulukko(lm_simple, kieli = "suomi")
  expect_true(all(c("Muuttuja", "B", "beta", "Luottamusvali", "p") %in% names(out)))
})

test_that("tee_regressiotaulukko row count: n_predictors + 1 R2 row", {
  # lm_multi has 2 predictors; intercept filtered; + 1 R2 row
  n_preds <- length(coef(lm_multi)) - 1L  # minus intercept
  out <- tee_regressiotaulukko(lm_multi)
  expect_equal(nrow(out), n_preds + 1L)
})

test_that("tee_regressiotaulukko Luottamusvali column has bracket format", {
  out <- tee_regressiotaulukko(lm_simple)
  lv_vals <- out[["Luottamusvali"]]
  lv_vals <- lv_vals[!is.na(lv_vals)]
  expect_true(all(grepl("^\\[", lv_vals)))
})
