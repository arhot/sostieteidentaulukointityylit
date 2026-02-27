test_that("tee_faktorianalyysitaulukko stops on non-fa/principal input", {
  expect_error(tee_faktorianalyysitaulukko(lm_simple),
               "faktorianalyysin tulosobjekti")
  expect_error(tee_faktorianalyysitaulukko("teksti"),
               "faktorianalyysin tulosobjekti")
})

test_that("tee_faktorianalyysitaulukko returns a tibble", {
  out <- tee_faktorianalyysitaulukko(fa_model)
  expect_s3_class(out, "tbl_df")
})

test_that("tee_faktorianalyysitaulukko correct row count: n_vars + 2 summary rows", {
  n_vars <- nrow(fa_model$loadings)
  out <- tee_faktorianalyysitaulukko(fa_model)
  expect_equal(nrow(out), n_vars + 2L)
})

test_that("tee_faktorianalyysitaulukko summary rows contain Finnish labels by default", {
  out <- tee_faktorianalyysitaulukko(fa_model, kieli = "suomi")
  muuttuja_col <- out[[1]]
  expect_true("Ominaisarvo" %in% muuttuja_col)
  expect_true("Selitysosuus" %in% muuttuja_col)
})

test_that("tee_faktorianalyysitaulukko summary rows use English labels with kieli = 'eng'", {
  out <- tee_faktorianalyysitaulukko(fa_model, kieli = "eng")
  muuttuja_col <- out[[1]]
  expect_true("SS loadings" %in% muuttuja_col)
  expect_true("Proportion Var" %in% muuttuja_col)
})

test_that("tee_faktorianalyysitaulukko piilota_lataukset_alle hides small loadings as ''", {
  out <- tee_faktorianalyysitaulukko(fa_model, piilota_lataukset_alle = 0.99)
  # With threshold 0.99 almost all loadings should be hidden
  factor_cols <- out[seq_len(nrow(fa_model$loadings)), -1]
  factor_cols <- factor_cols[, !names(factor_cols) %in%
                               c("Kommunaliteetti", "Communality")]
  hidden <- unlist(factor_cols)
  hidden <- hidden[!is.na(hidden)]
  expect_true(all(hidden == ""))
})

test_that("tee_faktorianalyysitaulukko desimaalierotin = 'pilkku' uses commas", {
  out <- tee_faktorianalyysitaulukko(fa_model, desimaalierotin = "pilkku")
  # At least some cells should contain a comma
  has_comma <- any(grepl(",", unlist(out)), na.rm = TRUE)
  expect_true(has_comma)
  # No periods should remain in numeric cells
  has_period <- any(grepl("\\.", unlist(out)), na.rm = TRUE)
  expect_false(has_period)
})

test_that("tee_faktorianalyysitaulukko 'Variable' column exists in English output", {
  out <- tee_faktorianalyysitaulukko(fa_model, kieli = "eng")
  expect_true("Variable" %in% names(out))
})
