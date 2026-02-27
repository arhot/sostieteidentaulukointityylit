test_that("tee_yhdistelmataulukko stops on empty input", {
  expect_error(tee_yhdistelmataulukko(), "vähintään yksi")
})

test_that("tee_yhdistelmataulukko returns a tibble or data.frame", {
  t1 <- tee_regressiotaulukko(lm_simple)
  out <- tee_yhdistelmataulukko(t1)
  expect_true(is.data.frame(out))
})

test_that("tee_yhdistelmataulukko single table: row count = original rows + 1 header", {
  t1 <- tee_regressiotaulukko(lm_simple)
  out <- tee_yhdistelmataulukko(t1)
  expect_equal(nrow(out), nrow(t1) + 1L)
})

test_that("tee_yhdistelmataulukko two tables: correct total rows", {
  t1 <- tee_regressiotaulukko(lm_simple)
  t2 <- tee_regressiotaulukko(lm_multi)
  out <- tee_yhdistelmataulukko(t1, t2)
  expect_equal(nrow(out), nrow(t1) + nrow(t2) + 2L)  # 2 header rows
})

test_that("tee_yhdistelmataulukko header rows contain 'Malli 1', 'Malli 2' (Finnish)", {
  t1 <- tee_regressiotaulukko(lm_simple)
  t2 <- tee_regressiotaulukko(lm_multi)
  out <- tee_yhdistelmataulukko(t1, t2, kieli = "suomi")
  muuttuja_col <- out[[1]]
  expect_true("Malli 1" %in% muuttuja_col)
  expect_true("Malli 2" %in% muuttuja_col)
})

test_that("tee_yhdistelmataulukko English uses 'Model' instead of 'Malli'", {
  t1 <- tee_regressiotaulukko(lm_simple, kieli = "eng")
  t2 <- tee_regressiotaulukko(lm_multi,  kieli = "eng")
  out <- tee_yhdistelmataulukko(t1, t2, kieli = "eng")
  muuttuja_col <- out[[1]]
  expect_true("Model 1" %in% muuttuja_col)
  expect_false("Malli 1" %in% muuttuja_col)
})

test_that("tee_yhdistelmataulukko header rows have NA in all non-Muuttuja columns", {
  t1 <- tee_regressiotaulukko(lm_simple)
  out <- tee_yhdistelmataulukko(t1)
  header_row <- out[out[[1]] == "Malli 1", ]
  other_cols <- header_row[, -1]
  expect_true(all(is.na(other_cols)))
})
