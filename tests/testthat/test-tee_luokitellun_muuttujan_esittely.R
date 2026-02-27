test_that("tee_luokitellun_muuttujan_esittely returns a tibble", {
  out <- tee_luokitellun_muuttujan_esittely(df_synth, cat)
  expect_s3_class(out, "tbl_df")
})

test_that("tee_luokitellun_muuttujan_esittely has n and % columns (Finnish)", {
  out <- tee_luokitellun_muuttujan_esittely(df_synth, cat, kieli = "suomi")
  expect_true("n" %in% names(out))
  expect_true("%" %in% names(out))
})

test_that("tee_luokitellun_muuttujan_esittely row count equals number of factor levels", {
  n_levels <- nlevels(df_synth$cat)
  out <- tee_luokitellun_muuttujan_esittely(df_synth, cat)
  expect_equal(nrow(out), n_levels)
})

test_that("tee_luokitellun_muuttujan_esittely n values sum to nrow(aineisto)", {
  out <- tee_luokitellun_muuttujan_esittely(df_synth, cat)
  expect_equal(sum(out[["n"]]), nrow(df_synth))
})

test_that("tee_luokitellun_muuttujan_esittely includes NA row when data has NAs", {
  df_na <- df_synth
  df_na$cat[c(1, 2)] <- NA
  out <- tee_luokitellun_muuttujan_esittely(df_na, cat)
  # janitor::tabyl includes NA as a separate row + valid_percent column
  expect_true(nrow(out) > nlevels(df_na$cat))
})

test_that("tee_luokitellun_muuttujan_esittely desimaalipaikkoja controls decimal places", {
  out1 <- tee_luokitellun_muuttujan_esittely(df_synth, cat, desimaalipaikkoja = 0)
  out2 <- tee_luokitellun_muuttujan_esittely(df_synth, cat, desimaalipaikkoja = 2)
  pct1 <- out1[["%"]][1]
  pct2 <- out2[["%"]][1]
  # 0 decimals: no dot in the numeric part; 2 decimals: has dot
  expect_false(grepl("\\.", gsub("%", "", pct1)))
  expect_true(grepl("\\.", gsub("%", "", pct2)))
})

test_that("tee_luokitellun_muuttujan_esittely English translation works", {
  out <- tee_luokitellun_muuttujan_esittely(df_synth, cat, kieli = "eng")
  expect_true("%" %in% names(out))   # % is same in both languages
  expect_false("Muuttuja" %in% names(out))  # first col is variable name, not "Muuttuja"
})
