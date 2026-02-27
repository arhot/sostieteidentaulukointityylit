test_that("tee_jatkuvan_muuttujan_esittely returns a tibble", {
  out <- tee_jatkuvan_muuttujan_esittely(df_synth, x2, x3)
  expect_s3_class(out, "tbl_df")
})

test_that("tee_jatkuvan_muuttujan_esittely base columns exist (Finnish)", {
  out <- tee_jatkuvan_muuttujan_esittely(df_synth, x2, x3, kieli = "suomi")
  expect_true(all(c("Muuttuja", "n", "ka", "kh") %in% names(out)))
})

test_that("tee_jatkuvan_muuttujan_esittely row count equals number of variables", {
  out <- tee_jatkuvan_muuttujan_esittely(df_synth, x2, x3, x4)
  expect_equal(nrow(out), 3L)
})

test_that("tee_jatkuvan_muuttujan_esittely n reflects non-NA observations", {
  # x1 has 3 NAs
  out <- tee_jatkuvan_muuttujan_esittely(df_synth, x1)
  expect_equal(out[["n"]], 97L)
})

test_that("tee_jatkuvan_muuttujan_esittely lv = TRUE adds CI columns", {
  out <- tee_jatkuvan_muuttujan_esittely(df_synth, x2, lv = TRUE)
  expect_true(all(c("lv_alaraja", "lv_ylaraja") %in% names(out)))
})

test_that("tee_jatkuvan_muuttujan_esittely lv = FALSE has no CI columns", {
  out <- tee_jatkuvan_muuttujan_esittely(df_synth, x2, lv = FALSE)
  expect_false("lv_alaraja" %in% names(out))
})

test_that("tee_jatkuvan_muuttujan_esittely vinous = TRUE adds vi and hu columns", {
  out <- tee_jatkuvan_muuttujan_esittely(df_synth, x2, vinous = TRUE)
  expect_true(all(c("vi", "hu") %in% names(out)))
})

test_that("tee_jatkuvan_muuttujan_esittely vaihteluvali = TRUE adds minimi and maksimi", {
  out <- tee_jatkuvan_muuttujan_esittely(df_synth, x2, vaihteluvali = TRUE)
  expect_true(all(c("minimi", "maksimi") %in% names(out)))
})

test_that("tee_jatkuvan_muuttujan_esittely English translation works", {
  out <- tee_jatkuvan_muuttujan_esittely(df_synth, x2, x3, kieli = "eng")
  expect_true(all(c("Variable", "mean", "sd") %in% names(out)))
  expect_false("ka" %in% names(out))
})

test_that("tee_jatkuvan_muuttujan_esittely CI columns translated in English", {
  out <- tee_jatkuvan_muuttujan_esittely(df_synth, x2, lv = TRUE, kieli = "eng")
  expect_true(all(c("ci_lo", "ci_hi") %in% names(out)))
})
