test_that("tee_jatkuvan_muuttujan_esittely returns a tibble", {
  out <- tee_jatkuvan_muuttujan_esittely(df_synth, x2, x3)
  expect_s3_class(out, "tbl_df")
})

test_that("tee_jatkuvan_muuttujan_esittely stops on non-data.frame input", {
  expect_error(tee_jatkuvan_muuttujan_esittely("not_a_df", x2),
               "data frame")
})

test_that("tee_jatkuvan_muuttujan_esittely stops on factor variable", {
  expect_error(
    tee_jatkuvan_muuttujan_esittely(df_synth, cat),
    "numeerisia"
  )
})

test_that("tee_jatkuvan_muuttujan_esittely error names the offending column", {
  expect_error(
    tee_jatkuvan_muuttujan_esittely(df_synth, cat),
    "cat"
  )
})

test_that("tee_jatkuvan_muuttujan_esittely stops when any selected variable is non-numeric", {
  expect_error(
    tee_jatkuvan_muuttujan_esittely(df_synth, x2, cat),
    "numeerisia"
  )
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

test_that("tee_jatkuvan_muuttujan_esittely vi and hu columns are character", {
  out <- tee_jatkuvan_muuttujan_esittely(df_synth, x2, vinous = TRUE)
  expect_type(out[["vi"]], "character")
  expect_type(out[["hu"]], "character")
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

test_that("tee_jatkuvan_muuttujan_esittely desimaalierotin = 'pilkku' uses commas", {
  out <- tee_jatkuvan_muuttujan_esittely(df_synth, x2, x3,
                                         vinous = TRUE, vaihteluvali = TRUE,
                                         desimaalierotin = "pilkku")
  char_cols <- out[, sapply(out, is.character)]
  char_vals <- unlist(char_cols)
  char_vals <- char_vals[!is.na(char_vals)]
  expect_false(any(grepl("\\.", char_vals)))
  # vi and hu specifically must use commas
  expect_false(any(grepl("\\.", out[["vi"]])))
  expect_false(any(grepl("\\.", out[["hu"]])))
})

test_that("tee_jatkuvan_muuttujan_esittely desimaalierotin = 'piste' keeps dots (default)", {
  out <- tee_jatkuvan_muuttujan_esittely(df_synth, x2)
  ka_vals <- out[["ka"]]
  expect_false(any(grepl(",", ka_vals)))
})
