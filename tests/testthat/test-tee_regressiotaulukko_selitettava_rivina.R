test_that("tee_regressiotaulukko_selitettava_rivina stops on non-lm input", {
  # glm inherits 'lm' in R, so test with a truly non-lm object
  expect_error(tee_regressiotaulukko_selitettava_rivina(fa_model),
               "regressioanalyysin tulosobjekti")
  expect_error(tee_regressiotaulukko_selitettava_rivina("teksti"),
               "regressioanalyysin tulosobjekti")
})

test_that("tee_regressiotaulukko_selitettava_rivina returns a tibble", {
  out <- tee_regressiotaulukko_selitettava_rivina(lm_simple)
  expect_s3_class(out, "tbl_df")
})

test_that("tee_regressiotaulukko_selitettava_rivina has Selitettava column (Finnish)", {
  out <- tee_regressiotaulukko_selitettava_rivina(lm_simple, kieli = "suomi")
  expect_true("Selitettava" %in% names(out))
})

test_that("tee_regressiotaulukko_selitettava_rivina Selitettava contains outcome name", {
  out <- tee_regressiotaulukko_selitettava_rivina(lm_simple)
  selitettava_col <- out[["Selitettava"]]
  expect_true(all(selitettava_col == "y"))
})

test_that("tee_regressiotaulukko_selitettava_rivina excludes intercept", {
  out <- tee_regressiotaulukko_selitettava_rivina(lm_simple)
  muuttuja <- out[[names(out)[names(out) != "Selitettava"][1]]]
  expect_false("(Intercept)" %in% muuttuja)
})

test_that("tee_regressiotaulukko_selitettava_rivina last row is R2 with NA CI and beta", {
  out <- tee_regressiotaulukko_selitettava_rivina(lm_simple, kieli = "suomi")
  last <- out[nrow(out), ]
  expect_equal(last[["Muuttuja"]], "R2, korjattu")
  expect_true(is.na(last[["beta"]]))
  expect_true(is.na(last[["Luottamusvali"]]))
})

test_that("tee_regressiotaulukko_selitettava_rivina beta column is character", {
  out <- tee_regressiotaulukko_selitettava_rivina(lm_simple)
  expect_type(out[["beta"]], "character")
})

test_that("tee_regressiotaulukko_selitettava_rivina English translation works", {
  out <- tee_regressiotaulukko_selitettava_rivina(lm_simple, kieli = "eng")
  expect_true("Outcome" %in% names(out))
  expect_false("Selitettava" %in% names(out))
})

test_that("tee_regressiotaulukko_selitettava_rivina desimaalierotin = 'pilkku' uses commas", {
  out <- tee_regressiotaulukko_selitettava_rivina(lm_simple, desimaalierotin = "pilkku")
  char_cols <- out[, sapply(out, is.character)]
  char_vals <- unlist(char_cols)
  char_vals <- char_vals[!is.na(char_vals)]
  expect_false(any(grepl("\\.", char_vals)))
  # beta specifically must use commas
  beta_vals <- out[["beta"]]
  beta_vals <- beta_vals[!is.na(beta_vals)]
  expect_false(any(grepl("\\.", beta_vals)))
})

test_that("tee_regressiotaulukko_selitettava_rivina desimaalierotin = 'piste' keeps dots (default)", {
  out <- tee_regressiotaulukko_selitettava_rivina(lm_simple)
  b_vals <- out[["B"]]
  b_vals <- b_vals[!is.na(b_vals)]
  expect_false(any(grepl(",", b_vals)))
})
