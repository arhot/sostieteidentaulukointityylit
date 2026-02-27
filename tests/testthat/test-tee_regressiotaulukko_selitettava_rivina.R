test_that("tee_regressiotaulukko_selitettava_rivina stops on non-lm input", {
  expect_error(tee_regressiotaulukko_selitettava_rivina(glm_logit),
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
  # Muuttuja-like column (second non-Selitettava column should be Muuttuja)
  muuttuja_col <- last[[names(last)[2]]]
  expect_equal(muuttuja_col, "R2, korjattu")
  expect_true(is.na(last[["beta"]]))
  expect_true(is.na(last[["Luottamusvali"]]))
})

test_that("tee_regressiotaulukko_selitettava_rivina English translation works", {
  out <- tee_regressiotaulukko_selitettava_rivina(lm_simple, kieli = "eng")
  expect_true("Outcome" %in% names(out))
  expect_false("Selitettava" %in% names(out))
})
