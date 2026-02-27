test_that("tee_log_regressiotaulukko stops on lm input", {
  expect_error(tee_log_regressiotaulukko(lm_simple),
               "logistisen regressioanalyysin")
})

test_that("tee_log_regressiotaulukko stops on non-binomial glm", {
  glm_pois <- glm(abs(round(y)) ~ x2, data = df_synth, family = poisson)
  expect_error(tee_log_regressiotaulukko(glm_pois),
               "logistisen regressioanalyysin")
})

test_that("tee_log_regressiotaulukko stops on non-glm input", {
  expect_error(tee_log_regressiotaulukko("teksti"),
               "logistisen regressioanalyysin")
})

test_that("tee_log_regressiotaulukko returns a tibble", {
  out <- tee_log_regressiotaulukko(glm_logit)
  expect_s3_class(out, "tbl_df")
})

test_that("tee_log_regressiotaulukko excludes intercept", {
  out <- tee_log_regressiotaulukko(glm_logit)
  muuttuja <- out[[1]]
  expect_false("(Intercept)" %in% muuttuja)
})

test_that("tee_log_regressiotaulukko has OR (Vetosuhde) column in Finnish", {
  out <- tee_log_regressiotaulukko(glm_logit, kieli = "suomi")
  expect_true("Vetosuhde" %in% names(out))
})

test_that("tee_log_regressiotaulukko has OR column in English", {
  out <- tee_log_regressiotaulukko(glm_logit, kieli = "eng")
  expect_true("OR" %in% names(out))
  expect_false("Vetosuhde" %in% names(out))
})

test_that("tee_log_regressiotaulukko OR values are positive", {
  out <- tee_log_regressiotaulukko(glm_logit, kieli = "suomi")
  or_vals <- as.numeric(out[["Vetosuhde"]])
  expect_true(all(or_vals > 0, na.rm = TRUE))
})

test_that("tee_log_regressiotaulukko row count equals n predictors", {
  n_preds <- length(coef(glm_logit)) - 1L  # minus intercept
  out <- tee_log_regressiotaulukko(glm_logit)
  expect_equal(nrow(out), n_preds)
})

test_that("tee_log_regressiotaulukko pyorista_est and pyorista_p args accepted", {
  expect_no_error(tee_log_regressiotaulukko(glm_logit, pyorista_est = 3, pyorista_p = 2))
})
